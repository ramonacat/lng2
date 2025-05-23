use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::{FunctionValue, PointerValue},
};

use crate::{
    ast::{self, Class, Expression, Function},
    identifier::{Identifier, Identifiers},
    module::{CompilerServices, ModuleCompiler},
    object::{self, ObjectFunctions, Value},
    types::{
        TypedAst,
        class::ClassType,
        expression::ExpressionType,
        function::{self, FunctionType},
    },
};

pub fn codegen(ast: &TypedAst, identifiers: &Identifiers) {
    let context = Context::create();
    let module_generator = ModuleGenerator::new(&context, identifiers);

    module_generator.generate(ast);
}

struct ClassCompiler<'class> {
    class: &'class Class<ClassType, FunctionType>,
}

impl<'ctx, 'class> ClassCompiler<'class> {
    const fn new(class: &'class Class<ClassType, FunctionType>) -> Self {
        Self { class }
    }

    fn compile_class(
        &mut self,
        // TODO should there be some kinda CompilationContext or whatever that encapsulates all
        // these arguments?
        context: &'ctx Context,
        module: &Module<'ctx>,
        object_functions: &ObjectFunctions<'ctx>,
        class_declarations: &HashMap<Identifier, ClassDeclaration<'ctx, 'class>>,
    ) {
        let class = class_declarations.get(&self.class.name).unwrap();

        for function in &self.class.functions {
            let function_value = class.methods.get(&function.prototype.name).unwrap();

            self.compile_function(
                *function_value,
                function,
                context,
                module,
                object_functions,
                class_declarations,
            );
        }
    }

    fn compile_function(
        &mut self,
        function_value: FunctionValue<'ctx>,
        function: &Function<FunctionType>,
        context: &'ctx Context,
        module: &Module<'ctx>,
        object_functions: &ObjectFunctions<'ctx>,
        class_declarations: &HashMap<Identifier, ClassDeclaration<'ctx, 'class>>,
    ) -> FunctionValue<'ctx> {
        match function.type_.as_kind() {
            function::FunctionTypeKind::Statements(statements) => {
                let builder = context.create_builder();
                let entry_block = context.append_basic_block(function_value, "entry");
                builder.position_at_end(entry_block);

                for statement in statements {
                    match statement {
                        ast::Statement::Expression(expression) => {
                            self.compile_expression(
                                expression,
                                context,
                                &builder,
                                object_functions,
                                class_declarations,
                            );
                        }
                    }
                }

                // TODO we have to return the actual value from a return statement here, and verify
                // that this we always have one (or never have one if the type is void)
                builder.build_return(None).unwrap();

                function_value
            }
            function::FunctionTypeKind::External(external_name) => {
                let external = module.add_function(
                    external_name,
                    // TODO actually set the correct type here
                    context.void_type().fn_type(&[], false),
                    None,
                );
                let trampoline = *class_declarations
                    .get(&self.class.name)
                    .unwrap()
                    .methods
                    .get(&function.prototype.name)
                    .unwrap();
                let builder = context.create_builder();
                let entry_block = context.append_basic_block(trampoline, "entry");
                builder.position_at_end(entry_block);

                builder.build_call(external, &[], "external_call").unwrap();
                builder.build_return(None).unwrap();

                trampoline
            }
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn compile_expression(
        &mut self,
        expression: &Expression<ExpressionType>,
        context: &'ctx Context,
        builder: &Builder<'ctx>,
        object_functions: &ObjectFunctions<'ctx>,
        class_declarations: &HashMap<Identifier, ClassDeclaration<'ctx, 'class>>,
    ) -> Value<'ctx, 'class> {
        match &expression.kind {
            ast::ExpressionKind::Call(expression) => {
                // TODO differentiate between static and non-static methods
                let expression = self.compile_expression(
                    expression,
                    context,
                    builder,
                    object_functions,
                    class_declarations,
                );

                let Value::Field(field) = expression else {
                    todo!();
                };

                object_functions.call_field(field, context, builder);

                Value::None
            }
            ast::ExpressionKind::VariableAccess(identifier) => {
                Value::Class(class_declarations.get(identifier).unwrap().clone())
            }
            ast::ExpressionKind::FieldAccess(target, field) => {
                let Value::Class(class) = self.compile_expression(
                    target,
                    context,
                    builder,
                    object_functions,
                    class_declarations,
                ) else {
                    todo!();
                };

                class.field_access(*field, object_functions, context, builder)
            }
        }
    }
}

struct ClassCompilers<'class>(HashMap<Identifier, ClassCompiler<'class>>);

impl<'class> ClassCompilers<'class> {
    fn new() -> Self {
        Self(HashMap::new())
    }

    fn add(&mut self, class: &'class Class<ClassType, FunctionType>) {
        self.0.insert(class.name, ClassCompiler::new(class));
    }
}

#[derive(Clone)]
pub struct ClassDeclaration<'ctx, 'class> {
    descriptor: PointerValue<'ctx>,
    methods: HashMap<Identifier, FunctionValue<'ctx>>,
    field_indices: HashMap<Identifier, u64>,
    #[allow(unused)]
    class: &'class Class<ClassType, FunctionType>,
}

impl<'ctx, 'class> ClassDeclaration<'ctx, 'class> {
    fn new(
        class: &'class Class<ClassType, FunctionType>,
        context: &'ctx Context,
        module: &Module<'ctx>,
        object_functions: &ObjectFunctions<'ctx>,
        identifiers: &Identifiers,
        compiler_services: &mut CompilerServices<'ctx>,
    ) -> Self {
        let mut field_indices = HashMap::new();

        let methods = class
            .functions
            .iter()
            .map(|x| {
                let resolved_name = identifiers.resolve(x.prototype.name);
                // TODO this is very hacky, we need to add an attribute for the main function, and
                // typecheck that only one exists in the program and that it has the right
                // signature
                let name = if resolved_name == "main" {
                    resolved_name
                } else {
                    // TODO name mangling (tho this should be good enough as long as there are
                    // no generics)
                    &format!("{}_{}", resolved_name, x.type_.id().into_u64())
                };

                (
                    x.prototype.name,
                    module.add_function(name, context.void_type().fn_type(&[], false), None),
                )
            })
            .collect::<HashMap<_, _>>();

        let mut fields = vec![];

        for (index, (name, function)) in methods.iter().enumerate() {
            fields.push(object::Field {
                name: *name,
                value: Value::Callable(*function),
            });
            field_indices.insert(*name, index as u64);
        }

        let descriptor = object_functions.declare_class(
            identifiers.resolve(class.name),
            &fields,
            context,
            module,
            compiler_services,
        );

        Self {
            class,
            descriptor: descriptor.as_pointer_value(),
            field_indices,
            methods,
        }
    }

    fn field_access(
        &self,
        field: Identifier,
        object_functions: &ObjectFunctions<'ctx>,
        context: &'ctx Context,
        builder: &Builder<'ctx>,
    ) -> Value<'ctx, 'class> {
        let index = self.field_indices.get(&field).unwrap();
        let field = object_functions.get_field(
            self.descriptor,
            usize::try_from(*index).unwrap(),
            context,
            builder,
        );

        Value::Field(field)
    }
}

struct ModuleGenerator<'ctx> {
    module_compiler: ModuleCompiler<'ctx>,
    object_functions: ObjectFunctions<'ctx>,
    identifiers: &'ctx Identifiers,
}

impl<'ctx> ModuleGenerator<'ctx> {
    pub fn new(context: &'ctx Context, identifiers: &'ctx Identifiers) -> Self {
        // TODO base the module name off of the name passed in SourceFile
        let module = context.create_module("main");
        let mut module_compiler = ModuleCompiler::new(context, module);
        // TODO these should be in a separate runtime module, and we should just link them in to
        // the modules that need 'em
        let object_functions = crate::object::generate_object_functions(&mut module_compiler);

        Self {
            module_compiler,
            object_functions,
            identifiers,
        }
    }

    // TODO this should really return a CompiledModule that we got from ModuleCompiler
    fn generate(mut self, ast: &TypedAst) {
        self.module_compiler
            .build(|context, module, compiler_services| {
                let mut class_declarations = HashMap::new();

                for declaration in &ast.declarations {
                    match declaration {
                        ast::Declaration::Class(class) => {
                            class_declarations.insert(
                                class.name,
                                ClassDeclaration::new(
                                    class,
                                    context,
                                    module,
                                    &self.object_functions,
                                    self.identifiers,
                                    compiler_services,
                                ),
                            );
                        }
                    }
                }

                let mut classes = ClassCompilers::new();
                let mut id = None;

                for declaration in &ast.declarations {
                    match declaration {
                        ast::Declaration::Class(class) => {
                            id = Some(class.name);
                            classes.add(class);
                        }
                    }
                }

                classes.0.get_mut(&id.unwrap()).unwrap().compile_class(
                    context,
                    module,
                    &self.object_functions,
                    &class_declarations,
                );
            });

        println!("{:?}", &self.module_compiler);
        self.module_compiler.run(|execution_engine| unsafe {
            execution_engine
                .get_function::<unsafe extern "C" fn()>("main")
                .unwrap()
                .call();
        });
    }
}
