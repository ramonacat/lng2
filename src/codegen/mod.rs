use std::collections::HashMap;

use inkwell::{builder::Builder, context::Context, module::Module, values::FunctionValue};

use crate::{
    ast::{self, Class, Expression, Function},
    identifier::{Identifier, Identifiers},
    module::{CompilerServices, ModuleCompiler},
    object::{self, Object, ObjectFunctions, Value},
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

pub trait AnyCompilerContext<'ctx, 'a> {
    fn context(&self) -> &'ctx Context;
    fn module(&self) -> &'a Module<'ctx>;
    fn object_functions(&self) -> &'a ObjectFunctions<'ctx>;
    fn identifiers(&self) -> &'a Identifiers;
}

#[derive(Clone, Copy)]
pub struct FunctionCompilerContext<'ctx, 'a, 'class> {
    pub class: ClassCompilerContext<'ctx, 'a, 'class>,
    pub function_value: FunctionValue<'ctx>,
    pub function: &'a Function<FunctionType>,
}

impl<'ctx, 'a> AnyCompilerContext<'ctx, 'a> for FunctionCompilerContext<'ctx, 'a, '_> {
    fn context(&self) -> &'ctx Context {
        self.class.context()
    }

    fn module(&self) -> &'a Module<'ctx> {
        self.class.module()
    }

    fn object_functions(&self) -> &'a ObjectFunctions<'ctx> {
        self.class.object_functions()
    }

    fn identifiers(&self) -> &'a Identifiers {
        self.class.identifiers()
    }
}

#[derive(Clone, Copy)]
pub struct ClassCompilerContext<'ctx, 'a, 'class> {
    pub compiler: CompilerContext<'ctx, 'a>,
    pub class_declarations: &'a HashMap<Identifier, ClassDeclaration<'ctx, 'class>>,
}

impl<'ctx, 'a> AnyCompilerContext<'ctx, 'a> for ClassCompilerContext<'ctx, 'a, '_> {
    fn context(&self) -> &'ctx Context {
        self.compiler.context
    }

    fn module(&self) -> &'a Module<'ctx> {
        self.compiler.module
    }

    fn object_functions(&self) -> &'a ObjectFunctions<'ctx> {
        self.compiler.object_functions
    }

    fn identifiers(&self) -> &'a Identifiers {
        self.compiler.identifiers
    }
}

#[derive(Clone, Copy)]
pub struct CompilerContext<'ctx, 'a> {
    pub context: &'ctx Context,
    pub module: &'a Module<'ctx>,
    pub object_functions: &'a ObjectFunctions<'ctx>,
    pub identifiers: &'a Identifiers,
}

impl<'ctx, 'a> AnyCompilerContext<'ctx, 'a> for CompilerContext<'ctx, 'a> {
    fn context(&self) -> &'ctx Context {
        self.context
    }

    fn module(&self) -> &'a Module<'ctx> {
        self.module
    }

    fn object_functions(&self) -> &'a ObjectFunctions<'ctx> {
        self.object_functions
    }

    fn identifiers(&self) -> &'a Identifiers {
        self.identifiers
    }
}

impl<'ctx, 'class> ClassCompiler<'class>
where
    'ctx: 'class,
{
    const fn new(class: &'class Class<ClassType, FunctionType>) -> Self {
        Self { class }
    }

    fn compile_class(&mut self, compiler_context: ClassCompilerContext<'ctx, 'class, 'class>) {
        let class = compiler_context
            .class_declarations
            .get(&self.class.name)
            .unwrap();

        for function in &self.class.functions {
            let function_value = class.methods.get(&function.prototype.name).unwrap();

            let function_compiler_context = FunctionCompilerContext {
                class: compiler_context,
                function_value: *function_value,
                function,
            };

            self.compile_function(function_compiler_context);
        }
    }

    fn compile_function(
        &mut self,
        context: FunctionCompilerContext<'ctx, 'class, 'class>,
    ) -> FunctionValue<'ctx>
    where
        'ctx: 'class,
    {
        match context.function.type_.as_kind() {
            function::FunctionTypeKind::Statements(statements) => {
                let builder = context.context().create_builder();
                let entry_block = context
                    .context()
                    .append_basic_block(context.function_value, "entry");
                builder.position_at_end(entry_block);

                for statement in statements {
                    match statement {
                        ast::Statement::Expression(expression) => {
                            self.compile_expression(expression, &builder, context);
                        }
                    }
                }

                // TODO we have to return the actual value from a return statement here, and verify
                // that this we always have one (or never have one if the type is void)
                builder.build_return(None).unwrap();

                context.function_value
            }
            function::FunctionTypeKind::External(external_name) => {
                let external = context.module().add_function(
                    external_name,
                    // TODO actually set the correct type here
                    context.context().void_type().fn_type(&[], false),
                    None,
                );
                let trampoline = *context
                    .class
                    .class_declarations
                    .get(&self.class.name)
                    .unwrap()
                    .methods
                    .get(&context.function.prototype.name)
                    .unwrap();
                let builder = context.context().create_builder();
                let entry_block = context.context().append_basic_block(trampoline, "entry");
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
        builder: &Builder<'ctx>,
        context: FunctionCompilerContext<'ctx, 'class, 'class>,
    ) -> Value<'ctx, 'class> {
        match &expression.kind {
            ast::ExpressionKind::Call(expression) => {
                // TODO differentiate between static and non-static methods
                let expression = self.compile_expression(expression, builder, context);

                let Value::Field(field) = expression else {
                    todo!();
                };

                field.build_call(builder, context);

                Value::None
            }
            ast::ExpressionKind::VariableAccess(identifier) => {
                Value::Class(context.class.class_declarations.get(identifier).unwrap())
            }
            ast::ExpressionKind::FieldAccess(target, field) => {
                let Value::Class(class) = self.compile_expression(target, builder, context) else {
                    todo!();
                };

                class.field_access(*field, builder, context)
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

pub struct ClassDeclaration<'ctx, 'class> {
    descriptor: Object<'ctx>,
    methods: HashMap<Identifier, FunctionValue<'ctx>>,
    field_indices: HashMap<Identifier, u64>,
    #[allow(unused)]
    class: &'class Class<ClassType, FunctionType>,
}

impl<'ctx, 'class> ClassDeclaration<'ctx, 'class> {
    fn new(
        // TODO pass ClassCompilerContext here as an arg instead of all the things?
        class: &'class Class<ClassType, FunctionType>,
        context: CompilerContext<'ctx, '_>,
        compiler_services: &mut CompilerServices<'ctx>,
    ) -> Self {
        let mut field_indices = HashMap::new();

        let methods = class
            .functions
            .iter()
            .map(|x| {
                let resolved_name = context.identifiers().resolve(x.prototype.name);
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
                    context.module().add_function(
                        name,
                        context.context().void_type().fn_type(&[], false),
                        None,
                    ),
                )
            })
            .collect::<HashMap<_, _>>();

        let mut fields = vec![];

        for (index, (name, function)) in methods.iter().enumerate() {
            fields.push(object::FieldDeclaration {
                name: *name,
                value: Value::Callable(*function),
            });
            field_indices.insert(*name, index as u64);
        }

        let descriptor = context.object_functions().declare_class(
            context.identifiers().resolve(class.name),
            &fields,
            context.context(),
            context.module(),
            compiler_services,
        );

        Self {
            descriptor,
            methods,
            field_indices,
            class,
        }
    }

    fn field_access(
        &'class self,
        field: Identifier,
        builder: &Builder<'ctx>,
        compiler_context: FunctionCompilerContext<'ctx, '_, 'class>,
    ) -> Value<'ctx, 'class> {
        let index = self.field_indices.get(&field).unwrap();
        let field =
            self.descriptor
                .get_field(usize::try_from(*index).unwrap(), compiler_context, builder);

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
                let compiler_context = CompilerContext {
                    context,
                    module,
                    object_functions: &self.object_functions,
                    identifiers: self.identifiers,
                };
                let mut class_declarations = HashMap::new();

                for declaration in &ast.declarations {
                    match declaration {
                        ast::Declaration::Class(class) => {
                            class_declarations.insert(
                                class.name,
                                ClassDeclaration::new(class, compiler_context, compiler_services),
                            );
                        }
                    }
                }

                let compiler_context = ClassCompilerContext {
                    compiler: compiler_context,
                    class_declarations: &class_declarations,
                };

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
                classes
                    .0
                    .get_mut(&id.unwrap())
                    .unwrap()
                    .compile_class(compiler_context);
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
