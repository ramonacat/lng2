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

pub fn codegen(ast: TypedAst, identifiers: &Identifiers) {
    let context = Context::create();
    let module_generator = ModuleGenerator::new(&context, identifiers);

    module_generator.generate(ast);
}

struct ClassCompiler {
    class: Class<ClassType, FunctionType>,
}

impl<'ctx> ClassCompiler {
    const fn new(class: Class<ClassType, FunctionType>) -> Self {
        Self { class }
    }

    fn compile_class<'a>(
        &mut self,
        // TODO should there be some kinda CompilationContext or whatever that encapsulates all
        // these arguments?
        context: &'ctx Context,
        module: &Module<'ctx>,
        class_declarations: &HashMap<Identifier, ClassDeclaration<'ctx>>,
    ) where
        'a: 'ctx,
    {
        let class = class_declarations.get(&self.class.name).unwrap();
        let mut functions = vec![];
        std::mem::swap(&mut functions, &mut self.class.functions);

        for function in functions {
            let function_value = class.methods.get(&function.prototype.name).unwrap();
            self.compile_function(
                *function_value,
                function,
                context,
                module,
                class_declarations,
            );
        }
    }

    fn compile_function<'classes>(
        &mut self,
        function_value: FunctionValue<'ctx>,
        function: Function<FunctionType>,
        context: &'ctx Context,
        module: &Module<'ctx>,
        class_declarations: &HashMap<Identifier, ClassDeclaration<'ctx>>,
    ) -> FunctionValue<'ctx>
    where
        'classes: 'ctx,
    {
        match function.type_.into_kind() {
            function::FunctionTypeKind::Statements(statements) => {
                let builder = context.create_builder();
                let entry_block = context.append_basic_block(function_value, "entry");
                builder.position_at_end(entry_block);

                for statement in statements {
                    match statement {
                        ast::Statement::Expression(expression) => {
                            self.compile_expression(expression, &builder, class_declarations);
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
                    &external_name,
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
        expression: Expression<ExpressionType>,
        builder: &Builder<'ctx>,
        class_declarations: &HashMap<Identifier, ClassDeclaration<'ctx>>,
    ) -> Value<'ctx> {
        match expression.kind {
            ast::ExpressionKind::Call(expression) => {
                let expression = self.compile_expression(*expression, builder, class_declarations);

                let Value::Callable(function_value) = expression else {
                    todo!();
                };

                // TODO support signatures other than fn() -> void
                builder
                    .build_direct_call(function_value, &[], "call")
                    .unwrap();

                Value::None
            }
            ast::ExpressionKind::VariableAccess(identifier) => {
                // TODO actually get the class here ;)
                Value::Class(class_declarations.get(&identifier).unwrap().clone())
            }
            ast::ExpressionKind::FieldAccess(target, field) => {
                let Value::Class(class) =
                    self.compile_expression(*target, builder, class_declarations)
                else {
                    todo!();
                };

                // TODO This should in reality codegen code that takes the declaration and looks
                // for the specific field, and on this side of the JIT we shouldn't really know or
                // care what the field values are
                let function = class.methods.get(&field).unwrap();

                Value::Callable(*function)
            }
        }
    }
}

struct ClassCompilers(HashMap<Identifier, ClassCompiler>);

impl ClassCompilers {
    fn new() -> Self {
        Self(HashMap::new())
    }

    fn add(&mut self, class: Class<ClassType, FunctionType>) {
        self.0.insert(class.name, ClassCompiler::new(class));
    }
}

#[derive(Clone)]
pub struct ClassDeclaration<'ctx> {
    #[allow(unused)]
    descriptor: PointerValue<'ctx>,
    methods: HashMap<Identifier, FunctionValue<'ctx>>,
}
impl<'ctx> ClassDeclaration<'ctx> {
    fn new(
        class: &Class<ClassType, FunctionType>,
        context: &'ctx Context,
        module: &Module<'ctx>,
        object_functions: &ObjectFunctions<'ctx>,
        identifiers: &Identifiers,
        compiler_services: &mut CompilerServices<'ctx>,
    ) -> Self {
        let methods = class
            .functions
            .iter()
            .map(|x| {
                let resolved_name = identifiers.resolve(x.prototype.name);

                (
                    x.prototype.name,
                    module.add_function(
                        // TODO name mangling
                        resolved_name,
                        context.void_type().fn_type(&[], false),
                        None,
                    ),
                )
            })
            .collect::<HashMap<_, _>>();

        let fields = methods
            .iter()
            .map(|x| object::Field {
                name: *x.0,
                value: Value::Callable(*x.1),
            })
            .collect::<Vec<_>>();

        let descriptor = object_functions.declare_class(
            identifiers.resolve(class.name),
            &fields,
            context,
            module,
            compiler_services,
        );

        Self {
            descriptor: descriptor.as_pointer_value(),
            methods,
        }
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
    fn generate(mut self, ast: TypedAst) {
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

                for declaration in ast.declarations {
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
