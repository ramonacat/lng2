use std::collections::HashMap;

use inkwell::{builder::Builder, context::Context, module::Module, values::FunctionValue};

use crate::{
    ast::{self, Class, Expression, Function},
    identifier::{Identifier, Identifiers},
    module::{self, CompilerServices, ModuleCompiler},
    object::{self, ClassValue, ObjectFunctions, Value},
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
    fn new(class: Class<ClassType, FunctionType>) -> Self {
        Self { class }
    }

    fn compile_class<'a>(
        &mut self,
        // TODO should there be some kinda CompilationContext or whatever that encapsulates all
        // these arguments?
        context: &'ctx Context,
        module: &Module<'ctx>,
        compiler_services: &mut module::CompilerServices<'ctx>,
        identifiers: &Identifiers,
        object_functions: &ObjectFunctions<'ctx>,
    ) -> CompiledClass<'ctx>
    where
        'a: 'ctx,
    {
        let mut functions = vec![];
        std::mem::swap(&mut functions, &mut self.class.functions);

        let fields = functions
            .into_iter()
            .map(|x| object::Field {
                name: x.prototype.name,
                value: object::Value::Callable(self.compile_function(
                    x,
                    context,
                    module,
                    compiler_services,
                    identifiers,
                    object_functions,
                )),
            })
            .collect::<Vec<_>>();

        object_functions.declare_class(
            &identifiers.resolve(self.class.name),
            &fields,
            context,
            module,
            compiler_services,
        );

        CompiledClass {
            fields: fields.into_iter().map(|x| (x.name, x.value)).collect(),
        }
    }

    fn compile_function<'classes>(
        &mut self,
        function: Function<FunctionType>,
        context: &'ctx Context,
        module: &Module<'ctx>,
        compiler_services: &mut module::CompilerServices<'ctx>,
        identifiers: &Identifiers,
        object_functions: &ObjectFunctions<'ctx>,
    ) -> FunctionValue<'ctx>
    where
        'classes: 'ctx,
    {
        match function.type_.into_kind() {
            function::FunctionTypeKind::Statements(statements) => {
                let function = module.add_function(
                    identifiers.resolve(function.prototype.name),
                    context.void_type().fn_type(&[], false),
                    None,
                );
                let builder = context.create_builder();
                let entry_block = context.append_basic_block(function, "entry");
                builder.position_at_end(entry_block);

                for statement in statements {
                    match statement {
                        ast::Statement::Expression(expression) => {
                            self.compile_expression(
                                expression,
                                context,
                                &builder,
                                module,
                                compiler_services,
                                identifiers,
                                object_functions,
                            );
                        }
                    }
                }

                function
            }
            function::FunctionTypeKind::External(external_name) => {
                module.add_function(
                    &external_name,
                    // TODO actually set the correct type here
                    context.void_type().fn_type(&[], false),
                    None,
                )
            }
        }
    }

    fn compile_expression(
        &mut self,
        expression: Expression<ExpressionType>,
        context: &'ctx Context,
        builder: &Builder<'ctx>,
        module: &Module<'ctx>,
        compiler_services: &mut module::CompilerServices<'ctx>,
        identifiers: &Identifiers,
        object_functions: &ObjectFunctions<'ctx>,
    ) -> Value<'ctx> {
        match expression.kind {
            ast::ExpressionKind::Call(expression) => {
                let expression = self.compile_expression(
                    *expression,
                    context,
                    builder,
                    module,
                    compiler_services,
                    identifiers,
                    object_functions,
                );

                let Value::Callable(function_value) = expression else {
                    todo!();
                };

                // TODO support signatures other than fn() -> void
                builder
                    .build_direct_call(function_value, &[], "call")
                    .unwrap();

                Value::None
            }
            ast::ExpressionKind::VariableAccess(_) => {
                // TODO actually get the class here ;)
                Value::Class(ClassValue::new(&CompiledClass { fields: HashMap::new() }))
            }
            ast::ExpressionKind::FieldAccess(target, field) => {
                let Value::Class(class) = self.compile_expression(
                    *target,
                    context,
                    builder,
                    module,
                    compiler_services,
                    identifiers,
                    object_functions,
                ) else {
                    todo!();
                };

                let Value::Callable(function) = class.field(field) else {
                    todo!();
                };

                Value::Callable(*function)
            }
        }
    }
}

#[derive(Clone)]
pub struct CompiledClass<'ctx> {
    pub fields: HashMap<Identifier, Value<'ctx>>,
}

struct ClassCompilers(HashMap<Identifier, ClassCompiler>);

impl<'ctx> ClassCompilers {
    fn new() -> Self {
        Self(HashMap::new())
    }

    fn add(&mut self, class: Class<ClassType, FunctionType>) {
        self.0.insert(class.name, ClassCompiler::new(class));
    }

    fn get(
        &mut self,
        identifier: Identifier,
        context: &'ctx Context,
        module: &Module<'ctx>,
        compiler_services: &mut CompilerServices<'ctx>,
        identifiers: &Identifiers,
        object_functions: &ObjectFunctions<'ctx>,
    ) -> CompiledClass<'ctx> {
        // TODO do we need to consider the case where there's just nothing to get? Or is it up to
        // typecheck?
        let compiler = self.0.get_mut(&identifier).unwrap();

        compiler.compile_class(
            context,
            module,
            compiler_services,
            identifiers,
            object_functions,
        )
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
        let mut module_compiler = ModuleCompiler::new(&context, module);
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
                let mut classes = ClassCompilers::new();
                let mut id = None;
                for declaration in ast.declarations {
                    match declaration {
                        ast::Declaration::Class(class) => {
                            id = Some(class.name);
                            classes.add(class)
                        }
                    }
                }
                let _ = classes.get(
                    id.unwrap(),
                    context,
                    module,
                    compiler_services,
                    &self.identifiers,
                    &mut self.object_functions,
                );
            });

        println!("{:?}", &self.module_compiler);
        self.module_compiler.run(|_execution_engine| {
            // TODO get the main here
        });
    }
}
