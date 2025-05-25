mod object;
mod scope;

use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::BasicType,
    values::FunctionValue,
};
use object::{Object, ObjectFunctions, Value};
use scope::Scope;

use crate::{
    ADDRESS_SPACE,
    ast::{self, Class, Expression, Function},
    identifier::{Identifier, Identifiers},
    module::{CompilerServices, ModuleCompiler},
    types::{
        TypedAst,
        class::ClassType,
        expression::ExpressionType,
        function::{self, FunctionId, FunctionType},
    },
};

pub fn make_fn_type<'ctx>(
    prototype: &ast::FunctionPrototype<ExpressionType>,
    context: CompilerContext<'ctx, '_>,
) -> inkwell::types::FunctionType<'ctx> {
    let arguments: Vec<_> = prototype
        .arguments
        .iter()
        .map(|x| match x.type_.kind() {
            crate::types::expression::ExpressionTypeKind::Todo => todo!(),
            crate::types::expression::ExpressionTypeKind::String => context
                .context()
                .ptr_type(*ADDRESS_SPACE)
                .as_basic_type_enum()
                .into(),
        })
        .collect();

    match &prototype.return_type.kind() {
        crate::types::expression::ExpressionTypeKind::Todo => {
            context.context().void_type().fn_type(&arguments[..], false)
        }
        crate::types::expression::ExpressionTypeKind::String => context
            .context()
            .ptr_type(*ADDRESS_SPACE)
            .fn_type(&arguments[..], false),
    }
}

pub fn codegen(ast: &TypedAst, identifiers: &Identifiers) {
    let context = Context::create();
    let module_generator = ModuleGenerator::new(&context, identifiers);

    module_generator.generate(ast);
}

struct ClassCompiler<'class> {
    class: &'class Class<ClassType, FunctionType, ExpressionType>,
}

pub trait AnyCompilerContext<'ctx, 'a> {
    fn context(&self) -> &'ctx Context;
    fn module(&self) -> &'a Module<'ctx>;
    fn object_functions(&self) -> &'a ObjectFunctions<'ctx>;
    fn identifiers(&self) -> &'a Identifiers;
    fn fatal_error(&self) -> FunctionValue<'ctx>;
}

#[derive(Clone, Copy)]
pub struct FunctionCompilerContext<'ctx, 'a, 'class> {
    pub class: ClassCompilerContext<'ctx, 'a, 'class>,
    pub function_value: FunctionValue<'ctx>,
    pub function: &'a Function<FunctionType, ExpressionType>,
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

    fn fatal_error(&self) -> FunctionValue<'ctx> {
        self.class.fatal_error()
    }
}

#[derive(Clone, Copy)]
pub struct ClassCompilerContext<'ctx, 'a, 'class> {
    pub compiler: CompilerContext<'ctx, 'a>,
    pub function_declarations: &'a HashMap<FunctionId, FunctionValue<'ctx>>,
    pub class: &'class Class<ClassType, FunctionType, ExpressionType>,
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

    fn fatal_error(&self) -> FunctionValue<'ctx> {
        self.compiler.fatal_error
    }
}

#[derive(Clone, Copy)]
pub struct CompilerContext<'ctx, 'a> {
    pub context: &'ctx Context,
    pub module: &'a Module<'ctx>,
    pub object_functions: &'a ObjectFunctions<'ctx>,
    pub identifiers: &'a Identifiers,
    pub fatal_error: FunctionValue<'ctx>,
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

    fn fatal_error(&self) -> FunctionValue<'ctx> {
        self.fatal_error
    }
}

impl<'ctx, 'class> ClassCompiler<'class>
where
    'ctx: 'class,
{
    const fn new(class: &'class Class<ClassType, FunctionType, ExpressionType>) -> Self {
        Self { class }
    }

    fn compile_class(
        &mut self,
        compiler_context: CompilerContext<'ctx, 'class>,
        mut scope: Scope<'ctx, 'class>,
        function_declarations: &'class HashMap<FunctionId, FunctionValue<'ctx>>,
    ) {
        let compiler_context = ClassCompilerContext {
            compiler: compiler_context,
            function_declarations,
            class: self.class,
        };

        for function in &self.class.functions {
            let function_value = compiler_context
                .function_declarations
                .get(&function.type_.id())
                .unwrap();

            let function_compiler_context = FunctionCompilerContext {
                class: compiler_context,
                function_value: *function_value,
                function,
            };

            scope = self
                .compile_function(scope.into_child(), function_compiler_context)
                .1;
        }
    }

    fn compile_function(
        &mut self,
        scope: Scope<'ctx, 'class>,
        context: FunctionCompilerContext<'ctx, 'class, 'class>,
    ) -> (FunctionValue<'ctx>, Scope<'ctx, 'class>)
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

                let mut return_built = false;

                for statement in statements {
                    match statement {
                        ast::Statement::Expression(expression) => {
                            self.compile_expression(expression, &scope, &builder, context);
                        }
                        ast::Statement::Return(expression) => {
                            let value =
                                self.compile_expression(expression, &scope, &builder, context);

                            // TODO verify that all code paths return compatible types
                            match value {
                                Value::None => todo!(),
                                Value::Callable(_) => todo!(),
                                Value::Field(_) => todo!(),
                                Value::Class(_) => todo!(),
                                Value::String(string) => {
                                    builder.build_return(Some(&string)).unwrap();
                                    return_built = true;
                                }
                                Value::IndirectCallable(_, _) => todo!(),
                            }
                        }
                    }
                }

                // TODO we have to do some real control flow analysis, this will break as soon as
                // the language supports if statements
                if !return_built {
                    builder.build_return(None).unwrap();
                }

                (context.function_value, scope.into_parent().unwrap())
            }
            function::FunctionTypeKind::External(external_name) => {
                let external = context.module().add_function(
                    external_name,
                    make_fn_type(&context.function.prototype, context.class.compiler),
                    None,
                );
                let trampoline = context.function_value;
                trampoline.set_linkage(Linkage::Internal);

                let builder = context.context().create_builder();
                let entry_block = context.context().append_basic_block(trampoline, "entry");
                builder.position_at_end(entry_block);

                let params = trampoline
                    .get_params()
                    .into_iter()
                    .map(Into::into)
                    .collect::<Vec<_>>();
                builder
                    .build_call(external, &params, "external_call")
                    .unwrap();
                builder.build_return(None).unwrap();

                (trampoline, scope.into_parent().unwrap())
            }
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn compile_expression(
        &mut self,
        expression: &Expression<ExpressionType>,
        scope: &Scope<'ctx, 'class>,
        builder: &Builder<'ctx>,
        context: FunctionCompilerContext<'ctx, 'class, 'class>,
    ) -> Value<'ctx, 'class> {
        match &expression.kind {
            ast::ExpressionKind::Call(expression, arguments) => {
                // TODO differentiate between static and non-static methods
                let built_expression = self.compile_expression(expression, scope, builder, context);
                let arguments: Vec<_> = arguments
                    .iter()
                    .map(|x| self.compile_expression(x, scope, builder, context))
                    .collect();

                let Value::Field(field) = built_expression else {
                    todo!();
                };

                field.build_call(arguments, builder, context)
            }
            ast::ExpressionKind::VariableAccess(identifier) => *scope.get(*identifier).unwrap(),
            ast::ExpressionKind::FieldAccess(target, field) => {
                let Value::Class(class) = self.compile_expression(target, scope, builder, context)
                else {
                    todo!();
                };

                class.field_access(*field, builder, context)
            }
            ast::ExpressionKind::Literal(literal) => match literal {
                ast::Literal::String(value) => {
                    let global_string = builder.build_global_string_ptr(value, "literal").unwrap();

                    Value::String(global_string.as_pointer_value())
                }
            },
        }
    }
}

struct ClassCompilers<'class>(HashMap<Identifier, ClassCompiler<'class>>);

impl<'class> ClassCompilers<'class> {
    fn new() -> Self {
        Self(HashMap::new())
    }

    fn add(&mut self, class: &'class Class<ClassType, FunctionType, ExpressionType>) {
        self.0.insert(class.name, ClassCompiler::new(class));
    }
}

#[derive(Debug)]
pub struct ClassDeclaration<'ctx, 'class> {
    descriptor: Object<'ctx>,
    field_indices: HashMap<Identifier, u64>,
    #[allow(unused)]
    class: &'class Class<ClassType, FunctionType, ExpressionType>,
}

impl<'ctx, 'class> ClassDeclaration<'ctx, 'class> {
    fn new(
        // TODO pass ClassCompilerContext here as an arg instead of all the things?
        class: &'class Class<ClassType, FunctionType, ExpressionType>,
        declared_functions: &mut HashMap<FunctionId, FunctionValue<'ctx>>,
        context: CompilerContext<'ctx, '_>,
        compiler_services: &mut CompilerServices<'ctx>,
    ) -> Self {
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
                    x,
                    context
                        .module()
                        .add_function(name, make_fn_type(&x.prototype, context), None),
                )
            })
            .collect::<Vec<_>>();

        let mut fields = vec![];
        let mut field_indices = HashMap::new();

        for (index, (function_ast, function)) in methods.iter().enumerate() {
            fields.push(object::FieldDeclaration {
                name: function_ast.prototype.name,
                value: Value::Callable(*function),
            });
            field_indices.insert(function_ast.prototype.name, index as u64);
            declared_functions.insert(function_ast.type_.id(), *function);
        }

        let descriptor = context.object_functions().declare_class(
            context.identifiers().resolve(class.name),
            &fields,
            context,
            compiler_services,
        );

        Self {
            descriptor,
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
        let object_functions = object::generate_object_functions(&mut module_compiler);

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
                let fatal_error = module.add_function(
                    "fatal_error",
                    context.void_type().fn_type(&[], false),
                    None,
                );

                let compiler_context = CompilerContext {
                    context,
                    module,
                    object_functions: &self.object_functions,
                    identifiers: self.identifiers,
                    fatal_error,
                };
                let mut class_declarations = HashMap::new();
                let mut function_declarations = HashMap::new();

                for declaration in &ast.declarations {
                    match declaration {
                        ast::Declaration::Class(class) => {
                            let class_declaration = ClassDeclaration::new(
                                class,
                                &mut function_declarations,
                                compiler_context,
                                compiler_services,
                            );

                            class_declarations.insert(class.type_.id(), class_declaration);
                        }
                    }
                }

                let mut scope: Scope<'ctx, '_> = Scope::new();
                for class in class_declarations.values() {
                    scope.set(class.class.name, Value::Class(class));
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
                    compiler_context,
                    scope,
                    &function_declarations,
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
