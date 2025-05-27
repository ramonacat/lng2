mod context;
mod helpers;
mod object;
mod scope;
mod stored_value;

use std::collections::HashMap;

use helpers::CodegenHelpers;
use inkwell::{
    builder::Builder, context::Context, module::Linkage, types::BasicType, values::FunctionValue,
};
use object::{Object, ObjectFunctions, field::FieldDeclaration};
use scope::Scope;

use crate::{
    ADDRESS_SPACE,
    ast::{self, Class, Expression},
    codegen::{
        context::{
            AnyCompilerContext, ClassCompilerContext, CompilerContext, FunctionCompilerContext,
        },
        stored_value::{Storage, StoredValue, ValueType},
    },
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
            crate::types::expression::ExpressionTypeKind::String => context
                .context()
                .ptr_type(*ADDRESS_SPACE)
                .as_basic_type_enum()
                .into(),
            crate::types::expression::ExpressionTypeKind::Class(_) => todo!(),
            crate::types::expression::ExpressionTypeKind::Unit => todo!(),
            crate::types::expression::ExpressionTypeKind::Callable { .. } => todo!(),
        })
        .collect();

    match &prototype.return_type.kind() {
        crate::types::expression::ExpressionTypeKind::Unit => {
            context.context().void_type().fn_type(&arguments[..], false)
        }
        crate::types::expression::ExpressionTypeKind::String => context
            .context()
            .ptr_type(*ADDRESS_SPACE)
            .fn_type(&arguments[..], false),
        crate::types::expression::ExpressionTypeKind::Class(_) => todo!(),
        crate::types::expression::ExpressionTypeKind::Callable { .. } => todo!(),
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

                            if let Some(value) = value {
                                value.build_return(&builder, &context);
                            } else {
                                builder.build_return(None).unwrap();
                            }

                            return_built = true;
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
    ) -> Option<StoredValue<'ctx, 'class>> {
        match &expression.kind {
            ast::ExpressionKind::Call(expression, arguments) => {
                // TODO differentiate between static and non-static methods
                let built_expression = self
                    .compile_expression(expression, scope, builder, context)
                    .unwrap();
                let arguments: Vec<_> = arguments
                    .iter()
                    .map(|x| self.compile_expression(x, scope, builder, context).unwrap())
                    .collect();

                built_expression.build_call(arguments, builder, &context)
            }
            ast::ExpressionKind::VariableAccess(identifier) => scope.get(*identifier).copied(),
            ast::ExpressionKind::FieldAccess(target, field) => {
                let value = self
                    .compile_expression(target, scope, builder, context)
                    .unwrap();

                value.field_access(*field, builder, context)
            }
            ast::ExpressionKind::Literal(literal) => match literal {
                ast::Literal::String(value) => {
                    let global_string = builder.build_global_string_ptr(value, "literal").unwrap();

                    Some(StoredValue::new(
                        Storage::Global(global_string),
                        ValueType::String,
                    ))
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
pub struct ClassDeclaration<'ctx> {
    descriptor: Object<'ctx>,
    field_indices: HashMap<Identifier, u64>,
}

impl<'ctx, 'class> ClassDeclaration<'ctx> {
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
            fields.push(FieldDeclaration {
                name: function_ast.prototype.name,
                value: StoredValue::new(
                    Storage::Global(function.as_global_value()),
                    ValueType::Callable(function.get_type()),
                ),
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
        }
    }

    fn field_access(
        &'class self,
        field: Identifier,
        builder: &Builder<'ctx>,
        compiler_context: FunctionCompilerContext<'ctx, '_, 'class>,
    ) -> StoredValue<'ctx, 'class> {
        let index = self.field_indices.get(&field).unwrap();
        let field_value =
            self.descriptor
                .get_field(usize::try_from(*index).unwrap(), compiler_context, builder);

        // TODO this will break when we have more than string & callable
        let value_type = if field_value.type_.is_pointer_type() {
            ValueType::String
        } else {
            ValueType::Callable(field_value.type_.into_function_type())
        };
        StoredValue::new(Storage::Field(field_value), value_type)
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
                    context
                        .void_type()
                        .fn_type(&[context.i64_type().into()], false),
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

                            class_declarations
                                .insert(class.type_.id(), (class.name, class_declaration));
                        }
                    }
                }

                let mut scope: Scope<'ctx, '_> = Scope::new();
                for (name, declaration) in class_declarations.values() {
                    scope.set(
                        *name,
                        StoredValue::new(Storage::Builtin, ValueType::Class(declaration)),
                    );
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
