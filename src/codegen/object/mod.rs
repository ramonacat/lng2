mod representation;

use crate::codegen::{CompilerContext, make_fn_type};
use inkwell::types::AnyType;
use inkwell::types::AnyTypeEnum;
use inkwell::types::FunctionType;
use inkwell::values::BasicValueEnum;
use inkwell::values::GlobalValue;
use inkwell::values::PointerValue;
use inkwell::{builder::Builder, types::BasicType, values::BasicValue};
use representation::ObjectFieldKind;

use crate::{
    ADDRESS_SPACE,
    codegen::{AnyCompilerContext as _, ClassDeclaration, FunctionCompilerContext},
    identifier::Identifier,
    module::{CompilerServices, ModuleCompiler},
};

#[derive(Debug, Clone, Copy)]
pub enum Storage<'ctx, 'class> {
    Field(Field<'ctx, 'class>),
    Heap(PointerValue<'ctx>),
    Global(GlobalValue<'ctx>),
    Builtin,
}

#[derive(Debug, Clone, Copy)]
pub enum ValueType<'ctx, 'class> {
    String,
    Class(&'class ClassDeclaration<'ctx>),
    Callable(FunctionType<'ctx>),
}

#[derive(Debug, Clone, Copy)]
pub struct StoredValue<'ctx, 'class> {
    storage: Storage<'ctx, 'class>,
    type_: ValueType<'ctx, 'class>,
}

impl<'ctx, 'class> StoredValue<'ctx, 'class> {
    pub fn into_basic_value_enum(self) -> BasicValueEnum<'ctx> {
        match self.storage {
            Storage::Field(_) => todo!(),
            Storage::Heap(pointer_value) => pointer_value.as_basic_value_enum(),
            Storage::Global(global) => global.as_basic_value_enum(),
            Storage::Builtin => todo!(),
        }
    }

    pub(crate) fn build_call(
        &self,
        arguments: Vec<Self>,
        builder: &Builder<'ctx>,
        context: FunctionCompilerContext<'ctx, 'class, 'class>,
    ) -> Option<Self> {
        match self.storage {
            Storage::Field(field) => field.build_call(arguments, builder, context),
            Storage::Heap(_) => todo!(),
            Storage::Global(_) => todo!(),
            Storage::Builtin => todo!(),
        }
    }

    pub(crate) fn field_access(
        &self,
        field: Identifier,
        builder: &Builder<'ctx>,
        context: FunctionCompilerContext<'ctx, 'class, 'class>,
    ) -> Option<Self> {
        match &self.storage {
            Storage::Field(_) => todo!(),
            Storage::Heap(_) => todo!(),
            Storage::Global(_) => todo!(),
            Storage::Builtin => match &self.type_ {
                ValueType::String => todo!(),
                ValueType::Class(class_declaration) => {
                    Some(class_declaration.field_access(field, builder, context))
                }
                ValueType::Callable(_) => todo!(),
            },
        }
    }

    pub(crate) const fn new(
        storage: Storage<'ctx, 'class>,
        type_: ValueType<'ctx, 'class>,
    ) -> Self {
        Self { storage, type_ }
    }
}

#[derive(Debug)]
pub struct FieldDeclaration<'ctx, 'class> {
    pub name: Identifier,
    pub value: StoredValue<'ctx, 'class>,
}

#[derive(Debug)]
pub struct Object<'ctx> {
    pub self_: PointerValue<'ctx>,
}

impl<'ctx> Object<'ctx> {
    pub(crate) fn get_field(
        &self,
        field_index: usize,
        context: FunctionCompilerContext<'ctx, '_, '_>,
        builder: &Builder<'ctx>,
    ) -> Field<'ctx, '_> {
        // TODO verify that the field_index is in bounds
        let gep = builder
            .build_struct_gep(
                context.object_functions().object_type,
                self.self_,
                0,
                "fields",
            )
            .unwrap();

        let field = unsafe {
            builder
                .build_gep(
                    context.object_functions().fields_type,
                    gep,
                    &[context
                        .context()
                        .i64_type()
                        .const_int(field_index as u64, false)],
                    "field",
                )
                .unwrap()
        };

        Field::new(
            self,
            field,
            make_fn_type(
                &context.class.class.functions[field_index].prototype,
                context.class.compiler,
            )
            .as_any_type_enum(),
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Field<'ctx, 'a> {
    #[allow(unused)]
    pub self_: &'a Object<'ctx>,
    pub value_pointer: PointerValue<'ctx>,
    pub type_: AnyTypeEnum<'ctx>,
}

impl<'ctx, 'a> Field<'ctx, 'a> {
    pub const fn new(
        self_: &'a Object<'ctx>,
        field: PointerValue<'ctx>,
        type_: AnyTypeEnum<'ctx>,
    ) -> Self {
        Self {
            self_,
            value_pointer: field,
            type_,
        }
    }

    fn build_read_value(
        &self,
        builder: &Builder<'ctx>,
        context: FunctionCompilerContext<'ctx, '_, '_>,
    ) -> StoredValue<'ctx, '_> {
        // TODO instead of asserting and erroring out on non-callables, we should instead just
        // return a matching Value every time
        let value_kind_pointer = builder
            .build_struct_gep(
                context.object_functions().fields_type,
                self.value_pointer,
                2,
                "value_kind_pointer",
            )
            .unwrap();

        let value_kind = builder
            .build_load(
                context.context().i8_type(),
                value_kind_pointer,
                "value_kind",
            )
            .unwrap();

        let is_kind_not_callable = builder
            .build_int_compare(
                inkwell::IntPredicate::NE,
                value_kind.into_int_value(),
                context
                    .context()
                    .i8_type()
                    .const_int(ObjectFieldKind::Callable as u64, false),
                "is_kind_not_callable",
            )
            .unwrap();

        let then_block = context
            .context()
            .append_basic_block(context.function_value, "then");
        let else_block = context
            .context()
            .append_basic_block(context.function_value, "else");

        builder
            .build_conditional_branch(is_kind_not_callable, then_block, else_block)
            .unwrap();
        builder.position_at_end(then_block);
        builder
            .build_direct_call(context.fatal_error(), &[], "fatal_error")
            .unwrap();
        builder.build_unreachable().unwrap();

        builder.position_at_end(else_block);
        let value_pointer = builder
            .build_struct_gep(
                context.object_functions().fields_type,
                self.value_pointer,
                1,
                "value_pointer",
            )
            .unwrap();

        let value = builder
            .build_load(
                context.context().ptr_type(*ADDRESS_SPACE),
                value_pointer,
                &format!(
                    "deref_function_{}__{}",
                    context.identifiers().resolve(context.class.class.name),
                    context
                        .identifiers()
                        .resolve(context.function.prototype.name)
                ),
            )
            .unwrap();

        StoredValue {
            storage: Storage::Heap(value.into_pointer_value()),
            type_: ValueType::Callable(self.type_.into_function_type()),
        }
    }

    pub(crate) fn build_call<'class>(
        &self,
        arguments: Vec<StoredValue<'ctx, 'class>>,
        builder: &Builder<'ctx>,
        context: FunctionCompilerContext<'ctx, 'class, '_>,
    ) -> Option<StoredValue<'ctx, 'class>> {
        // TODO ensure we have the correct function signature here
        // TODO handle the return value
        let function_pointer = self.build_read_value(builder, context);
        let arguments = arguments
            .into_iter()
            .map(|x| x.into_basic_value_enum().into())
            .collect::<Vec<_>>();

        let (function_type, function_pointer) = match &function_pointer.type_ {
            ValueType::String => todo!(),
            ValueType::Class(_) => todo!(),
            ValueType::Callable(function_type) => (
                *function_type,
                function_pointer
                    .into_basic_value_enum()
                    .into_pointer_value(),
            ),
        };

        let result = builder
            .build_indirect_call(function_type, function_pointer, &arguments, "call_result")
            .unwrap();

        // TODO this will awfully break once there are any types beside string & unit
        if self.type_.into_function_type().get_return_type().is_some() {
            Some(StoredValue {
                storage: Storage::Heap(
                    result
                        .try_as_basic_value()
                        .unwrap_left()
                        .into_pointer_value(),
                ),
                type_: ValueType::String,
            })
        } else {
            None
        }
    }
}

pub struct ObjectFunctions<'ctx> {
    object_type: inkwell::types::StructType<'ctx>,
    fields_type: inkwell::types::StructType<'ctx>,
}

impl<'ctx> ObjectFunctions<'ctx> {
    fn store_field(
        &self,
        array_index: usize,
        field_index: usize,
        value: BasicValueEnum<'ctx>,
        fields_field: PointerValue<'ctx>,
        builder: &Builder<'ctx>,
        compiler_context: CompilerContext<'ctx, '_>,
    ) {
        let field_ptr = unsafe {
            fields_field.const_gep(
                self.fields_type,
                &[
                    compiler_context
                        .context
                        .i64_type()
                        .const_int(array_index as u64, false),
                    compiler_context
                        .context
                        .i64_type()
                        .const_int(field_index as u64, false),
                ],
            )
        };

        builder.build_store(field_ptr, value).unwrap();
    }

    pub(crate) fn declare_class(
        &self,
        name: &str,
        fields: &[FieldDeclaration<'ctx, '_>],
        compiler_context: CompilerContext<'ctx, '_>,
        compiler_services: &mut CompilerServices<'ctx>,
    ) -> Object<'ctx> {
        let global =
            compiler_context
                .module
                .add_global(self.object_type, Some(*ADDRESS_SPACE), name);

        global.set_initializer(
            &self.object_type.const_named_struct(&[
                compiler_context
                    .context
                    .ptr_type(*ADDRESS_SPACE)
                    .const_null()
                    .as_basic_value_enum(),
                compiler_context
                    .context
                    .i64_type()
                    .const_int(0, false)
                    .as_basic_value_enum(),
                compiler_context
                    .context
                    .ptr_type(*ADDRESS_SPACE)
                    .const_null()
                    .as_basic_value_enum(),
                compiler_context
                    .context
                    .i64_type()
                    .const_int(0, false)
                    .as_basic_value_enum(),
                compiler_context
                    .context
                    .ptr_type(*ADDRESS_SPACE)
                    .const_null()
                    .as_basic_value_enum(),
            ]),
        );

        let constructor = compiler_context.module.add_function(
            &format!("class_constructor_{name}"),
            compiler_context.context.void_type().fn_type(&[], false),
            None,
        );
        let basic_block = compiler_context
            .context
            .append_basic_block(constructor, "entry");
        let builder = compiler_context.context.create_builder();
        builder.position_at_end(basic_block);

        let fields_field = builder
            .build_struct_gep(self.object_type, global.as_pointer_value(), 0, "fields_ptr")
            .unwrap();
        let fields_len = u32::try_from(fields.len()).unwrap();
        let fields_value = compiler_context.module.add_global(
            self.fields_type.array_type(fields_len),
            None,
            "fields",
        );
        fields_value.set_initializer(&self.fields_type.array_type(fields_len).const_zero());

        self.fill_fields(fields, compiler_context, &builder, fields_field);

        builder.build_store(fields_field, fields_value).unwrap();
        builder.build_return(None).unwrap();

        compiler_services.add_global_construtor(
            128,
            constructor.as_global_value().as_pointer_value(),
            Some(global.as_pointer_value()),
        );

        let self_ = global.as_pointer_value();

        Object { self_ }
    }

    fn fill_fields(
        &self,
        fields: &[FieldDeclaration<'ctx, '_>],
        compiler_context: CompilerContext<'ctx, '_>,
        builder: &Builder<'ctx>,
        fields_field: PointerValue<'ctx>,
    ) {
        for (index, field) in fields.iter().enumerate() {
            let (value, value_kind) = match &field.value.type_ {
                ValueType::String => todo!(),
                ValueType::Class(_) => todo!(),
                ValueType::Callable(_) => (
                    field.value.into_basic_value_enum(),
                    ObjectFieldKind::Callable,
                ),
            };

            self.store_field(
                index,
                0,
                compiler_context
                    .context
                    .i64_type()
                    .const_int(field.name.into_id() as u64, false)
                    .as_basic_value_enum(),
                fields_field,
                builder,
                compiler_context,
            );

            self.store_field(index, 1, value, fields_field, builder, compiler_context);

            self.store_field(
                index,
                2,
                compiler_context
                    .context
                    .i8_type()
                    .const_int(value_kind as u64, false)
                    .as_basic_value_enum(),
                fields_field,
                builder,
                compiler_context,
            );
        }
    }
}

pub fn generate_object_functions<'ctx>(
    module_compiler: &mut ModuleCompiler<'ctx>,
) -> ObjectFunctions<'ctx> {
    module_compiler.build(|context, _module, _compiler_services| {
        let object_type = context.opaque_struct_type("object_type");
        object_type.set_body(
            &[
                // fields:
                context.ptr_type(*ADDRESS_SPACE).as_basic_type_enum(),
                // field_count:
                context.i64_type().as_basic_type_enum(),
                // interfaces:
                context.ptr_type(*ADDRESS_SPACE).as_basic_type_enum(),
                // interface_count:
                context.i64_type().as_basic_type_enum(),
                // type_:
                context.ptr_type(*ADDRESS_SPACE).as_basic_type_enum(),
            ],
            false,
        );
        let fields_type = context.opaque_struct_type("object_field");
        fields_type.set_body(
            &[
                // name (u64, interned identifier)
                context.i64_type().as_basic_type_enum(),
                // value (union of (u64, *mut Object, fn())
                context.i64_type().as_basic_type_enum(),
                // value_kind
                context.i8_type().as_basic_type_enum(),
                // type_id
                context.i16_type().as_basic_type_enum(),
            ],
            false,
        );

        // TODO run the constructor for the object
        ObjectFunctions {
            object_type,
            fields_type,
        }
    })
}
