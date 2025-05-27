pub(super) mod field;
mod representation;

use field::{Field, FieldDeclaration};
use inkwell::{
    builder::Builder,
    types::{AnyType, BasicType, BasicTypeEnum},
    values::{BasicValue, BasicValueEnum, IntValue, PointerValue},
};
use representation::ObjectFieldKind;

use super::{CodegenHelpers as _, context::FunctionCompilerContext, helpers::BuilderHelpers as _};
use crate::{
    ADDRESS_SPACE,
    codegen::{
        context::{AnyCompilerContext, CompilerContext},
        make_fn_type,
    },
    module::{CompilerServices, ModuleCompiler},
};

#[derive(Debug)]
pub struct Object<'ctx> {
    pub self_: PointerValue<'ctx>,
}

impl<'ctx, 'a> Object<'ctx> {
    pub(crate) fn get_field(
        &self,
        field_index: usize,
        context: FunctionCompilerContext<'ctx, '_, '_>,
        builder: &Builder<'ctx>,
    ) -> Field<'ctx, '_> {
        let field_count = self.build_load_field_count(&context, builder);

        let field_index_too_great = builder
            .build_int_compare(
                inkwell::IntPredicate::UGE,
                context.context().const_u64(field_index as u64),
                field_count,
                "is_field_index_too_great",
            )
            .unwrap();

        let then_block = context
            .context()
            .append_basic_block(context.function_value, "then");

        let else_block = context
            .context()
            .append_basic_block(context.function_value, "else");

        builder
            .build_conditional_branch(field_index_too_great, then_block, else_block)
            .unwrap();

        builder.position_at_end(then_block);
        builder.build_fatal_error(2, &context);

        builder.position_at_end(else_block);

        let field =
            self.build_load_object_field(u32::try_from(field_index).unwrap(), &context, builder);

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

    fn build_load_field(
        &self,
        index: u32,
        value_type: BasicTypeEnum<'ctx>,
        context: &dyn AnyCompilerContext<'ctx, 'a>,
        builder: &Builder<'ctx>,
    ) -> BasicValueEnum<'ctx>
    where
        'ctx: 'a,
    {
        let fields_pointer = builder
            .build_struct_gep(
                context.object_functions().object_type,
                self.self_,
                index,
                "fields",
            )
            .unwrap();

        builder
            .build_load(value_type, fields_pointer, "fields")
            .unwrap()
    }

    fn build_load_fields(
        &self,
        context: &dyn AnyCompilerContext<'ctx, 'a>,
        builder: &Builder<'ctx>,
    ) -> PointerValue<'ctx>
    where
        'ctx: 'a,
    {
        self.build_load_field(
            0,
            context.context().ptr_type(*ADDRESS_SPACE).into(),
            context,
            builder,
        )
        .into_pointer_value()
    }

    fn build_load_field_count(
        &self,
        context: &dyn AnyCompilerContext<'ctx, 'a>,
        builder: &Builder<'ctx>,
    ) -> IntValue<'ctx>
    where
        'ctx: 'a,
    {
        self.build_load_field(1, context.context().i64_type().into(), context, builder)
            .into_int_value()
    }

    fn build_load_object_field(
        &self,
        field_index: u32,
        context: &dyn AnyCompilerContext<'ctx, 'a>,
        builder: &Builder<'ctx>,
    ) -> PointerValue<'ctx>
    where
        'ctx: 'a,
    {
        let fields = self.build_load_fields(context, builder);

        unsafe {
            builder
                .build_gep(
                    context.object_functions().fields_type,
                    fields,
                    &[context.context().const_u64(u64::from(field_index))],
                    "field",
                )
                .unwrap()
        }
    }
}

#[derive(Debug)]
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

        let field_count_field = builder
            .build_struct_gep(
                self.object_type,
                global.as_pointer_value(),
                1,
                "fields_count_ptr",
            )
            .unwrap();

        let fields_len = u32::try_from(fields.len()).unwrap();
        let fields_value = compiler_context.module.add_global(
            self.fields_type.array_type(fields_len),
            None,
            "fields",
        );
        fields_value.set_initializer(&self.fields_type.array_type(fields_len).const_zero());

        self.fill_fields(
            fields,
            compiler_context,
            &builder,
            fields_value.as_pointer_value(),
        );

        builder.build_store(fields_field, fields_value).unwrap();
        builder
            .build_store(
                field_count_field,
                compiler_context
                    .context()
                    .i64_type()
                    .const_int(u64::from(fields_len), false),
            )
            .unwrap();

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
            let value = field.value.into_callable().1;

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

            self.store_field(
                index,
                1,
                value.as_basic_value_enum(),
                fields_field,
                builder,
                compiler_context,
            );

            self.store_field(
                index,
                2,
                compiler_context
                    .context
                    .const_u8(ObjectFieldKind::Callable as u8)
                    .into(),
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
