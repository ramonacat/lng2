use inkwell::{
    builder::Builder,
    types::AnyTypeEnum,
    values::{IntValue, PointerValue},
};

use super::representation;
use crate::{
    ADDRESS_SPACE,
    codegen::{
        CodegenHelpers as _, ValueType,
        context::{AnyCompilerContext, AnyFunctionCompilerContext},
        helpers::BuilderHelpers as _,
        stored_value::{Storage, StoredValue},
    },
    identifier::Identifier,
};

#[derive(Debug)]
pub struct FieldDeclaration<'ctx, 'class> {
    pub name: Identifier,
    pub value: StoredValue<'ctx, 'class>,
}

#[derive(Debug, Clone, Copy)]
pub struct Field<'ctx> {
    pub value_pointer: PointerValue<'ctx>,
    pub type_: AnyTypeEnum<'ctx>,
}

impl<'ctx> Field<'ctx> {
    pub const fn new(field: PointerValue<'ctx>, type_: AnyTypeEnum<'ctx>) -> Self {
        Self {
            value_pointer: field,
            type_,
        }
    }

    pub(in crate::codegen) fn build_read_value<'a>(
        &self,
        builder: &Builder<'ctx>,
        context: &dyn AnyFunctionCompilerContext<'ctx, 'a>,
    ) -> StoredValue<'ctx, '_>
    where
        'ctx: 'a,
    {
        // TODO instead of asserting and erroring out on non-callables, we should instead just
        // return a matching Value every time
        let value_kind = self.build_load_value_kind(builder, context);

        let is_kind_not_callable = builder
            .build_int_compare(
                inkwell::IntPredicate::NE,
                value_kind,
                context
                    .context()
                    .const_u8(representation::ObjectFieldKind::Callable as u8),
                "is_kind_not_callable",
            )
            .unwrap();

        let then_block = context
            .context()
            .append_basic_block(context.function_value(), "then");

        let else_block = context
            .context()
            .append_basic_block(context.function_value(), "else");

        builder
            .build_conditional_branch(is_kind_not_callable, then_block, else_block)
            .unwrap();
        builder.position_at_end(then_block);
        builder.build_fatal_error(1, context);

        builder.position_at_end(else_block);

        StoredValue::new(
            Storage::Heap(self.build_load_value(builder, context)),
            ValueType::Callable(self.type_.into_function_type()),
        )
    }

    fn build_load_value<'a>(
        &self,
        builder: &Builder<'ctx>,
        context: &dyn AnyCompilerContext<'ctx, 'a>,
    ) -> PointerValue<'ctx>
    where
        'ctx: 'a,
    {
        let value_pointer = builder
            .build_struct_gep(
                context.object_functions().fields_type,
                self.value_pointer,
                1,
                "value_kind_pointer",
            )
            .unwrap();

        builder
            .build_load(
                context.context().ptr_type(*ADDRESS_SPACE),
                value_pointer,
                "value_kind",
            )
            .unwrap()
            .into_pointer_value()
    }

    fn build_load_value_kind<'a>(
        &self,
        builder: &Builder<'ctx>,
        context: &dyn AnyCompilerContext<'ctx, 'a>,
    ) -> IntValue<'ctx>
    where
        'ctx: 'a,
    {
        let value_kind_pointer = builder
            .build_struct_gep(
                context.object_functions().fields_type,
                self.value_pointer,
                2,
                "value_kind_pointer",
            )
            .unwrap();

        builder
            .build_load(
                context.context().i8_type(),
                value_kind_pointer,
                "value_kind",
            )
            .unwrap()
            .into_int_value()
    }
}
