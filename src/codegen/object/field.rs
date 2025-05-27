use inkwell::{
    builder::Builder,
    types::AnyTypeEnum,
    values::{IntValue, PointerValue},
};

use super::{Object, representation};
use crate::{
    ADDRESS_SPACE,
    codegen::{
        CodegenHelpers as _, ValueType,
        context::{AnyCompilerContext, FunctionCompilerContext},
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
        let value_kind = self.build_load_value_kind(builder, &context);

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
            .append_basic_block(context.function_value, "then");

        let else_block = context
            .context()
            .append_basic_block(context.function_value, "else");

        builder
            .build_conditional_branch(is_kind_not_callable, then_block, else_block)
            .unwrap();
        builder.position_at_end(then_block);
        builder.build_fatal_error(1, &context);

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

        StoredValue::new(
            Storage::Heap(value.into_pointer_value()),
            ValueType::Callable(self.type_.into_function_type()),
        )
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

        let (function_type, function_pointer) = function_pointer.into_callable();

        let result = builder
            .build_indirect_call(function_type, function_pointer, &arguments, "call_result")
            .unwrap();

        // TODO this will awfully break once there are any types beside string & unit
        if self.type_.into_function_type().get_return_type().is_some() {
            Some(StoredValue::new(
                Storage::Heap(
                    result
                        .try_as_basic_value()
                        .unwrap_left()
                        .into_pointer_value(),
                ),
                ValueType::String,
            ))
        } else {
            None
        }
    }

    fn build_load_value_kind(
        &self,
        builder: &Builder<'ctx>,
        context: &dyn AnyCompilerContext<'ctx, 'a>,
    ) -> IntValue<'ctx> {
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
