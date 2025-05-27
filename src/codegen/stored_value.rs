use inkwell::{
    builder::Builder,
    types::FunctionType,
    values::{BasicValue as _, BasicValueEnum, GlobalValue, PointerValue},
};

use super::{FunctionCompilerContext, object::field::Field};
use crate::{codegen::ClassDeclaration, identifier::Identifier};

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
    pub(crate) const fn new(
        storage: Storage<'ctx, 'class>,
        type_: ValueType<'ctx, 'class>,
    ) -> Self {
        Self { storage, type_ }
    }

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

    pub(crate) fn into_callable(self) -> (FunctionType<'ctx>, PointerValue<'ctx>) {
        let ValueType::Callable(function_type) = self.type_ else {
            todo!();
        };

        (
            function_type,
            self.into_basic_value_enum().into_pointer_value(),
        )
    }
}
