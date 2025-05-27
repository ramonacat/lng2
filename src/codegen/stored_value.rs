use inkwell::{
    builder::Builder,
    types::FunctionType,
    values::{BasicValue as _, BasicValueEnum, GlobalValue, PointerValue},
};

use super::{FunctionCompilerContext, context::AnyFunctionCompilerContext, object::field::Field};
use crate::{codegen::ClassDeclaration, identifier::Identifier};

#[derive(Debug, Clone, Copy)]
pub enum Storage<'ctx> {
    Field(Field<'ctx>),
    Global(GlobalValue<'ctx>),
    Builtin,
    Heap(PointerValue<'ctx>),
}

#[derive(Debug, Clone, Copy)]
pub enum ValueType<'ctx, 'class> {
    String,
    Class(&'class ClassDeclaration<'ctx>),
    Callable(FunctionType<'ctx>),
}

#[derive(Debug, Clone, Copy)]
pub struct StoredValue<'ctx, 'class> {
    storage: Storage<'ctx>,
    type_: ValueType<'ctx, 'class>,
}

impl<'ctx, 'class> StoredValue<'ctx, 'class> {
    pub(crate) const fn new(storage: Storage<'ctx>, type_: ValueType<'ctx, 'class>) -> Self {
        Self { storage, type_ }
    }

    fn into_basic_value_enum<'a>(
        self,
        builder: &Builder<'ctx>,
        context: &dyn AnyFunctionCompilerContext<'ctx, 'a>,
    ) -> BasicValueEnum<'ctx>
    where
        'ctx: 'a,
    {
        match self.storage {
            Storage::Field(field) => field
                .build_read_value(builder, context)
                .into_basic_value_enum(builder, context),
            Storage::Global(global) => global.as_basic_value_enum(),
            Storage::Builtin => todo!(),
            Storage::Heap(pointer_value) => pointer_value.as_basic_value_enum(),
        }
    }

    pub(in crate::codegen) fn build_call<'a>(
        &self,
        arguments: Vec<Self>,
        builder: &Builder<'ctx>,
        context: &dyn AnyFunctionCompilerContext<'ctx, 'a>,
    ) -> Option<Self>
    where
        'ctx: 'a,
    {
        let (function_type, function_value) = self.into_callable(builder, context);

        let arguments = arguments
            .into_iter()
            .map(|x| x.into_basic_value_enum(builder, context).into())
            .collect::<Vec<_>>();

        let call_result = builder
            .build_indirect_call(function_type, function_value, &arguments, "value_call")
            .unwrap();

        // TODO this will break once we start passing things that aren't strings...
        if function_type.get_return_type().is_some() {
            Some(StoredValue {
                storage: Storage::Heap(
                    call_result
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

    pub(crate) fn field_access(
        &self,
        field: Identifier,
        builder: &Builder<'ctx>,
        context: FunctionCompilerContext<'ctx, 'class, 'class>,
    ) -> Option<Self> {
        match &self.storage {
            Storage::Field(_) => todo!(),
            Storage::Global(_) => todo!(),
            Storage::Builtin => match &self.type_ {
                ValueType::String => todo!(),
                ValueType::Class(class_declaration) => {
                    Some(class_declaration.field_access(field, builder, context))
                }
                ValueType::Callable(_) => todo!(),
            },
            Storage::Heap(_) => todo!(),
        }
    }

    pub(in crate::codegen) fn into_callable<'a>(
        self,
        builder: &Builder<'ctx>,
        context: &dyn AnyFunctionCompilerContext<'ctx, 'a>,
    ) -> (FunctionType<'ctx>, PointerValue<'ctx>)
    where
        'ctx: 'a,
    {
        let ValueType::Callable(function_type) = self.type_ else {
            todo!();
        };

        (
            function_type,
            self.into_basic_value_enum(builder, context)
                .into_pointer_value(),
        )
    }

    pub(in crate::codegen) fn build_return<'a>(
        &self,
        builder: &Builder<'ctx>,
        context: &dyn AnyFunctionCompilerContext<'ctx, 'a>,
    ) where
        'ctx: 'a,
    {
        builder
            .build_return(Some(&self.into_basic_value_enum(builder, context)))
            .unwrap();
    }
}
