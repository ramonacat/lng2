use crate::codegen::ClassDeclaration;
use crate::codegen::object::Field;
use crate::identifier::Identifier;
use inkwell::builder::Builder;
use inkwell::types::FunctionType;
use inkwell::values::BasicValue as _;
use inkwell::values::BasicValueEnum;
use inkwell::values::GlobalValue;
use inkwell::values::PointerValue;

use super::FunctionCompilerContext;

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
    // TODO can we avoid this method, and pull the logic that needs it into StoredValue?
    pub(crate) const fn type_(&self) -> ValueType<'ctx, 'class> {
        self.type_
    }
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
