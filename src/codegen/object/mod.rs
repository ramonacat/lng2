mod representation;

use crate::codegen::{CompilerContext, make_fn_type};
use inkwell::types::AnyType;
use inkwell::types::AnyTypeEnum;
use inkwell::types::FunctionType;
use inkwell::values::BasicValueEnum;
use inkwell::values::PointerValue;
use inkwell::{
    builder::Builder,
    types::BasicType,
    values::{BasicValue, FunctionValue},
};
use representation::ObjectFieldKind;

use crate::{
    ADDRESS_SPACE,
    codegen::{AnyCompilerContext as _, ClassDeclaration, FunctionCompilerContext},
    identifier::Identifier,
    module::{CompilerServices, ModuleCompiler},
};

// TODO we should really only have an Object and Primitive in terms of kinds of values here, the
// rest should be handled by builtin magical objects
#[derive(Debug, Clone, Copy)]
pub enum Value<'ctx, 'class> {
    None,
    Callable(FunctionValue<'ctx>),
    Field(Field<'ctx, 'class>),
    Class(&'class ClassDeclaration<'ctx, 'class>),
    String(PointerValue<'ctx>),
    IndirectCallable(FunctionType<'ctx>, PointerValue<'ctx>),
}

#[derive(Debug)]
pub struct FieldDeclaration<'ctx, 'class> {
    pub name: Identifier,
    pub value: Value<'ctx, 'class>,
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
    ) -> Value<'ctx, '_> {
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
                "deref_function",
            )
            .unwrap();

        Value::IndirectCallable(self.type_.into_function_type(), value.into_pointer_value())
    }

    pub(crate) fn build_call<'class>(
        &self,
        arguments: Vec<Value<'ctx, 'class>>,
        builder: &Builder<'ctx>,
        context: FunctionCompilerContext<'ctx, 'class, '_>,
    ) -> Value<'ctx, 'class> {
        // TODO ensure we have the correct function signature here
        // TODO handle the return value
        let function_pointer = self.build_read_value(builder, context);
        let arguments = arguments
            .into_iter()
            .map(|x| match x {
                Value::None => todo!(),
                Value::Callable(_) => todo!(),
                Value::Field(_) => todo!(),
                Value::Class(_) => todo!(),
                Value::String(pointer) => pointer.as_basic_value_enum().into(),
                Value::IndirectCallable(_, _) => todo!(),
            })
            .collect::<Vec<_>>();

        let (function_type, function_pointer) = match function_pointer {
            Value::None => todo!(),
            Value::Callable(_) => todo!(),
            Value::Field(_) => todo!(),
            Value::Class(_) => todo!(),
            Value::String(_) => todo!(),
            Value::IndirectCallable(type_, value) => (type_, value),
        };

        let result = builder
            .build_indirect_call(function_type, function_pointer, &arguments, "call_result")
            .unwrap();

        // TODO this will awfully break once there are any types beside string & unit
        if self.type_.into_function_type().get_return_type().is_some() {
            Value::String(
                result
                    .try_as_basic_value()
                    .unwrap_left()
                    .into_pointer_value(),
            )
        } else {
            Value::None
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
            let (value, value_kind) = match field.value {
                Value::Callable(function_value) => (function_value, ObjectFieldKind::Callable),
                Value::None => todo!(),
                Value::Class(_) => todo!(),
                Value::Field(_) => todo!(),
                Value::String(_) => todo!(),
                Value::IndirectCallable(_, _) => todo!(),
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

            self.store_field(
                index,
                1,
                value.as_global_value().as_basic_value_enum(),
                fields_field,
                builder,
                compiler_context,
            );

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
