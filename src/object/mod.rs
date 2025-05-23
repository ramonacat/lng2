mod representation;

use inkwell::{
    builder::Builder,
    context::Context,
    types::BasicType,
    values::{BasicValue, FunctionValue, GlobalValue, PointerValue},
};
use representation::ObjectFieldKind;

use crate::{
    ADDRESS_SPACE,
    codegen::{ClassCompilerContext, ClassDeclaration},
    identifier::Identifier,
    module::{CompilerServices, ModuleCompiler},
};

pub enum Value<'ctx, 'class> {
    None,
    Callable(FunctionValue<'ctx>),
    Field(Field<'ctx>),
    Class(ClassDeclaration<'ctx, 'class>),
}

pub struct FieldDeclaration<'ctx, 'class> {
    pub name: Identifier,
    pub value: Value<'ctx, 'class>,
}

pub struct Field<'ctx> {
    // TODO self_ should be in fact a struct that has the object pointer and methods like get_field
    #[allow(unused)]
    pub self_: PointerValue<'ctx>,
    pub field: PointerValue<'ctx>,
}

impl<'ctx> Field<'ctx> {
    pub const fn new(self_: PointerValue<'ctx>, field: PointerValue<'ctx>) -> Self {
        Self { self_, field }
    }

    pub(crate) fn build_call(&self, builder: &Builder<'ctx>, context: ClassCompilerContext) {
        // TODO we have to assert here that the field is of the correct type
        let function_pointer_pointer = builder
            .build_struct_gep(
                context.object_functions.fields_type,
                self.field,
                1,
                "function_pointer",
            )
            .unwrap();

        let function_pointer = builder
            .build_load(
                context.context.ptr_type(*ADDRESS_SPACE),
                function_pointer_pointer,
                "deref_function",
            )
            .unwrap();

        // TODO ensure we have the correct function signature here
        // TODO handle the return value
        builder
            .build_indirect_call(
                context.context.void_type().fn_type(&[], false),
                function_pointer.into_pointer_value(),
                &[],
                "call_result",
            )
            .unwrap();
    }
}

pub struct ObjectFunctions<'ctx> {
    object_type: inkwell::types::StructType<'ctx>,
    fields_type: inkwell::types::StructType<'ctx>,
}

impl<'ctx> ObjectFunctions<'ctx> {
    pub(crate) fn declare_class(
        &self,
        name: &str,
        fields: &[FieldDeclaration],
        context: &'ctx Context,
        module: &inkwell::module::Module<'ctx>,
        compiler_services: &mut CompilerServices<'ctx>,
    ) -> GlobalValue<'ctx> {
        let global = module.add_global(self.object_type, Some(*ADDRESS_SPACE), name);

        global.set_initializer(
            &self.object_type.const_named_struct(&[
                context
                    .ptr_type(*ADDRESS_SPACE)
                    .const_null()
                    .as_basic_value_enum(),
                context.i64_type().const_int(0, false).as_basic_value_enum(),
                context
                    .ptr_type(*ADDRESS_SPACE)
                    .const_null()
                    .as_basic_value_enum(),
                context.i64_type().const_int(0, false).as_basic_value_enum(),
                context
                    .ptr_type(*ADDRESS_SPACE)
                    .const_null()
                    .as_basic_value_enum(),
            ]),
        );

        let constructor = module.add_function(
            &format!("class_constructor_{name}"),
            context.void_type().fn_type(&[], false),
            None,
        );
        let basic_block = context.append_basic_block(constructor, "entry");
        let builder = context.create_builder();
        builder.position_at_end(basic_block);

        let fields_field = builder
            .build_struct_gep(self.object_type, global.as_pointer_value(), 0, "fields_ptr")
            .unwrap();
        let fields_len = u32::try_from(fields.len()).unwrap();
        let fields_value =
            module.add_global(self.fields_type.array_type(fields_len), None, "fields");
        fields_value.set_initializer(&self.fields_type.array_type(fields_len).const_zero());

        for (index, field) in fields.iter().enumerate() {
            let name_ptr = unsafe {
                fields_field.const_gep(
                    self.fields_type,
                    &[
                        context.i64_type().const_int(index as u64, false),
                        context.i64_type().const_int(0, false),
                    ],
                )
            };

            builder
                .build_store(
                    name_ptr,
                    context
                        .i64_type()
                        .const_int(field.name.into_id() as u64, false),
                )
                .unwrap();

            let (value, value_kind) = match field.value {
                Value::Callable(function_value) => (function_value, ObjectFieldKind::Callable),
                Value::None => todo!(),
                Value::Class(_) => todo!(),
                Value::Field(_) => todo!(),
            };

            let value_ptr = unsafe {
                fields_field.const_gep(
                    self.fields_type,
                    &[
                        context.i64_type().const_int(index as u64, false),
                        context.i64_type().const_int(1, false),
                    ],
                )
            };

            builder
                .build_store(value_ptr, value.as_global_value())
                .unwrap();

            let value_kind_ptr = unsafe {
                fields_field.const_gep(
                    self.fields_type,
                    &[
                        context.i64_type().const_int(index as u64, false),
                        context.i64_type().const_int(2, false),
                    ],
                )
            };

            builder
                .build_store(
                    value_kind_ptr,
                    context.i8_type().const_int(value_kind as u64, false),
                )
                .unwrap();
        }
        builder.build_store(fields_field, fields_value).unwrap();
        builder.build_return(None).unwrap();

        compiler_services.add_global_construtor(
            128,
            constructor.as_global_value().as_pointer_value(),
            Some(global.as_pointer_value()),
        );

        global
    }

    pub(crate) fn get_field(
        &self,
        descriptor: PointerValue<'ctx>,
        field_index: usize,
        context: ClassCompilerContext<'ctx, '_, '_>,
        builder: &Builder<'ctx>,
    ) -> Field<'ctx> {
        // TODO verify that the field_index is in bounds
        let gep = builder
            .build_struct_gep(self.object_type, descriptor, 0, "fields")
            .unwrap();

        let field = unsafe {
            builder
                .build_gep(
                    self.fields_type,
                    gep,
                    &[context
                        .context
                        .i64_type()
                        .const_int(field_index as u64, false)],
                    "field",
                )
                .unwrap()
        };

        Field::new(descriptor, field)
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
