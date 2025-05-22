mod representation;

use inkwell::{
    types::BasicType,
    values::{BasicValue, FunctionValue, GlobalValue},
};
use representation::ObjectFieldKind;

use crate::{
    ADDRESS_SPACE,
    codegen::ClassDeclaration,
    identifier::Identifier,
    module::{CompilerServices, ModuleCompiler},
};

#[derive(Clone)]
pub enum Value<'ctx> {
    None,
    Callable(FunctionValue<'ctx>),
    Class(ClassDeclaration<'ctx>),
}

pub struct Field<'ctx> {
    pub name: Identifier,
    pub value: Value<'ctx>,
}

pub struct ObjectFunctions<'ctx> {
    object_type: inkwell::types::StructType<'ctx>,
}

impl<'ctx> ObjectFunctions<'ctx> {
    pub(crate) fn declare_class(
        &self,
        name: &str,
        fields: &[Field],
        context: &'ctx inkwell::context::Context,
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
        let fields_type = context.struct_type(
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

        // TODO use the actual name of the class here for the constructor name!
        let constructor = module.add_function(
            "class_constructor",
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
        let fields_value = module.add_global(fields_type.array_type(fields_len), None, "fields");
        fields_value.set_initializer(&fields_type.array_type(fields_len).const_zero());

        for (index, field) in fields.iter().enumerate() {
            let name_ptr = unsafe {
                fields_field.const_gep(
                    fields_type,
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
            };

            let value_ptr = unsafe {
                fields_field.const_gep(
                    fields_type,
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
                    fields_type,
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

        // TODO run the constructor for the object
        ObjectFunctions { object_type }
    })
}
