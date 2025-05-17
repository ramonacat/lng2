use crate::ADDRESS_SPACE;
use crate::{module::ModuleCompiler, object::ObjectFunctions};
use inkwell::module::Linkage;
use inkwell::types::BasicType;
use inkwell::values::BasicValue;

pub fn generate_vector_definition(
    module_compiler: &ModuleCompiler,
    object_functions: &ObjectFunctions,
) {
    module_compiler.build(|context, module| {
        // TODO name mangling
        let vector_definition = module.add_global(
            context.ptr_type(*ADDRESS_SPACE),
            Some(*ADDRESS_SPACE),
            "vector_definition",
        );
        vector_definition.set_initializer(&context.ptr_type(*ADDRESS_SPACE).const_null());

        let initializer = module.add_function(
            // TODO name mangling to avoid conflicts with userspace code
            "vector_initializer",
            context.void_type().fn_type(&[], false),
            None,
        );

        let builder = context.create_builder();
        let basic_block = context.append_basic_block(initializer, "entry");
        builder.position_at_end(basic_block);
        let vector_struct_descriptor = builder
            .build_call(
                object_functions.create,
                &[context.ptr_type(*ADDRESS_SPACE).const_null().into()],
                "vector_struct_descriptor",
            )
            .unwrap();
        builder
            .build_store(
                vector_definition.as_pointer_value(),
                vector_struct_descriptor.try_as_basic_value().unwrap_left(),
            )
            .unwrap();
        builder.build_return(None).unwrap();

        let type_ = context.opaque_struct_type("global_ctors_entry");
        type_.set_body(
            &[
                context.i32_type().as_basic_type_enum(),
                context.ptr_type(*ADDRESS_SPACE).as_basic_type_enum(),
                context.ptr_type(*ADDRESS_SPACE).as_basic_type_enum(),
            ],
            false,
        );

        let global = module.add_global(
            type_.array_type(1).as_basic_type_enum(),
            None,
            "llvm.global_ctors",
        );
        global.set_linkage(Linkage::Appending);
        global.set_initializer(
            &type_
                .const_array(&[type_.const_named_struct(&[
                    context.i32_type().const_int(1, false).as_basic_value_enum(),
                    initializer
                        .as_global_value()
                        .as_pointer_value()
                        .as_basic_value_enum(),
                    vector_definition.as_pointer_value().as_basic_value_enum(),
                ])])
                .as_basic_value_enum(),
        );
    });
}
