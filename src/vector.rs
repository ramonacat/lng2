use crate::ADDRESS_SPACE;
use crate::{module::ModuleCompiler, object::ObjectFunctions};

pub fn generate_vector_definition<'ctx>(
    module_compiler: &mut ModuleCompiler<'ctx>,
    object_functions: &ObjectFunctions<'ctx>,
) {
    module_compiler.build(|context, module, compiler_services| {
        // TODO name mangling
        let vector_definition = module.add_global(
            context.ptr_type(*ADDRESS_SPACE),
            Some(*ADDRESS_SPACE),
            "vector_definition",
        );
        vector_definition.set_initializer(&context.ptr_type(*ADDRESS_SPACE).const_null());

        create_struct_definition_constructor(
            object_functions,
            context,
            module,
            compiler_services,
            vector_definition,
        );

        create_struct_definition_destructor(
            object_functions,
            context,
            module,
            compiler_services,
            vector_definition,
        );
    });
}

fn create_struct_definition_constructor<'ctx>(
    object_functions: &ObjectFunctions<'ctx>,
    context: &'ctx inkwell::context::Context,
    module: &inkwell::module::Module<'ctx>,
    compiler_services: &mut crate::module::CompilerServices<'ctx>,
    vector_definition: inkwell::values::GlobalValue<'ctx>,
) {
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

    compiler_services.add_global_destructor(
        1,
        initializer.as_global_value().as_pointer_value(),
        Some(vector_definition.as_pointer_value()),
    );
}

fn create_struct_definition_destructor<'ctx>(
    object_functions: &ObjectFunctions<'ctx>,
    context: &'ctx inkwell::context::Context,
    module: &inkwell::module::Module<'ctx>,
    compiler_services: &mut crate::module::CompilerServices<'ctx>,
    vector_definition: inkwell::values::GlobalValue<'ctx>,
) {
    let finalizer = module.add_function(
        // TODO name mangling to avoid conflicts with userspace code
        "vector_finalizer",
        context.void_type().fn_type(&[], false),
        None,
    );

    let builder = context.create_builder();
    let basic_block = context.append_basic_block(finalizer, "entry");

    builder.position_at_end(basic_block);

    let vector_definition_value = builder
        .build_load(
            context.ptr_type(*ADDRESS_SPACE),
            vector_definition.as_pointer_value(),
            "vector_definition",
        )
        .unwrap();

    builder
        .build_call(
            object_functions.destroy,
            &[vector_definition_value.into()],
            "vector_struct_descriptor",
        )
        .unwrap();

    builder.build_return(None).unwrap();

    compiler_services.add_global_construtor(
        1,
        finalizer.as_global_value().as_pointer_value(),
        Some(vector_definition.as_pointer_value()),
    );
}
