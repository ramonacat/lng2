use inkwell::{types::BasicType, values::FunctionValue};

use crate::{ADDRESS_SPACE, module::ModuleCompiler};

#[repr(C)]
#[allow(unused)] // TODO use it 
struct ObjectHandle {}

#[allow(unused)] // TODO use it 
pub struct ObjectFunctions<'ctx> {
    pub create: FunctionValue<'ctx>,
    pub destroy: FunctionValue<'ctx>,
}

pub fn generate_object_functions<'ctx>(
    module_compiler: &ModuleCompiler<'ctx>,
) -> ObjectFunctions<'ctx> {
    module_compiler.build(|context, module| {
        let object_type = context.opaque_struct_type("object_type");
        object_type.set_body(
            &[
                // fields:
                context.ptr_type(*ADDRESS_SPACE).as_basic_type_enum(),
            ],
            false,
        );

        let create = generate_object_create(context, module, object_type);
        let destroy = generate_object_destroy(context, module);

        // TODO run the constructor for the object
        ObjectFunctions { create, destroy }
    })
}

fn generate_object_create<'ctx>(
    context: &'ctx inkwell::context::Context,
    module: &inkwell::module::Module<'ctx>,
    object_type: inkwell::types::StructType<'ctx>,
) -> FunctionValue<'ctx> {
    let object_create = module.add_function(
        "object_create",
        // The argument is the object that is the type descriptor
        //    - it must be alive until the function returns
        // The return type is the newly created object
        //    - the caller is responsible for freeing the return value, by calling
        //    object_destroy
        context
            .ptr_type(*ADDRESS_SPACE)
            .fn_type(&[context.ptr_type(*ADDRESS_SPACE).into()], false),
        None,
    );

    let builder = context.create_builder();
    let entry = context.append_basic_block(object_create, "entry");
    builder.position_at_end(entry);

    let new_object = builder.build_malloc(object_type, "new_object").unwrap();
    builder.build_return(Some(&new_object)).unwrap();

    object_create
}

fn generate_object_destroy<'ctx>(
    context: &'ctx inkwell::context::Context,
    module: &inkwell::module::Module<'ctx>,
) -> FunctionValue<'ctx> {
    let object_create = module.add_function(
        "object_destroy",
        // The argument is an object created through object_create.
        context
            .void_type()
            .fn_type(&[context.ptr_type(*ADDRESS_SPACE).into()], false),
        None,
    );

    let builder = context.create_builder();
    let entry = context.append_basic_block(object_create, "entry");
    builder.position_at_end(entry);

    builder
        .build_free(
            object_create
                .get_first_param()
                .unwrap()
                .into_pointer_value(),
        )
        .unwrap();
    builder.build_return(None).unwrap();

    object_create
}
