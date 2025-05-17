#![deny(clippy::all, clippy::pedantic, clippy::nursery, warnings)]
mod module;
mod object;
mod vector;

use std::sync::LazyLock;

use inkwell::{AddressSpace, context::Context, module::Linkage};
use module::ModuleCompiler;

static ADDRESS_SPACE: LazyLock<AddressSpace> = LazyLock::new(AddressSpace::default);

fn main() {
    let context = Context::create();
    let module = context.create_module("main");

    let module_compiler = ModuleCompiler::new(&context, module);

    module_compiler.build(|context, module| {
        let builder = context.create_builder();
        let type_ = context.i64_type().fn_type(&[], false);
        let function = module.add_function("main", type_, Some(Linkage::External));

        let basic_block = context.append_basic_block(function, "entry");
        builder.position_at_end(basic_block);
        builder
            .build_return(Some(&context.i64_type().const_int(12_456_789, false)))
            .unwrap();
    });
    let object_functions = crate::object::generate_object_functions(&module_compiler);
    crate::vector::generate_vector_definition(&module_compiler, &object_functions);

    println!("{module_compiler:?}");

    module_compiler.run(|execution_engine| {
        let main =
            unsafe { execution_engine.get_function::<unsafe extern "C" fn() -> u64>("main") }
                .unwrap();
        let result = unsafe { main.call() };

        println!("{result}");
    });
}
