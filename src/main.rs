#![deny(clippy::all, clippy::pedantic, clippy::nursery, warnings)]
mod module;

use inkwell::{context::Context, module::Linkage};
use module::ModuleCompiler;

fn main() {
    let context = Context::create();
    let module = context.create_module("main");

    let module_compiler = ModuleCompiler::new(&context, module);

    let main = unsafe {
        module_compiler.build_function::<unsafe extern "C" fn() -> u64>(
            "main",
            |context, module| {
                let builder = context.create_builder();
                let type_ = context.i64_type().fn_type(&[], false);
                let function = module.add_function("main", type_, Some(Linkage::External));

                let basic_block = context.append_basic_block(function, "entry");
                builder.position_at_end(basic_block);
                builder
                    .build_return(Some(&context.i64_type().const_int(12_456_789, false)))
                    .unwrap();
            },
        )
    };

    let result = unsafe { main.call() };

    println!("{result}");
}
