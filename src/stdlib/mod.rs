use inkwell::{execution_engine::ExecutionEngine, module::Module};

extern "C" fn printline() {
    println!("ok");
}

// TODO print some actual useful information here (detailed error message, stacktrace, etc.)
extern "C" fn fatal_error() {
    println!("fatal error!");

    std::process::exit(1);
}

pub fn register_mappings(module: &Module, execution_engine: &ExecutionEngine) {
    execution_engine
        .add_global_mapping(&module.get_function("println").unwrap(), printline as usize);

    execution_engine.add_global_mapping(
        &module.get_function("fatal_error").unwrap(),
        fatal_error as usize,
    );
}
