use inkwell::{execution_engine::ExecutionEngine, module::Module};

extern "C" fn printline() {
    println!("ok");
}

pub fn register_mappings(module: &Module, execution_engine: &ExecutionEngine) {
    execution_engine
        .add_global_mapping(&module.get_function("println").unwrap(), printline as usize);
}
