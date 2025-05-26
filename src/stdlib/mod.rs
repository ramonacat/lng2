use std::ffi::{CStr, c_char};

use inkwell::{execution_engine::ExecutionEngine, module::Module};

// TODO this should take a string object
extern "C" fn printline(line: *const c_char) {
    println!("{}", unsafe { CStr::from_ptr(line) }.to_str().unwrap());
}

// TODO print some actual useful information here (detailed error message, stacktrace, etc.)
// The marker is a hack so we know which place is throwing the error without doing the actual work
// of showing useful errors.
extern "C" fn fatal_error(marker: u64) {
    println!("fatal error! {marker}");

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
