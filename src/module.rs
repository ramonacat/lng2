use inkwell::{
    context::Context,
    execution_engine::{JitFunction, UnsafeFunctionPointer},
    module::Module,
};

pub struct ModuleCompiler<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    execution_engine: inkwell::execution_engine::ExecutionEngine<'ctx>,
}

impl<'ctx> ModuleCompiler<'ctx> {
    pub(super) fn new(context: &'ctx Context, module: Module<'ctx>) -> Self {
        let execution_engine = module
            .create_jit_execution_engine(inkwell::OptimizationLevel::Default)
            .unwrap();

        Self {
            context,
            module,
            execution_engine,
        }
    }

    /// SAFETY: The `compile_insides` closure must create a function, whose signature matches `TReturn`
    pub(super) unsafe fn build_function<TReturn: UnsafeFunctionPointer>(
        &self,
        name: &str,
        compile_insides: impl FnOnce(&'ctx Context, &Module<'ctx>),
    ) -> JitFunction<TReturn> {
        compile_insides(self.context, &self.module);

        unsafe { self.execution_engine.get_function(name) }.unwrap()
    }
}
