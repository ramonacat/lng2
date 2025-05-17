use std::fmt::Debug;

use inkwell::{context::Context, execution_engine::ExecutionEngine, module::Module};

pub struct ModuleCompiler<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
}

impl<'ctx> ModuleCompiler<'ctx> {
    pub(super) const fn new(context: &'ctx Context, module: Module<'ctx>) -> Self {
        Self { context, module }
    }

    pub(crate) fn build<TReturn>(
        &self,
        compile_insides: impl FnOnce(&'ctx Context, &Module<'ctx>) -> TReturn,
    ) -> TReturn {
        compile_insides(self.context, &self.module)
    }

    pub(crate) fn run(&self, action: impl Fn(&ExecutionEngine<'ctx>)) {
        let execution_engine = self
            .module
            .create_jit_execution_engine(inkwell::OptimizationLevel::Default)
            .unwrap();

        self.module.verify().unwrap();

        execution_engine.run_static_constructors();
        action(&execution_engine);
        execution_engine.run_static_destructors();
    }
}

impl Debug for ModuleCompiler<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            &self
                .module
                .print_to_string()
                .to_string()
                .replace("\\n", "\n"),
        )
    }
}
