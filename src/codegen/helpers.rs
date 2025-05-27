use inkwell::{
    builder::Builder,
    context::Context,
    values::{BasicValue, IntValue},
};

use super::context::AnyCompilerContext;

pub(super) trait CodegenHelpers<'ctx> {
    fn const_u64(&'ctx self, value: u64) -> IntValue<'ctx>;
    fn const_u8(&'ctx self, value: u8) -> IntValue<'ctx>;
}

impl<'ctx> CodegenHelpers<'ctx> for Context {
    fn const_u64(&'ctx self, value: u64) -> IntValue<'ctx> {
        self.i64_type().const_int(value, false)
    }

    fn const_u8(&'ctx self, value: u8) -> IntValue<'ctx> {
        self.i8_type().const_int(value.into(), false)
    }
}

pub(super) trait BuilderHelpers<'ctx> {
    fn build_fatal_error(&self, id: u64, context: &dyn AnyCompilerContext<'ctx, '_>);
}

impl<'ctx> BuilderHelpers<'ctx> for Builder<'ctx> {
    // TODO build a useable error message here and also backtrace, and anything else that could be
    // helpful
    fn build_fatal_error(&self, id: u64, context: &dyn AnyCompilerContext<'ctx, '_>) {
        self.build_direct_call(
            context.fatal_error(),
            &[context
                .context()
                .i64_type()
                .const_int(id, false)
                .as_basic_value_enum()
                .into()],
            "fatal_error",
        )
        .unwrap();

        self.build_unreachable().unwrap();
    }
}
