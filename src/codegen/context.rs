use std::{collections::HashMap, fmt::Debug};

use inkwell::{context::Context, module::Module, values::FunctionValue};

use super::object::ObjectFunctions;
use crate::{
    ast::{Class, Function},
    identifier::Identifiers,
    types::{
        class::ClassType,
        expression::ExpressionType,
        function::{FunctionId, FunctionType},
    },
};

pub trait AnyCompilerContext<'ctx, 'a>: Debug {
    fn context(&self) -> &'ctx Context;
    fn module(&self) -> &'a Module<'ctx>;
    fn object_functions(&self) -> &'a ObjectFunctions<'ctx>;
    fn identifiers(&self) -> &'a Identifiers;
    fn fatal_error(&self) -> FunctionValue<'ctx>;
}

#[derive(Debug, Clone, Copy)]
pub struct FunctionCompilerContext<'ctx, 'a, 'class> {
    pub class: ClassCompilerContext<'ctx, 'a, 'class>,
    pub function_value: FunctionValue<'ctx>,
    pub function: &'a Function<FunctionType, ExpressionType>,
}

impl<'ctx, 'a> AnyCompilerContext<'ctx, 'a> for FunctionCompilerContext<'ctx, 'a, '_> {
    fn context(&self) -> &'ctx Context {
        self.class.context()
    }

    fn module(&self) -> &'a Module<'ctx> {
        self.class.module()
    }

    fn object_functions(&self) -> &'a ObjectFunctions<'ctx> {
        self.class.object_functions()
    }

    fn identifiers(&self) -> &'a Identifiers {
        self.class.identifiers()
    }

    fn fatal_error(&self) -> FunctionValue<'ctx> {
        self.class.fatal_error()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ClassCompilerContext<'ctx, 'a, 'class> {
    pub compiler: CompilerContext<'ctx, 'a>,
    pub function_declarations: &'a HashMap<FunctionId, FunctionValue<'ctx>>,
    pub class: &'class Class<ClassType, FunctionType, ExpressionType>,
}

impl<'ctx, 'a> AnyCompilerContext<'ctx, 'a> for ClassCompilerContext<'ctx, 'a, '_> {
    fn context(&self) -> &'ctx Context {
        self.compiler.context
    }

    fn module(&self) -> &'a Module<'ctx> {
        self.compiler.module
    }

    fn object_functions(&self) -> &'a ObjectFunctions<'ctx> {
        self.compiler.object_functions
    }

    fn identifiers(&self) -> &'a Identifiers {
        self.compiler.identifiers
    }

    fn fatal_error(&self) -> FunctionValue<'ctx> {
        self.compiler.fatal_error
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CompilerContext<'ctx, 'a> {
    pub context: &'ctx Context,
    pub module: &'a Module<'ctx>,
    pub object_functions: &'a ObjectFunctions<'ctx>,
    pub identifiers: &'a Identifiers,
    pub fatal_error: FunctionValue<'ctx>,
}

impl<'ctx, 'a> AnyCompilerContext<'ctx, 'a> for CompilerContext<'ctx, 'a> {
    fn context(&self) -> &'ctx Context {
        self.context
    }

    fn module(&self) -> &'a Module<'ctx> {
        self.module
    }

    fn object_functions(&self) -> &'a ObjectFunctions<'ctx> {
        self.object_functions
    }

    fn identifiers(&self) -> &'a Identifiers {
        self.identifiers
    }

    fn fatal_error(&self) -> FunctionValue<'ctx> {
        self.fatal_error
    }
}
