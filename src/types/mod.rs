use class::ClassType;
use function::{FunctionType, UncheckedFunctionType};

use crate::ast::SourceFile;

pub mod class;
pub mod expression;
pub mod function;

pub type TypedAst = SourceFile<ClassType, FunctionType>;
pub type UntypedAst = SourceFile<(), UncheckedFunctionType>;
