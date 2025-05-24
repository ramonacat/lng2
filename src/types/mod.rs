use class::{ClassType, UncheckedClassType};
use expression::ExpressionType;
use function::{FunctionType, UncheckedFunctionType};

use crate::ast;

pub mod class;
pub mod expression;
pub mod function;

pub type TypedAst = ast::SourceFile<ClassType, FunctionType, ExpressionType>;
pub type UntypedAst =
    ast::SourceFile<UncheckedClassType, UncheckedFunctionType, Option<ast::TypeConstraint>>;
