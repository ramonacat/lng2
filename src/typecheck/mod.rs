mod declarations;
mod definitions;

use declarations::DeclarationsChecker;
use definitions::DefinitionsChecker;

use crate::types::{TypedAst, UntypedAst};

pub fn type_check(ast: UntypedAst) -> TypedAst {
    let declarations_checker = DeclarationsChecker::new();
    let checked_file = declarations_checker.check_file(ast);

    let definitions_checker = DefinitionsChecker::new();

    definitions_checker.check_file(checked_file)
}
