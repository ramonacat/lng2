mod declarations;
mod definitions;
mod scope;

use declarations::DeclarationsChecker;
use definitions::DefinitionsChecker;

use crate::{
    identifier::Identifiers,
    types::{TypedAst, UntypedAst},
};

pub fn type_check(ast: UntypedAst, identifiers: &Identifiers) -> TypedAst {
    let declarations_checker = DeclarationsChecker::new();
    let checked_file = declarations_checker.check_file(ast);

    let definitions_checker = DefinitionsChecker::new(identifiers);

    definitions_checker.check_file(checked_file)
}
