mod declarations;
mod definitions;

use declarations::DeclarationsChecker;
use definitions::DefinitionsChecker;

use crate::ast::{NodeType, SourceFile, Statement};
use crate::identifier::{Identifier, Identifiers};

#[derive(Debug)]
#[allow(unused)]
pub enum TypeKind {
    Class(Identifier),
    ExternalFunction(Identifier, String),
    Function(Identifier),
    Todo,
}

#[derive(Debug)]
#[allow(unused)]
pub struct Type {
    kind: TypeKind,
}

impl NodeType for Type {
    fn pretty(&self, identifiers: &Identifiers) -> String {
        let name = match &self.kind {
            TypeKind::Class(identifier) => format!("class({})", identifiers.resolve(*identifier)),
            TypeKind::ExternalFunction(identifier, external_name) => format!(
                "external-function({}, \"{external_name}\")",
                identifiers.resolve(*identifier)
            ),
            TypeKind::Function(identifier) => {
                format!("function({})", identifiers.resolve(*identifier))
            }
            TypeKind::Todo => "TODO".to_string(),
        };

        format!("({name}) ")
    }
}

#[derive(Debug)]
pub enum UncheckedFunctionType {
    Function { statements: Vec<Statement<()>> },
    ExternalFunction(Identifier, String),
}

impl NodeType for UncheckedFunctionType {
    fn pretty(&self, _identifiers: &Identifiers) -> String {
        "(unchecked-function) ".to_string()
    }
}

pub fn type_check(ast: SourceFile<(), (), ()>) -> SourceFile<Type, Type, Type> {
    let declarations_checker = DeclarationsChecker {};
    let checked_file = declarations_checker.check_file(ast);

    let definitions_checker = DefinitionsChecker {};

    definitions_checker.check_file(checked_file)
}
