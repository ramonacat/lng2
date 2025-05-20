use crate::{ast::NodeType, identifier::Identifiers};

use super::function::FunctionId;

#[derive(Debug)]
pub struct UncheckedClassType {}
impl UncheckedClassType {
    pub(crate) const fn new() -> Self {
        Self {}
    }
}

impl NodeType for UncheckedClassType {
    fn pretty(&self, _identifiers: &Identifiers) -> String {
        "(unchecked-class) ".to_string()
    }
}

#[derive(Debug)]
pub struct ClassType {
    #[allow(unused)]
    functions: Vec<FunctionId>,
}

impl ClassType {
    pub(crate) const fn new(functions: Vec<FunctionId>) -> Self {
        Self { functions }
    }
}

impl NodeType for ClassType {
    fn pretty(&self, _identifiers: &Identifiers) -> String {
        "(class) ".to_string()
    }
}
