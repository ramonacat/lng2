use crate::{ast::NodeType, identifier::Identifiers};

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
pub struct ClassType {}

impl ClassType {
    pub(crate) const fn new() -> Self {
        Self {}
    }
}

impl NodeType for ClassType {
    fn pretty(&self, _identifiers: &Identifiers) -> String {
        "(class) ".to_string()
    }
}
