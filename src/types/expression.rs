use crate::{ast::NodeType, identifier::Identifiers};

#[derive(Debug)]
pub enum ExpressionTypeKind {
    Todo,
}

#[derive(Debug)]
pub struct ExpressionType {
    kind: ExpressionTypeKind,
}
impl ExpressionType {
    pub(crate) const fn new(kind: ExpressionTypeKind) -> Self {
        Self { kind }
    }
}

impl NodeType for ExpressionType {
    fn pretty(&self, _identifiers: &Identifiers) -> String {
        let name = match &self.kind {
            ExpressionTypeKind::Todo => "TODO".to_string(),
        };

        format!("({name}) ")
    }
}
