use crate::{ast::NodeType, identifier::Identifiers};

#[derive(Debug)]
pub enum ExpressionTypeKind {
    Todo,
    String,
}

#[derive(Debug)]
pub struct ExpressionType {
    kind: ExpressionTypeKind,
}
impl ExpressionType {
    pub(crate) const fn new(kind: ExpressionTypeKind) -> Self {
        Self { kind }
    }

    pub(crate) const fn kind(&self) -> &ExpressionTypeKind {
        &self.kind
    }
}

impl NodeType for ExpressionType {
    fn pretty(&self, _identifiers: &Identifiers) -> String {
        let name = match &self.kind {
            ExpressionTypeKind::Todo => "TODO".to_string(),
            ExpressionTypeKind::String => "(builtin.string)".to_string(),
        };

        format!("({name}) ")
    }
}
