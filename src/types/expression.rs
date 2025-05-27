use std::fmt::Write;

use crate::{ast::NodeType, identifier::Identifiers, types::class::ClassId};

#[derive(Clone, Debug)]
pub enum ExpressionTypeKind {
    String,
    Class(ClassId),
    Unit,
    Callable {
        arguments: Vec<ExpressionType>,
        return_type: Box<ExpressionType>,
    },
}

#[derive(Clone, Debug)]
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
    #[allow(clippy::only_used_in_recursion)]
    fn pretty(&self, identifiers: &Identifiers) -> String {
        let name = match &self.kind {
            ExpressionTypeKind::String => "(builtin.string)".to_string(),
            ExpressionTypeKind::Class(class_id) => format!("(class.{})", class_id.into_u64()),
            ExpressionTypeKind::Unit => "unit".to_string(),
            ExpressionTypeKind::Callable {
                arguments,
                return_type,
            } => {
                let arguments = arguments.iter().map(|x| x.pretty(identifiers));
                let mut formatted_arguments = String::new();
                for argument in arguments {
                    write!(formatted_arguments, "{argument},").unwrap();
                }

                format!(
                    "fn({}): {}",
                    formatted_arguments,
                    return_type.pretty(identifiers)
                )
            }
        };

        format!("({name}) ")
    }
}
