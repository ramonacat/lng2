use std::fmt::Write;
use std::sync::atomic::AtomicU64;

use crate::{
    ast::{self, NodeType, Statement, pretty::pretty_print_function_type},
    identifier::Identifiers,
};

use super::expression::ExpressionType;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionId(u64);

impl FunctionId {
    pub const fn into_u64(self) -> u64 {
        self.0
    }
}

#[derive(Debug)]
pub enum FunctionTypeKind {
    Statements(Vec<Statement<ExpressionType>>),
    External(String),
}

#[derive(Debug)]
pub struct FunctionType {
    id: FunctionId,
    kind: FunctionTypeKind,
}

impl FunctionType {
    pub(crate) const fn new(id: FunctionId, kind: FunctionTypeKind) -> Self {
        Self { id, kind }
    }

    pub(crate) const fn id(&self) -> FunctionId {
        self.id
    }

    pub(crate) const fn as_kind(&self) -> &FunctionTypeKind {
        &self.kind
    }
}

impl NodeType for FunctionType {
    fn pretty(&self, identifiers: &Identifiers) -> String {
        pretty_print_function_type(self.id, &self.kind, identifiers).unwrap()
    }
}

#[derive(Debug)]
pub enum UncheckedFunctionTypeKind {
    Statements(Vec<Statement<ast::TypeConstraint>>),
    External(String),
}

#[derive(Debug)]
pub struct UncheckedFunctionType {
    kind: UncheckedFunctionTypeKind,
}

#[derive(Debug)]
pub struct IntermediateFunctionType {
    kind: UncheckedFunctionTypeKind,
    self_: Option<ExpressionType>,
    arguments: Vec<ast::TypeConstraint>,
    return_type: ast::TypeConstraint,
}
impl IntermediateFunctionType {
    pub(crate) const fn new(
        kind: UncheckedFunctionTypeKind,
        self_: Option<ExpressionType>,
        arguments: Vec<ast::TypeConstraint>,
        return_type: ast::TypeConstraint,
    ) -> Self {
        Self {
            kind,
            self_,
            arguments,
            return_type,
        }
    }

    pub(crate) fn into_kind(self) -> UncheckedFunctionTypeKind {
        self.kind
    }
}

impl NodeType for IntermediateFunctionType {
    fn pretty(&self, identifiers: &Identifiers) -> String {
        let mut result = "fn(".to_string();

        if let Some(self_) = &self.self_ {
            result += "self: ";
            result += &self_.pretty(identifiers);
            result += ", ";
        }

        for argument in &self.arguments {
            result += &argument.pretty(identifiers);
            result += ", ";
        }

        result += "): ";

        result += &self.return_type.pretty(identifiers);

        match &self.kind {
            UncheckedFunctionTypeKind::Statements(_) => write!(result, "(statements);"),
            UncheckedFunctionTypeKind::External(external_name) => {
                write!(result, "(external({external_name}))")
            }
        }
        .unwrap();

        result
    }
}

impl UncheckedFunctionType {
    pub(crate) const fn new(kind: UncheckedFunctionTypeKind) -> Self {
        Self { kind }
    }

    pub(crate) fn into_kind(self) -> UncheckedFunctionTypeKind {
        self.kind
    }
}

impl NodeType for UncheckedFunctionType {
    fn pretty(&self, _identifiers: &Identifiers) -> String {
        "(unchecked-function) ".to_string()
    }
}

pub struct FunctionIdGenerator {
    current: AtomicU64,
}
impl FunctionIdGenerator {
    pub(crate) fn next(&self) -> FunctionId {
        FunctionId(
            self.current
                .fetch_add(1, std::sync::atomic::Ordering::AcqRel),
        )
    }

    pub(crate) const fn new() -> Self {
        Self {
            current: AtomicU64::new(0),
        }
    }
}
