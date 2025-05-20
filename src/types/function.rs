use std::sync::atomic::AtomicU64;

use crate::{
    ast::{NodeType, Statement, pretty::pretty_print_function_type},
    identifier::Identifiers,
};

use super::expression::ExpressionType;

#[derive(Debug, Clone, Copy)]
#[allow(unused)]
pub struct FunctionId(u64);

#[derive(Debug)]
#[allow(unused)]
pub enum FunctionTypeKind {
    Statements(Vec<Statement<ExpressionType>>),
    External(String),
}

#[derive(Debug)]
#[allow(unused)]
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

    pub(crate) fn into_kind(self) -> FunctionTypeKind {
        self.kind
    }
}

impl NodeType for FunctionType {
    fn pretty(&self, identifiers: &Identifiers) -> String {
        // TODO support returning result here
        pretty_print_function_type(self.id, &self.kind, identifiers).unwrap()
    }
}

#[derive(Debug)]
#[allow(unused)]
pub enum UncheckedFunctionTypeKind {
    Statements(Vec<Statement<()>>),
    External(String),
}

#[derive(Debug)]
pub struct UncheckedFunctionType {
    kind: UncheckedFunctionTypeKind,
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
