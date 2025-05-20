use crate::identifier::{Identifier, Identifiers};

pub mod pretty;

pub trait NodeType: std::fmt::Debug {
    fn pretty(&self, identifiers: &Identifiers) -> String;
}

impl NodeType for () {
    fn pretty(&self, _identifiers: &Identifiers) -> String {
        String::new()
    }
}

#[derive(Debug)]
#[allow(unused)]
pub struct Class<T: NodeType, TFunction: NodeType, TInner: NodeType> {
    pub name: Identifier,
    pub functions: Vec<Function<TFunction, TInner>>,
    pub type_: T,
}

#[derive(Debug)]
#[allow(unused)]
pub enum ExpressionKind<T: NodeType> {
    Call(Box<Expression<T>>),
    VariableAccess(Identifier),
    FieldAccess(Box<Expression<T>>, Identifier),
}

#[derive(Debug)]
#[allow(unused)]
pub struct Expression<T: NodeType> {
    pub kind: ExpressionKind<T>,
    pub type_: T,
}

#[derive(Debug)]
#[allow(unused)]
pub enum Statement<T: NodeType> {
    Expression(Expression<T>),
}

#[derive(Debug)]
#[allow(unused)]
pub struct FunctionPrototype {
    pub name: Identifier,
}

#[derive(Debug)]
#[allow(unused)]
pub enum FunctionKind<T: NodeType> {
    Implemented {
        statements: Vec<Statement<T>>,
        prototype: FunctionPrototype,
    },
    Extern {
        external_name: String,
        prototype: FunctionPrototype,
    },
}

#[derive(Debug)]
#[allow(unused)]
pub struct Function<T: NodeType, TInside: NodeType> {
    pub kind: FunctionKind<TInside>,
    pub type_: T,
}

#[derive(Debug)]
#[allow(unused)]
pub struct ExternFunction {}

#[derive(Debug)]
#[allow(unused)]
pub enum Declaration<T: NodeType, TFunction: NodeType, TInner: NodeType> {
    Class(Class<T, TFunction, TInner>),
}

#[derive(Debug)]
#[allow(unused)]
pub struct SourceFile<T: NodeType, TFunction: NodeType, TInner: NodeType> {
    pub declarations: Vec<Declaration<T, TFunction, TInner>>,
}
