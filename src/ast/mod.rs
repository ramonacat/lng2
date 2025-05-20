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
pub struct Class<TClass: NodeType, TFunction: NodeType> {
    pub name: Identifier,
    pub functions: Vec<Function<TFunction>>,
    pub type_: TClass,
}

#[derive(Debug)]
#[allow(unused)]
pub enum ExpressionKind<TExpression: NodeType> {
    Call(Box<Expression<TExpression>>),
    VariableAccess(Identifier),
    FieldAccess(Box<Expression<TExpression>>, Identifier),
}

#[derive(Debug)]
#[allow(unused)]
pub struct Expression<TExpression: NodeType> {
    pub kind: ExpressionKind<TExpression>,
    pub type_: TExpression,
}

#[derive(Debug)]
#[allow(unused)]
pub enum Statement<TExpression: NodeType> {
    Expression(Expression<TExpression>),
}

#[derive(Debug)]
#[allow(unused)]
pub struct FunctionPrototype {
    pub name: Identifier,
}

#[derive(Debug)]
#[allow(unused)]
pub struct Function<TFunction: NodeType> {
    pub prototype: FunctionPrototype,
    pub type_: TFunction,
}

#[derive(Debug)]
#[allow(unused)]
pub struct ExternFunction {}

#[derive(Debug)]
#[allow(unused)]
pub enum Declaration<TClass: NodeType, TFunction: NodeType> {
    Class(Class<TClass, TFunction>),
}

#[derive(Debug)]
#[allow(unused)]
pub struct SourceFile<TClass: NodeType, TFunction: NodeType> {
    pub declarations: Vec<Declaration<TClass, TFunction>>,
}
