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
pub struct Class<TClass: NodeType, TFunction: NodeType> {
    pub name: Identifier,
    pub functions: Vec<Function<TFunction>>,
    pub type_: TClass,
}

#[derive(Debug)]
pub enum ExpressionKind<TExpression: NodeType> {
    Call(Box<Expression<TExpression>>),
    VariableAccess(Identifier),
    FieldAccess(Box<Expression<TExpression>>, Identifier),
}

#[derive(Debug)]
pub struct Expression<TExpression: NodeType> {
    pub kind: ExpressionKind<TExpression>,
    // TODO This will most likely be needed once there are like assignments and stuff
    #[allow(unused)]
    pub type_: TExpression,
}

#[derive(Debug)]
pub enum Statement<TExpression: NodeType> {
    Expression(Expression<TExpression>),
}

#[derive(Debug)]
pub struct FunctionPrototype {
    pub name: Identifier,
}

#[derive(Debug)]
pub struct Function<TFunction: NodeType> {
    pub prototype: FunctionPrototype,
    pub type_: TFunction,
}

#[derive(Debug)]
pub enum Declaration<TClass: NodeType, TFunction: NodeType> {
    Class(Class<TClass, TFunction>),
}

#[derive(Debug)]
pub struct SourceFile<TClass: NodeType, TFunction: NodeType> {
    pub declarations: Vec<Declaration<TClass, TFunction>>,
}
