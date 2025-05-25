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

impl NodeType for TypeConstraint {
    fn pretty(&self, identifiers: &Identifiers) -> String {
        match self {
            Self::Named(identifier) => identifiers.resolve(*identifier).to_string(),
            Self::Unknown => "unknown".to_string(),
        }
    }
}

#[derive(Debug)]
pub struct Class<TClass: NodeType, TFunction: NodeType, TExpression: NodeType> {
    pub name: Identifier,
    pub functions: Vec<Function<TFunction, TExpression>>,
    pub type_: TClass,
}

#[derive(Debug)]
pub enum Literal {
    String(String),
}

#[derive(Debug)]
pub enum ExpressionKind<TExpression: NodeType> {
    Call(Box<Expression<TExpression>>, Vec<Expression<TExpression>>),
    VariableAccess(Identifier),
    FieldAccess(Box<Expression<TExpression>>, Identifier),
    Literal(Literal),
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
    Return(Expression<TExpression>),
}

#[derive(Debug, Clone, Copy)]
pub enum TypeConstraint {
    Named(Identifier),
    Unknown,
}

#[derive(Debug)]
#[allow(unused)]
pub struct FunctionArgument<TExpression: NodeType> {
    pub name: Identifier,
    pub type_: TExpression,
}

#[derive(Debug)]
pub struct FunctionPrototype<TExpression: NodeType> {
    pub name: Identifier,
    pub arguments: Vec<FunctionArgument<TExpression>>,
    pub return_type: TExpression,
}

#[derive(Debug)]
pub struct Function<TFunction: NodeType, TExpression: NodeType> {
    pub prototype: FunctionPrototype<TExpression>,
    pub type_: TFunction,
}

#[derive(Debug)]
pub enum Declaration<TClass: NodeType, TFunction: NodeType, TExpression: NodeType> {
    Class(Class<TClass, TFunction, TExpression>),
}

#[derive(Debug)]
pub struct SourceFile<TClass: NodeType, TFunction: NodeType, TExpression: NodeType> {
    pub declarations: Vec<Declaration<TClass, TFunction, TExpression>>,
}
