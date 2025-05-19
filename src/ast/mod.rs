use crate::identifier::Identifier;

#[derive(Debug)]
#[allow(unused)]
pub struct Class {
    pub name: Identifier,
    pub functions: Vec<Function>,
}

#[derive(Debug)]
#[allow(unused)]
pub enum Expression {
    Call(Box<Expression>),
    VariableAccess(Identifier),
    FieldAccess(Box<Expression>, Identifier),
}

#[derive(Debug)]
#[allow(unused)]
pub enum Statement {
    Expression(Expression),
}

#[derive(Debug)]
#[allow(unused)]
pub struct FunctionPrototype {
    pub name: Identifier,
}

#[derive(Debug)]
#[allow(unused)]
pub enum Function {
    Implemented {
        statements: Vec<Statement>,
        prototype: FunctionPrototype,
    },
    Extern {
        external_name: String,
        prototype: FunctionPrototype,
    },
}

#[derive(Debug)]
#[allow(unused)]
pub struct ExternFunction {}

#[derive(Debug)]
#[allow(unused)]
pub enum Declaration {
    Class(Class),
}

#[derive(Debug)]
#[allow(unused)]
pub struct SourceFile {
    pub declarations: Vec<Declaration>,
}
