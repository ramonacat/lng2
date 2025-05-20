use crate::ast::{
    Class, Declaration, Expression, ExpressionKind, Function, FunctionKind, SourceFile, Statement,
};

use super::{Type, TypeKind, UncheckedFunctionType};

pub(super) struct DefinitionsChecker {}

impl DefinitionsChecker {
    pub(crate) fn check_file(
        &self,
        ast: SourceFile<Type, UncheckedFunctionType, ()>,
    ) -> SourceFile<Type, Type, Type> {
        let mut declarations = vec![];

        for declaration in ast.declarations {
            declarations.push(self.check_declaration(declaration));
        }

        SourceFile { declarations }
    }

    fn check_declaration(
        &self,
        declaration: Declaration<Type, UncheckedFunctionType, ()>,
    ) -> Declaration<Type, Type, Type> {
        match declaration {
            Declaration::Class(class) => Declaration::Class(self.check_class(class)),
        }
    }

    fn check_class(
        &self,
        class: Class<Type, UncheckedFunctionType, ()>,
    ) -> Class<Type, Type, Type> {
        let name = class.name;

        let mut functions = vec![];

        for function in class.functions {
            functions.push(self.check_function(function));
        }

        Class {
            name: class.name,
            functions,
            type_: Type {
                kind: TypeKind::Class(name),
            },
        }
    }

    fn check_function(
        &self,
        function: Function<UncheckedFunctionType, ()>,
    ) -> Function<Type, Type> {
        match function.kind {
            FunctionKind::Implemented {
                statements: _,
                prototype,
            } => {
                let name = prototype.name;

                let statements = match function.type_ {
                    UncheckedFunctionType::Function { statements } => statements,
                    UncheckedFunctionType::ExternalFunction(_, _) => todo!(),
                };

                let statements = statements
                    .into_iter()
                    .map(|x| self.check_statement(x))
                    .collect();

                Function {
                    kind: FunctionKind::Implemented {
                        statements,
                        prototype,
                    },
                    type_: Type {
                        kind: TypeKind::Function(name),
                    },
                }
            }
            FunctionKind::Extern {
                external_name: kind_external_name,
                prototype,
            } => {
                // TODO can we avoid panicing here with the type system?
                let UncheckedFunctionType::ExternalFunction(identifier, external_name) =
                    function.type_
                else {
                    todo!();
                };

                Function {
                    kind: FunctionKind::Extern {
                        external_name: kind_external_name,
                        prototype,
                    },
                    type_: Type {
                        kind: TypeKind::ExternalFunction(identifier, external_name),
                    },
                }
            }
        }
    }

    fn check_statement(&self, statement: Statement<()>) -> Statement<Type> {
        match statement {
            Statement::Expression(expression) => {
                Statement::Expression(self.check_expression(expression))
            }
        }
    }

    fn check_expression(
        &self,
        expression: crate::ast::Expression<()>,
    ) -> crate::ast::Expression<Type> {
        match expression.kind {
            crate::ast::ExpressionKind::Call(expression) => Expression {
                kind: ExpressionKind::Call(Box::new(self.check_expression(*expression))),
                type_: Type {
                    kind: TypeKind::Todo,
                },
            },
            crate::ast::ExpressionKind::VariableAccess(identifier) => Expression {
                kind: ExpressionKind::VariableAccess(identifier),
                type_: Type {
                    kind: TypeKind::Todo,
                },
            },
            crate::ast::ExpressionKind::FieldAccess(expression, identifier) => Expression {
                kind: ExpressionKind::FieldAccess(
                    Box::new(self.check_expression(*expression)),
                    identifier,
                ),
                type_: Type {
                    kind: TypeKind::Todo,
                },
            },
        }
    }
}
