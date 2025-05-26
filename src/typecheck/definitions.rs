use crate::{
    ast::{self, Class, Declaration, Expression, ExpressionKind, Function, SourceFile, Statement},
    identifier::Identifiers,
    types::{
        TypedAst,
        class::{ClassType, UncheckedClassType},
        expression::{ExpressionType, ExpressionTypeKind},
        function::{
            FunctionIdGenerator, FunctionType, FunctionTypeKind, IntermediateFunctionType,
            UncheckedFunctionTypeKind,
        },
    },
};

use super::scope::{IntermediateClassType, IntermediateFieldType, Scope};

pub(super) struct DefinitionsChecker<'ids> {
    function_id_generator: FunctionIdGenerator,
    identifiers: &'ids Identifiers,
}

impl<'ids> DefinitionsChecker<'ids> {
    pub(crate) fn check_file(
        &self,
        ast: SourceFile<UncheckedClassType, IntermediateFunctionType, ast::TypeConstraint>,
    ) -> TypedAst {
        let mut declarations = vec![];
        let mut scope = Scope::new();

        for declaration in &ast.declarations {
            match declaration {
                Declaration::Class(class) => {
                    scope.add(
                        class.name,
                        IntermediateClassType::new(
                            class.type_.id(),
                            class
                                .functions
                                .iter()
                                .map(|x| {
                                    (
                                        x.prototype.name,
                                        IntermediateFieldType::Callable {
                                            arguments: x
                                                .prototype
                                                .arguments
                                                .iter()
                                                .map(|x| x.type_)
                                                .collect(),
                                            return_type: x.prototype.return_type,
                                        },
                                    )
                                })
                                .collect(),
                        ),
                    );
                }
            }
        }

        for declaration in ast.declarations {
            let value = self.check_declaration(declaration, scope.into_child());
            declarations.push(value.0);
            scope = value.1;
        }

        SourceFile { declarations }
    }

    fn check_declaration(
        &self,
        declaration: Declaration<UncheckedClassType, IntermediateFunctionType, ast::TypeConstraint>,
        scope: Scope,
    ) -> (Declaration<ClassType, FunctionType, ExpressionType>, Scope) {
        match declaration {
            Declaration::Class(class) => {
                let check_result = self.check_class(class, scope);

                (Declaration::Class(check_result.0), check_result.1)
            }
        }
    }

    fn check_class(
        &self,
        class: Class<UncheckedClassType, IntermediateFunctionType, ast::TypeConstraint>,
        mut scope: Scope,
    ) -> (Class<ClassType, FunctionType, ExpressionType>, Scope) {
        let mut functions = vec![];

        for function in class.functions {
            let checked_function = self.check_function(function, scope.into_child());

            functions.push(checked_function.0);
            scope = checked_function.1;
        }

        (
            Class {
                name: class.name,
                functions,
                type_: ClassType::new(class.type_.id()),
            },
            scope,
        )
    }

    fn check_function(
        &self,
        function: Function<IntermediateFunctionType, ast::TypeConstraint>,
        scope: Scope,
    ) -> (Function<FunctionType, ExpressionType>, Scope) {
        let kind = match function.type_.into_kind() {
            UncheckedFunctionTypeKind::Statements(statements) => FunctionTypeKind::Statements(
                statements
                    .into_iter()
                    .map(|x| self.check_statement(x, &scope))
                    .collect(),
            ),
            UncheckedFunctionTypeKind::External(external_name) => {
                FunctionTypeKind::External(external_name)
            }
        };

        let prototype = ast::FunctionPrototype {
            name: function.prototype.name,
            arguments: function
                .prototype
                .arguments
                .into_iter()
                .map(|x| ast::FunctionArgument {
                    name: x.name,
                    type_: self.type_constraint_to_type(x.type_),
                })
                .collect(),
            return_type: self.type_constraint_to_type(function.prototype.return_type),
            static_: function.prototype.static_,
        };

        (
            Function {
                type_: FunctionType::new(self.function_id_generator.next(), kind),
                prototype,
            },
            scope.into_parent().unwrap(),
        )
    }

    fn check_statement(
        &self,
        statement: Statement<ast::TypeConstraint>,
        scope: &Scope,
    ) -> Statement<ExpressionType> {
        match statement {
            Statement::Expression(expression) => {
                Statement::Expression(self.check_expression(expression, scope))
            }
            Statement::Return(expression) => {
                Statement::Return(self.check_expression(expression, scope))
            }
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn check_expression(
        &self,
        expression: crate::ast::Expression<ast::TypeConstraint>,
        scope: &Scope,
    ) -> crate::ast::Expression<ExpressionType> {
        match expression.kind {
            crate::ast::ExpressionKind::Call(expression, arguments) => {
                let call_target = self.check_expression(*expression, scope);
                Expression {
                    type_: match &call_target.type_.kind() {
                        ExpressionTypeKind::String => todo!(),
                        ExpressionTypeKind::Class(_) => todo!(),
                        ExpressionTypeKind::Unit => todo!(),
                        ExpressionTypeKind::Callable {
                            arguments: _,
                            return_type,
                        } => *return_type.clone(),
                    },
                    kind: ExpressionKind::Call(
                        Box::new(call_target),
                        arguments
                            .into_iter()
                            .map(|x| self.check_expression(x, scope))
                            .collect(),
                    ),
                }
            }
            crate::ast::ExpressionKind::VariableAccess(identifier) => Expression {
                kind: ExpressionKind::VariableAccess(identifier),
                type_: scope.get(identifier).unwrap().to_expression_type(),
            },
            crate::ast::ExpressionKind::FieldAccess(expression, identifier) => {
                let checked_expression = self.check_expression(*expression, scope);
                let type_ = match &checked_expression.type_.kind() {
                    ExpressionTypeKind::String => todo!(),
                    ExpressionTypeKind::Class(class_id) => {
                        let class_type = scope.get_class(*class_id).unwrap();
                        let field = class_type.get_field(identifier).unwrap();
                        let (arguments, return_type) = match field {
                            IntermediateFieldType::Callable {
                                arguments,
                                return_type,
                            } => (
                                arguments
                                    .iter()
                                    .map(|x| self.type_constraint_to_type(*x))
                                    .collect(),
                                self.type_constraint_to_type(*return_type),
                            ),
                        };

                        ExpressionType::new(ExpressionTypeKind::Callable {
                            arguments,
                            return_type: Box::new(return_type),
                        })
                    }
                    ExpressionTypeKind::Unit => todo!(),
                    ExpressionTypeKind::Callable { .. } => todo!(),
                };

                Expression {
                    type_,
                    kind: ExpressionKind::FieldAccess(Box::new(checked_expression), identifier),
                }
            }
            ExpressionKind::Literal(literal) => self.check_literal(literal),
        }
    }

    pub(crate) const fn new(identifiers: &'ids Identifiers) -> Self {
        Self {
            function_id_generator: FunctionIdGenerator::new(),
            identifiers,
        }
    }

    #[allow(clippy::unused_self)]
    fn check_literal(&self, literal: ast::Literal) -> Expression<ExpressionType> {
        match literal {
            ast::Literal::String(string) => Expression {
                kind: ExpressionKind::Literal(ast::Literal::String(string)),
                type_: ExpressionType::new(ExpressionTypeKind::String),
            },
        }
    }

    fn type_constraint_to_type(&self, type_constraint: ast::TypeConstraint) -> ExpressionType {
        match type_constraint {
            ast::TypeConstraint::Named(identifier) => match self.identifiers.resolve(identifier) {
                "string" => ExpressionType::new(ExpressionTypeKind::String), // TODO Should this be Object("builtin.string")?
                "unit" => ExpressionType::new(ExpressionTypeKind::Unit),
                _ => todo!(),
            },
            ast::TypeConstraint::Unknown => todo!(),
        }
    }
}
