use crate::{
    ast::{self, Class, Declaration, Expression, ExpressionKind, Function, SourceFile, Statement},
    identifier::Identifiers,
    types::{
        TypedAst, UntypedAst,
        class::{ClassIdGenerator, ClassType, UncheckedClassType},
        expression::{ExpressionType, ExpressionTypeKind},
        function::{
            FunctionIdGenerator, FunctionType, FunctionTypeKind, UncheckedFunctionType,
            UncheckedFunctionTypeKind,
        },
    },
};

pub(super) struct DefinitionsChecker<'ids> {
    function_id_generator: FunctionIdGenerator,
    class_id_generator: ClassIdGenerator,
    identifiers: &'ids Identifiers,
}

impl<'ids> DefinitionsChecker<'ids> {
    pub(crate) fn check_file(&self, ast: UntypedAst) -> TypedAst {
        let mut declarations = vec![];

        for declaration in ast.declarations {
            declarations.push(self.check_declaration(declaration));
        }

        SourceFile { declarations }
    }

    fn check_declaration(
        &self,
        declaration: Declaration<UncheckedClassType, UncheckedFunctionType, ast::TypeConstraint>,
    ) -> Declaration<ClassType, FunctionType, ExpressionType> {
        match declaration {
            Declaration::Class(class) => Declaration::Class(self.check_class(class)),
        }
    }

    fn check_class(
        &self,
        class: Class<UncheckedClassType, UncheckedFunctionType, ast::TypeConstraint>,
    ) -> Class<ClassType, FunctionType, ExpressionType> {
        let mut functions = vec![];

        for function in class.functions {
            let checked_function = self.check_function(function);
            functions.push(checked_function);
        }

        Class {
            name: class.name,
            functions,
            type_: ClassType::new(self.class_id_generator.next()),
        }
    }

    fn check_function(
        &self,
        function: Function<UncheckedFunctionType, ast::TypeConstraint>,
    ) -> Function<FunctionType, ExpressionType> {
        let kind = match function.type_.into_kind() {
            UncheckedFunctionTypeKind::Statements(statements) => FunctionTypeKind::Statements(
                statements
                    .into_iter()
                    .map(|x| self.check_statement(x))
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
        };

        Function {
            type_: FunctionType::new(self.function_id_generator.next(), kind),
            prototype,
        }
    }

    fn check_statement(
        &self,
        statement: Statement<ast::TypeConstraint>,
    ) -> Statement<ExpressionType> {
        match statement {
            Statement::Expression(expression) => {
                Statement::Expression(self.check_expression(expression))
            }
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn check_expression(
        &self,
        expression: crate::ast::Expression<ast::TypeConstraint>,
    ) -> crate::ast::Expression<ExpressionType> {
        match expression.kind {
            crate::ast::ExpressionKind::Call(expression, arguments) => Expression {
                kind: ExpressionKind::Call(
                    Box::new(self.check_expression(*expression)),
                    arguments
                        .into_iter()
                        .map(|x| self.check_expression(x))
                        .collect(),
                ),
                type_: ExpressionType::new(ExpressionTypeKind::Todo),
            },
            crate::ast::ExpressionKind::VariableAccess(identifier) => Expression {
                kind: ExpressionKind::VariableAccess(identifier),
                type_: ExpressionType::new(ExpressionTypeKind::Todo),
            },
            crate::ast::ExpressionKind::FieldAccess(expression, identifier) => Expression {
                kind: ExpressionKind::FieldAccess(
                    Box::new(self.check_expression(*expression)),
                    identifier,
                ),
                type_: ExpressionType::new(ExpressionTypeKind::Todo),
            },
            ExpressionKind::Literal(literal) => Expression {
                kind: ExpressionKind::Literal(self.check_literal(literal)),
                type_: ExpressionType::new(ExpressionTypeKind::Todo),
            },
        }
    }

    pub(crate) const fn new(identifiers: &'ids Identifiers) -> Self {
        Self {
            function_id_generator: FunctionIdGenerator::new(),
            class_id_generator: ClassIdGenerator::new(),
            identifiers,
        }
    }

    #[allow(clippy::unused_self)]
    fn check_literal(&self, literal: ast::Literal) -> ast::Literal {
        match literal {
            ast::Literal::String(string) => ast::Literal::String(string),
        }
    }

    fn type_constraint_to_type(&self, type_constraint: ast::TypeConstraint) -> ExpressionType {
        match type_constraint {
            ast::TypeConstraint::Named(identifier) => match self.identifiers.resolve(identifier) {
                "string" => ExpressionType::new(ExpressionTypeKind::String), // TODO Should this be Object("builtin.string")?
                "unit" => ExpressionType::new(ExpressionTypeKind::Todo),
                _ => todo!(),
            },
            ast::TypeConstraint::Unknown => todo!(),
        }
    }
}
