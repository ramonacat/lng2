use crate::{
    ast::{Class, Declaration, Expression, ExpressionKind, Function, SourceFile, Statement},
    types::{
        class::{ClassType, UncheckedClassType},
        expression::{ExpressionType, ExpressionTypeKind},
        function::{
            FunctionIdGenerator, FunctionType, FunctionTypeKind, UncheckedFunctionType,
            UncheckedFunctionTypeKind,
        },
    },
};

pub(super) struct DefinitionsChecker {
    function_id_generator: FunctionIdGenerator,
}

impl DefinitionsChecker {
    pub(crate) fn check_file(
        &self,
        ast: SourceFile<UncheckedClassType, UncheckedFunctionType>,
    ) -> SourceFile<ClassType, FunctionType> {
        let mut declarations = vec![];

        for declaration in ast.declarations {
            declarations.push(self.check_declaration(declaration));
        }

        SourceFile { declarations }
    }

    fn check_declaration(
        &self,
        declaration: Declaration<UncheckedClassType, UncheckedFunctionType>,
    ) -> Declaration<ClassType, FunctionType> {
        match declaration {
            Declaration::Class(class) => Declaration::Class(self.check_class(class)),
        }
    }

    fn check_class(
        &self,
        class: Class<UncheckedClassType, UncheckedFunctionType>,
    ) -> Class<ClassType, FunctionType> {
        let mut functions = vec![];
        let mut function_ids = vec![];

        for function in class.functions {
            let checked_function = self.check_function(function);
            function_ids.push(checked_function.type_.id());

            functions.push(checked_function);
        }

        Class {
            name: class.name,
            functions,
            type_: ClassType::new(function_ids),
        }
    }

    fn check_function(&self, function: Function<UncheckedFunctionType>) -> Function<FunctionType> {
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

        Function {
            type_: FunctionType::new(self.function_id_generator.next(), kind),
            prototype: function.prototype,
        }
    }

    fn check_statement(&self, statement: Statement<()>) -> Statement<ExpressionType> {
        match statement {
            Statement::Expression(expression) => {
                Statement::Expression(self.check_expression(expression))
            }
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn check_expression(
        &self,
        expression: crate::ast::Expression<()>,
    ) -> crate::ast::Expression<ExpressionType> {
        match expression.kind {
            crate::ast::ExpressionKind::Call(expression) => Expression {
                kind: ExpressionKind::Call(Box::new(self.check_expression(*expression))),
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
        }
    }

    pub(crate) const fn new() -> Self {
        Self {
            function_id_generator: FunctionIdGenerator::new(),
        }
    }
}
