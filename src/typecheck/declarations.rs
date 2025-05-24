use crate::ast;
use crate::ast::Class;
use crate::ast::Declaration;
use crate::ast::Function;
use crate::ast::SourceFile;
use crate::types::UntypedAst;
use crate::types::class::UncheckedClassType;
use crate::types::function::UncheckedFunctionType;

pub(super) struct DeclarationsChecker {}

impl DeclarationsChecker {
    pub(crate) fn check_file(
        &self,
        ast: UntypedAst,
    ) -> SourceFile<UncheckedClassType, UncheckedFunctionType, Option<ast::TypeConstraint>> {
        let mut declarations = vec![];

        for declaration in ast.declarations {
            declarations.push(self.check_declaration(declaration));
        }

        SourceFile { declarations }
    }

    fn check_declaration(
        &self,
        declaration: Declaration<
            UncheckedClassType,
            UncheckedFunctionType,
            Option<ast::TypeConstraint>,
        >,
    ) -> Declaration<UncheckedClassType, UncheckedFunctionType, Option<ast::TypeConstraint>> {
        match declaration {
            Declaration::Class(class) => Declaration::Class(self.check_class(class)),
        }
    }

    fn check_class(
        &self,
        class: Class<UncheckedClassType, UncheckedFunctionType, Option<ast::TypeConstraint>>,
    ) -> Class<UncheckedClassType, UncheckedFunctionType, Option<ast::TypeConstraint>> {
        let mut functions = vec![];

        for function in class.functions {
            let checked_function = self.check_function(function);
            functions.push(checked_function);
        }

        Class {
            name: class.name,
            functions,
            type_: UncheckedClassType::new(),
        }
    }

    #[allow(clippy::unused_self)]
    const fn check_function(
        &self,
        function: Function<UncheckedFunctionType, Option<ast::TypeConstraint>>,
    ) -> Function<UncheckedFunctionType, Option<ast::TypeConstraint>> {
        function
    }

    pub(crate) const fn new() -> Self {
        Self {}
    }
}
