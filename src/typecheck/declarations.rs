use crate::{
    ast,
    ast::{Class, Declaration, Function, SourceFile},
    types::{
        UntypedAst,
        class::{ClassId, UncheckedClassType},
        expression::{ExpressionType, ExpressionTypeKind},
        function::{IntermediateFunctionType, UncheckedFunctionType},
    },
};

pub(super) struct DeclarationsChecker {}

impl DeclarationsChecker {
    pub(crate) fn check_file(
        &self,
        ast: UntypedAst,
    ) -> SourceFile<UncheckedClassType, IntermediateFunctionType, ast::TypeConstraint> {
        let mut declarations = vec![];

        for declaration in ast.declarations {
            declarations.push(self.check_declaration(declaration));
        }

        SourceFile { declarations }
    }

    fn check_declaration(
        &self,
        declaration: Declaration<UncheckedClassType, UncheckedFunctionType, ast::TypeConstraint>,
    ) -> Declaration<UncheckedClassType, IntermediateFunctionType, ast::TypeConstraint> {
        match declaration {
            Declaration::Class(class) => Declaration::Class(self.check_class(class)),
        }
    }

    fn check_class(
        &self,
        class: Class<UncheckedClassType, UncheckedFunctionType, ast::TypeConstraint>,
    ) -> Class<UncheckedClassType, IntermediateFunctionType, ast::TypeConstraint> {
        let mut functions = vec![];

        for function in class.functions {
            let checked_function = self.check_function(function, class.type_.id());
            functions.push(checked_function);
        }

        Class {
            name: class.name,
            functions,
            type_: class.type_,
        }
    }

    #[allow(clippy::unused_self)]
    fn check_function(
        &self,
        function: Function<UncheckedFunctionType, ast::TypeConstraint>,
        class_id: ClassId,
    ) -> Function<IntermediateFunctionType, ast::TypeConstraint> {
        let self_ = if function.prototype.static_ {
            None
        } else {
            Some(ExpressionType::new(ExpressionTypeKind::Class(class_id)))
        };

        Function {
            type_: IntermediateFunctionType::new(
                function.type_.into_kind(),
                self_,
                function
                    .prototype
                    .arguments
                    .iter()
                    .map(|x| x.type_)
                    .collect(),
                function.prototype.return_type,
            ),
            prototype: function.prototype,
        }
    }

    pub(crate) const fn new() -> Self {
        Self {}
    }
}
