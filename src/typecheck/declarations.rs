use crate::ast::Class;
use crate::ast::Declaration;
use crate::ast::Function;
use crate::ast::FunctionKind;
use crate::ast::FunctionPrototype;
use crate::ast::SourceFile;
use crate::typecheck::TypeKind;

use super::Type;
use super::UncheckedFunctionType;

pub(super) struct DeclarationsChecker {}

impl DeclarationsChecker {
    pub(crate) fn check_file(
        &self,
        ast: crate::ast::SourceFile<(), (), ()>,
    ) -> crate::ast::SourceFile<Type, UncheckedFunctionType, ()> {
        let mut declarations = vec![];

        for declaration in ast.declarations {
            declarations.push(self.check_declaration(declaration));
        }

        SourceFile { declarations }
    }

    fn check_declaration(
        &self,
        declaration: Declaration<(), (), ()>,
    ) -> Declaration<Type, UncheckedFunctionType, ()> {
        match declaration {
            Declaration::Class(class) => Declaration::Class(self.check_class(class)),
        }
    }

    fn check_class(&self, class: Class<(), (), ()>) -> Class<Type, UncheckedFunctionType, ()> {
        let mut functions = vec![];

        for function in class.functions {
            functions.push(self.check_function(function));
        }

        Class {
            name: class.name,
            functions,
            type_: Type {
                kind: TypeKind::Class(class.name),
            },
        }
    }

    fn check_function(&self, function: Function<(), ()>) -> Function<UncheckedFunctionType, ()> {
        match function.kind {
            FunctionKind::Implemented {
                statements,
                prototype,
            } => {
                let prototype = self.check_function_prototype(prototype);

                Function {
                    kind: FunctionKind::Implemented {
                        statements: vec![],
                        prototype,
                    },
                    type_: UncheckedFunctionType::Function { statements },
                }
            }
            FunctionKind::Extern {
                external_name,
                prototype,
            } => {
                let name = prototype.name;
                Function {
                    kind: FunctionKind::Extern {
                        external_name: external_name.clone(),
                        prototype: self.check_function_prototype(prototype),
                    },
                    type_: UncheckedFunctionType::ExternalFunction(name, external_name),
                }
            }
        }
    }

    #[allow(clippy::unused_self)]
    const fn check_function_prototype(&self, prototype: FunctionPrototype) -> FunctionPrototype {
        prototype
    }
}
