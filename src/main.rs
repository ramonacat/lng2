#![deny(clippy::all, clippy::pedantic, clippy::nursery, warnings)]
mod ast;
mod codegen;
mod error;
mod identifier;
mod module;
mod parser;
mod stdlib;
mod typecheck;
mod types;

use std::sync::LazyLock;

use ast::pretty::pretty_print;
use codegen::codegen;
use error::pretty_error;
use identifier::Identifiers;
use inkwell::AddressSpace;
use parser::parse;
use typecheck::type_check;
use types::class::ClassIdGenerator;

static ADDRESS_SPACE: LazyLock<AddressSpace> = LazyLock::new(AddressSpace::default);

fn main() {
    let source = "
        class MyClass {
            #[extern(\"println\")]
            fn printline(argument: string): unit;

            fn test(): string {
                return \"this is a return value\";
            }

            fn main(): unit {
                MyClass.printline(\"henlo\");
                MyClass.printline(MyClass.test());
            }
        }
    ";
    let mut identifiers = Identifiers::new();
    let mut class_id_generator = ClassIdGenerator::new();

    let ast = parse(source, &mut identifiers, &mut class_id_generator);

    match ast {
        Ok(ast) => {
            println!(
                "{}",
                pretty_print(&ast, &identifiers).expect("Failed to pretty print untyped AST")
            );

            let checked_ast = type_check(ast, &identifiers);

            println!(
                "{}",
                pretty_print(&checked_ast, &identifiers).expect("Failed to pretty print typed AST")
            );

            codegen(&checked_ast, &identifiers);
        }
        Err(error) => {
            pretty_error(source, error);
            std::process::exit(1);
        }
    }
}
