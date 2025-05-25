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

static ADDRESS_SPACE: LazyLock<AddressSpace> = LazyLock::new(AddressSpace::default);

fn main() {
    let mut identifiers = Identifiers::new();

    let source = "
        class MyClass {
            #[extern(\"println\")]
            fn printline(argument: string): unit;

            fn main(): unit {
                MyClass.printline(\"henlo\");
            }
        }
    ";
    let ast = parse(source, &mut identifiers);

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
