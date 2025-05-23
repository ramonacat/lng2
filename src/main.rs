#![deny(clippy::all, clippy::pedantic, clippy::nursery, warnings)]
mod ast;
mod codegen;
mod identifier;
mod module;
mod object;
mod parser;
mod typecheck;
mod types;

use std::sync::LazyLock;

use ast::pretty::pretty_print;
use codegen::codegen;
use identifier::Identifiers;
use inkwell::AddressSpace;
use parser::parse;
use typecheck::type_check;

static ADDRESS_SPACE: LazyLock<AddressSpace> = LazyLock::new(AddressSpace::default);

fn main() {
    let mut identifiers = Identifiers::new();

    let ast = parse(
        "
            class MyClass {
                #[extern(\"println\")]
                fn printline();

                fn main() {
                    MyClass.printline();
                }
            }
        ",
        &mut identifiers,
    )
    .unwrap();

    println!(
        "{}",
        pretty_print(&ast, &identifiers).expect("Failed to pretty print untyped AST")
    );

    let checked_ast = type_check(ast);

    println!(
        "{}",
        pretty_print(&checked_ast, &identifiers).expect("Failed to pretty print typed AST")
    );

    codegen(&checked_ast, &identifiers);
}
