#![deny(clippy::all, clippy::pedantic, clippy::nursery, warnings)]
mod ast;
mod codegen;
mod identifier;
mod module;
mod object;
mod parser;
mod typecheck;
mod vector;

use std::sync::LazyLock;

use ast::pretty::pretty_print;
use identifier::Identifiers;
use inkwell::{AddressSpace, context::Context, module::Linkage};
use module::ModuleCompiler;
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

    let context = Context::create();
    let module = context.create_module("main");

    let mut module_compiler = ModuleCompiler::new(&context, module);

    module_compiler.build(|context, module, _| {
        let builder = context.create_builder();
        let type_ = context.i64_type().fn_type(&[], false);
        let function = module.add_function("main", type_, Some(Linkage::External));

        let basic_block = context.append_basic_block(function, "entry");
        builder.position_at_end(basic_block);
        builder
            .build_return(Some(&context.i64_type().const_int(12_456_789, false)))
            .unwrap();
    });
    let object_functions = crate::object::generate_object_functions(&mut module_compiler);
    crate::vector::generate_vector_definition(&mut module_compiler, &object_functions);

    println!("{module_compiler:?}");

    module_compiler.run(|execution_engine| {
        let main =
            unsafe { execution_engine.get_function::<unsafe extern "C" fn() -> u64>("main") }
                .unwrap();
        let result = unsafe { main.call() };

        println!("{result}");
    });
}
