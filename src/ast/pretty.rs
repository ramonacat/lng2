use std::fmt::Write;

use crate::identifier::Identifiers;

use super::{NodeType, SourceFile};

macro_rules! println_to {
    ($printer:ident, $pattern:literal) => {
        $printer.print_indent()?;
        writeln!($printer.target, $pattern)?;
    };
    ($printer:ident, $pattern:literal, $($argument:expr),+) => {
        $printer.print_indent()?;
        writeln!($printer.target, $pattern, $($argument),+)?;
    };
}

macro_rules! print_to {
    ($printer:ident, $pattern:literal) => {
        $printer.print_indent()?;
        write!($printer.target, $pattern)?;
    };
    ($printer:ident, $pattern:literal, $($argument:expr),+) => {
        $printer.print_indent()?;
        write!($printer.target, $pattern, $($argument),+)?;
    };
}

struct PrettyPrinter<TTarget> {
    indent: usize,
    target: TTarget,
}

impl<TTarget: Write> PrettyPrinter<TTarget> {
    fn print_indent(&mut self) -> std::fmt::Result {
        write!(&mut self.target, "{}", "  ".repeat(self.indent))
    }

    fn indented<TReturn>(&mut self, callback: impl Fn(&mut Self) -> TReturn) -> TReturn {
        self.indent += 1;
        let result = callback(self);
        self.indent -= 1;

        result
    }
}

pub fn pretty_print<T: NodeType, TFunction: NodeType, TInner: NodeType>(
    ast: &SourceFile<T, TFunction, TInner>,
    identifiers: &Identifiers,
) -> Result<String, std::fmt::Error> {
    let mut printer = PrettyPrinter {
        indent: 0,
        target: String::new(),
    };

    println_to!(printer, "{{");

    printer.indented(|printer| {
        for declaration in &ast.declarations {
            print_declaration(printer, identifiers, declaration)?;
        }

        Ok(())
    })?;

    println_to!(printer, "}}");

    Ok(printer.target)
}

fn print_declaration<T: NodeType, TFunction: NodeType, TInner: NodeType>(
    printer: &mut PrettyPrinter<impl Write>,
    identifiers: &Identifiers,
    declaration: &super::Declaration<T, TFunction, TInner>,
) -> std::fmt::Result {
    match declaration {
        super::Declaration::Class(class) => print_class(printer, identifiers, class)?,
    }

    Ok(())
}

fn print_class<T: NodeType, TFunction: NodeType, TInner: NodeType>(
    printer: &mut PrettyPrinter<impl Write>,
    identifiers: &Identifiers,
    class: &super::Class<T, TFunction, TInner>,
) -> std::fmt::Result {
    println_to!(
        printer,
        "{}class {} {{",
        class.type_.pretty(identifiers),
        identifiers.resolve(class.name)
    );

    printer.indented(|printer| {
        for function in &class.functions {
            print_function(printer, identifiers, function)?;
        }

        Ok(())
    })?;

    println_to!(printer, "}}");

    Ok(())
}

fn print_function<T: NodeType, TInner: NodeType>(
    printer: &mut PrettyPrinter<impl Write>,
    identifiers: &Identifiers,
    function: &super::Function<T, TInner>,
) -> std::fmt::Result {
    print_to!(printer, "{}", function.type_.pretty(identifiers));

    match &function.kind {
        // TODO move the prototype to the top-level ast::Function struct
        crate::ast::FunctionKind::Implemented {
            statements,
            prototype,
        } => {
            writeln!(
                printer.target,
                "fn {}() {{",
                identifiers.resolve(prototype.name)
            )?;

            printer.indented(|printer| {
                for statement in statements {
                    print_statement(printer, identifiers, statement)?;
                }

                Ok(())
            })?;

            println_to!(printer, "}}");
        }
        crate::ast::FunctionKind::Extern {
            external_name,
            prototype,
        } => {
            writeln!(
                printer.target,
                "fn {}() = extern({});",
                identifiers.resolve(prototype.name),
                external_name
            )?;
        }
    }

    Ok(())
}

fn print_statement<T: NodeType>(
    printer: &mut PrettyPrinter<impl Write>,
    identifiers: &Identifiers,
    statement: &super::Statement<T>,
) -> std::fmt::Result {
    match statement {
        super::Statement::Expression(expression) => {
            print_to!(printer, "");

            print_expression(printer, identifiers, expression)?;

            writeln!(printer.target, ";")?;
        }
    }

    Ok(())
}

fn print_expression<T: NodeType>(
    printer: &mut PrettyPrinter<impl Write>,
    identifiers: &Identifiers,
    expression: &super::Expression<T>,
) -> std::fmt::Result {
    match &expression.kind {
        super::ExpressionKind::Call(expression) => {
            print_expression(printer, identifiers, expression)?;

            write!(printer.target, "()")?;
        }
        super::ExpressionKind::VariableAccess(identifier) => {
            write!(printer.target, "({})", identifiers.resolve(*identifier))?;
        }
        super::ExpressionKind::FieldAccess(expression, identifier) => {
            write!(printer.target, "(")?;
            print_expression(printer, identifiers, expression)?;
            write!(printer.target, ").{}", identifiers.resolve(*identifier))?;
        }
    }

    Ok(())
}
