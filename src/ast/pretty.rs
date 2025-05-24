use crate::types::function::FunctionId;
use std::fmt::Write;

use crate::{identifier::Identifiers, types::function::FunctionTypeKind};

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

pub fn pretty_print<TClass: NodeType, TFunction: NodeType, TExpression: NodeType>(
    ast: &SourceFile<TClass, TFunction, TExpression>,
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

fn print_declaration<TClass: NodeType, TFunction: NodeType, TExpression: NodeType>(
    printer: &mut PrettyPrinter<impl Write>,
    identifiers: &Identifiers,
    declaration: &super::Declaration<TClass, TFunction, TExpression>,
) -> std::fmt::Result {
    match declaration {
        super::Declaration::Class(class) => print_class(printer, identifiers, class)?,
    }

    Ok(())
}

fn print_class<TClass: NodeType, TFunction: NodeType, TExpression: NodeType>(
    printer: &mut PrettyPrinter<impl Write>,
    identifiers: &Identifiers,
    class: &super::Class<TClass, TFunction, TExpression>,
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

fn print_function<TFunction: NodeType, TExpression: NodeType>(
    printer: &mut PrettyPrinter<impl Write>,
    identifiers: &Identifiers,
    function: &super::Function<TFunction, TExpression>,
) -> std::fmt::Result {
    println_to!(printer, "");

    println_to!(
        printer,
        "fn-named({})",
        identifiers.resolve(function.prototype.name)
    );

    for line in function.type_.pretty(identifiers).lines() {
        println_to!(printer, "{line}");
    }

    Ok(())
}

pub fn pretty_print_function_type(
    id: FunctionId,
    kind: &FunctionTypeKind,
    identifiers: &Identifiers,
) -> Result<String, std::fmt::Error> {
    let mut printer = PrettyPrinter {
        indent: 0,
        target: String::new(),
    };

    print_to!(printer, "fn#{id:?} ");

    match kind {
        FunctionTypeKind::Statements(statements) => {
            writeln!(printer.target, "{{")?;

            printer.indented(|printer| {
                for statement in statements {
                    print_statement(printer, identifiers, statement)?;
                }

                Ok(())
            })?;

            println_to!(printer, "}}");
        }
        FunctionTypeKind::External(external_name) => {
            writeln!(printer.target, "external(\"{external_name}\")")?;
        }
    }

    Ok(printer.target)
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
        super::ExpressionKind::Call(expression, arguments) => {
            print_expression(printer, identifiers, expression)?;

            write!(printer.target, "(")?;

            for argument in arguments {
                print_expression(printer, identifiers, argument)?;
                write!(printer.target, ",")?;
            }

            write!(printer.target, ")")?;
        }
        super::ExpressionKind::VariableAccess(identifier) => {
            write!(printer.target, "({})", identifiers.resolve(*identifier))?;
        }
        super::ExpressionKind::FieldAccess(expression, identifier) => {
            write!(printer.target, "(")?;
            print_expression(printer, identifiers, expression)?;
            write!(printer.target, ").{}", identifiers.resolve(*identifier))?;
        }
        super::ExpressionKind::Literal(literal) => {
            print_literal(printer, literal)?;
        }
    }

    Ok(())
}

fn print_literal(
    printer: &mut PrettyPrinter<impl Write>,
    literal: &super::Literal,
) -> std::fmt::Result {
    match literal {
        super::Literal::String(string) => {
            write!(printer.target, "\"{string}\"")
        }
    }
}
