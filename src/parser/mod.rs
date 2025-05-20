use lalrpop_util::{ParseError, lalrpop_mod};

use crate::{ast, identifier::Identifiers};

lalrpop_mod!(
    #[allow(
        clippy::redundant_pub_crate,
        clippy::unicode_not_nfc,
        clippy::elidable_lifetime_names,
        clippy::uninlined_format_args,
        clippy::no_effect_underscore_binding,
        clippy::cast_sign_loss,
        clippy::option_if_let_else,
        clippy::use_self,
        clippy::missing_const_for_fn,
        clippy::needless_pass_by_ref_mut,
        clippy::trivially_copy_pass_by_ref,
        clippy::cloned_instead_of_copied,
        clippy::too_many_lines,
        clippy::match_same_arms,
        clippy::unnested_or_patterns
    )]
    grammar
);

// TODO this should probably return a Result and allow higher-level code to handle printing the
// Errors
pub fn parse(contents: &str, identifiers: &mut Identifiers) -> Option<ast::SourceFile<(), (), ()>> {
    match grammar::SourceFileParser::new().parse(identifiers, contents) {
        Ok(ast) => Some(ast),
        Err(e) => {
            pretty_error(contents, e);

            None
        }
    }
}

fn pretty_error(contents: &str, error: ParseError<usize, grammar::Token<'_>, &str>) {
    match error {
        ParseError::InvalidToken { location } => {
            print_with_highlight(contents, "invalid token", location);
        }
        ParseError::UnrecognizedEof { location, expected } => print_with_highlight(
            contents,
            &format!("unrecognized EOF, expected one of: {}", expected.join(", ")),
            location,
        ),
        // TODO the tokens have ranges, support highlighting them!
        ParseError::UnrecognizedToken { token, expected } => print_with_highlight(
            contents,
            &format!(
                "unrecognized token \"{}\", expected one of: {}",
                token.1,
                expected.join(", ")
            ),
            token.0,
        ),
        ParseError::ExtraToken { token } => print_with_highlight(
            contents,
            &format!("unexpected token \"{}\"", token.1),
            token.0,
        ),
        ParseError::User { error: _ } => todo!(),
    }
}

fn print_with_highlight(contents: &str, message: &str, highlight: usize) {
    // TODO what are the actual units here? codepoints?
    let mut printed_codepoints = 0;
    for line in contents.lines() {
        println!("{line}");

        if printed_codepoints + line.len() > highlight {
            print!("{}^", " ".repeat(highlight - printed_codepoints));
            println!("{message}");
        }

        // TODO the 1 here is for line endings, but we have to consider the case of \r\n as well
        printed_codepoints += line.len() + 1;
    }
}
