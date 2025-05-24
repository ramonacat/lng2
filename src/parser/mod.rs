use crate::{parser::grammar::Token, types::UntypedAst};
use lalrpop_util::{ParseError, lalrpop_mod};

use crate::identifier::Identifiers;

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
        clippy::unnested_or_patterns,
        clippy::unnecessary_wraps,
        clippy::type_complexity,
    )]
    pub grammar,
    "/parser/grammar.rs"
);

// TODO this should probably return a Result and allow higher-level code to handle printing the
// Errors
pub fn parse<'src>(
    contents: &'src str,
    identifiers: &mut Identifiers,
) -> Result<UntypedAst, ParseError<usize, Token<'src>, &'static str>> {
    grammar::SourceFileParser::new().parse(identifiers, contents)
}
