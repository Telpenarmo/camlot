use crate::{
    grammar::type_expr,
    parser::{CompletedMarker, Parser},
    token_set::TokenSet,
    SyntaxKind,
};

const ATOMIC_PATTERN: TokenSet = TokenSet::new(&[SyntaxKind::IDENT, SyntaxKind::UNDERSCORE]);

pub(super) const PARAM_START: TokenSet =
    TokenSet::new(&[SyntaxKind::L_PAREN]).union(ATOMIC_PATTERN);

pub(crate) fn params(parser: &mut Parser) -> CompletedMarker {
    let mark = parser.open();
    while parser.at_any(PARAM_START) {
        param(parser);
    }
    parser.close(mark, SyntaxKind::PARAMS)
}

fn param(parser: &mut Parser) -> CompletedMarker {
    let mut mark = parser.open();

    if parser.at_any(ATOMIC_PATTERN) {
        pattern(parser);
    } else if parser.eat(SyntaxKind::L_PAREN) {
        if parser.eat(SyntaxKind::R_PAREN) {
            let pattern_marker = parser.close(mark, SyntaxKind::UNIT_PATTERN);
            mark = parser.open_before(pattern_marker);
        } else {
            pattern(parser);
            let mark = parser.open();
            parser.expect(SyntaxKind::COLON);
            type_expr::type_expr(parser);
            parser.close(mark, SyntaxKind::TYPE_ANNOTATION);
            if !parser.eat(SyntaxKind::R_PAREN) {
                parser
                    .eat_error_until(TokenSet::new(&[SyntaxKind::R_PAREN]), "Expected ')'".into());
                parser.eat(SyntaxKind::R_PAREN);
            }
        }
    } else {
        unreachable!();
    }
    parser.close(mark, SyntaxKind::PARAM)
}

pub(crate) fn pattern(parser: &mut Parser) {
    if parser.at_any(PARAM_START) {
        let marker = parser.open();

        if parser.eat(SyntaxKind::UNDERSCORE) {
            parser.close(marker, SyntaxKind::UNDERSCORE_PATTERN);
        } else if parser.eat(SyntaxKind::IDENT) {
            parser.close(marker, SyntaxKind::IDENT_PATTERN);
        } else if parser.eat(SyntaxKind::L_PAREN) {
            parser.expect(SyntaxKind::R_PAREN);
            parser.close(marker, SyntaxKind::UNIT_PATTERN);
        } else {
            unreachable!("Unhandled pattern");
        }
    } else {
        parser.error("Expected pattern".into());
    }
}
