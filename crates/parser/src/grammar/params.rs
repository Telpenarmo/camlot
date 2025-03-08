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
    assert!(parser.at_any(PARAM_START));

    let mut mark = parser.open();

    if parser.at_any(ATOMIC_PATTERN) {
        pattern(parser);
    } else {
        assert!(parser.eat(SyntaxKind::L_PAREN));
        if parser.eat(SyntaxKind::R_PAREN) {
            // unit pattern
            let pattern_marker = parser.close(mark, SyntaxKind::UNIT_PATTERN);
            mark = parser.open_before(pattern_marker);
        } else {
            // type annotated param
            pattern(parser);
            if !parser.at(SyntaxKind::COLON) {
                parser.error("Expected ':'".into());
                return parser.close(mark, SyntaxKind::PARAM);
            }
            let mark = parser.open();
            assert!(parser.eat(SyntaxKind::COLON));
            type_expr::type_expr(parser);
            parser.close(mark, SyntaxKind::TYPE_ANNOTATION);
            if !parser.eat(SyntaxKind::R_PAREN) {
                let delimiters =
                    TokenSet::new(&[SyntaxKind::R_PAREN, SyntaxKind::EQUAL, SyntaxKind::L_BRACE]);
                parser.eat_error_until(delimiters, "Expected ')'".into());
                parser.eat(SyntaxKind::R_PAREN);
            }
        }
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

pub(crate) fn type_params(parser: &mut Parser) -> CompletedMarker {
    let mark = parser.open();
    while parser.at(SyntaxKind::APOSTROPHE) {
        let mark = parser.open();
        parser.advance();
        parser.expect(SyntaxKind::IDENT);
        parser.close(mark, SyntaxKind::TYPE_PARAM);
    }
    parser.close(mark, SyntaxKind::TYPE_PARAMS)
}
