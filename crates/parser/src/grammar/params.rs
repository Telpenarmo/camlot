use crate::{
    grammar::type_expr,
    parser::{CompletedMarker, Parser},
    token_set::TokenSet,
    SyntaxKind,
};

const PARAM_START: TokenSet = TokenSet::new(&[SyntaxKind::IDENT, SyntaxKind::L_PAREN]);

pub(crate) fn params(parser: &mut Parser) -> CompletedMarker {
    let mark = parser.open();
    while parser.at_any(PARAM_START) {
        param(parser);
    }
    parser.close(mark, SyntaxKind::PARAMS)
}

fn param(parser: &mut Parser) -> CompletedMarker {
    let mark = parser.open();

    if parser.eat(SyntaxKind::IDENT) {
    } else if parser.eat(SyntaxKind::L_PAREN) {
        parser.expect(SyntaxKind::IDENT);
        parser.expect(SyntaxKind::COLON);
        type_expr::type_expr(parser);
        parser.expect(SyntaxKind::R_PAREN);
    } else {
        parser.error("Expected parameter".to_string());
    }
    parser.close(mark, SyntaxKind::PARAM)
}
