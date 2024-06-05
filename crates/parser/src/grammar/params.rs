use crate::{
    event::ErrorPlacement,
    grammar::type_expr,
    parser::{CompletedMarker, Parser},
    SyntaxKind,
};

pub(crate) fn params(parser: &mut Parser) -> CompletedMarker {
    let mark = parser.open();
    while parser.at(SyntaxKind::IDENT) || parser.at(SyntaxKind::L_PAREN) {
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
        parser.error(
            "Expected parameter".to_string(),
            ErrorPlacement::PrevTokenEnd,
        );
    }
    parser.close(mark, SyntaxKind::PARAM)
}
