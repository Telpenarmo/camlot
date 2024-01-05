use crate::{
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

    match parser.current() {
        SyntaxKind::IDENT => parser.advance(),
        SyntaxKind::L_PAREN => {
            parser.advance();
            parser.expect(SyntaxKind::IDENT);
            parser.expect(SyntaxKind::COLON);
            type_expr::type_expr(parser);
            parser.expect(SyntaxKind::R_PAREN);
        },
        _ => unreachable!(),
    }
    parser.close(mark, SyntaxKind::PARAM)
}
