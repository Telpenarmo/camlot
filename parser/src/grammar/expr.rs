use crate::{
    grammar::params::params,
    parser::{CompletedMarker, Parser},
    SyntaxKind,
};

pub(crate) fn expr(parser: &mut Parser) -> CompletedMarker {
    let marker = parser.open();

    match parser.current() {
        SyntaxKind::IDENT => ident_expr(parser),
        SyntaxKind::L_PAREN => paren_expr(parser),
        SyntaxKind::LAMBDA => lambda_expr(parser),
        SyntaxKind::LET_KW => let_expr(parser),
        SyntaxKind::INT | SyntaxKind::EMPTY_PAREN => literal_expr(parser),
        _ => unreachable!(),
    };

    parser.close(marker, SyntaxKind::EXPR)
}

fn ident_expr(parser: &mut Parser) -> CompletedMarker {
    assert!(parser.at(SyntaxKind::IDENT));

    let mark = parser.open();
    parser.expect(SyntaxKind::IDENT);
    parser.close(mark, SyntaxKind::IDENT_EXPR)
}

fn literal_expr(parser: &mut Parser) -> CompletedMarker {
    let mark = parser.open();
    match parser.current() {
        SyntaxKind::INT | SyntaxKind::EMPTY_PAREN => parser.advance(),
        _ => unreachable!(),
    }
    parser.close(mark, SyntaxKind::LITERAL_EXPR)
}

fn lambda_expr(parser: &mut Parser) -> CompletedMarker {
    assert!(parser.at(SyntaxKind::LAMBDA));

    let mark = parser.open();
    parser.expect(SyntaxKind::LAMBDA);
    params(parser);
    parser.expect(SyntaxKind::ARROW);
    expr(parser);
    parser.close(mark, SyntaxKind::LAMBDA_EXPR)
}

fn let_expr(parser: &mut Parser) -> CompletedMarker {
    assert!(parser.at(SyntaxKind::LET_KW));

    let mark = parser.open();
    parser.expect(SyntaxKind::LET_KW);
    parser.expect(SyntaxKind::IDENT);
    params(parser);
    parser.expect(SyntaxKind::EQUAL);
    expr(parser);
    parser.expect(SyntaxKind::IN_KW);
    expr(parser);

    parser.close(mark, SyntaxKind::LET_EXPR)
}

fn paren_expr(parser: &mut Parser) -> CompletedMarker {
    assert!(parser.at(SyntaxKind::L_PAREN));

    let mark = parser.open();
    parser.expect(SyntaxKind::L_PAREN);
    expr(parser);
    parser.expect(SyntaxKind::R_PAREN);
    parser.close(mark, SyntaxKind::PAREN_EXPR)
}
