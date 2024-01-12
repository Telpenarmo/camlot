use crate::{
    grammar::{expr, params, type_expr},
    parser::Parser,
    SyntaxKind,
};

pub fn decl(parser: &mut Parser) {
    fn eat_error(parser: &mut Parser) {
        loop {
            let current = parser.current();
            match current {
                SyntaxKind::EOF
                | SyntaxKind::LET_KW
                | SyntaxKind::TYPE_KW
                | SyntaxKind::OPEN_KW => break,
                SyntaxKind::SEMICOLON => {
                    parser.advance();
                    break;
                }
                _ => {
                    parser.error(format!("Unexpected token: {:#?}", current));
                    parser.advance();
                }
            }
        }
    }
    match parser.current() {
        SyntaxKind::LET_KW => {
            let_decl(parser);
        }
        SyntaxKind::TYPE_KW => {
            type_decl(parser);
        }
        SyntaxKind::OPEN_KW => {
            open_decl(parser);
        }
        _ => eat_error(parser),
    }
}

fn open_decl(parser: &mut Parser) {
    assert!(parser.at(SyntaxKind::OPEN_KW));

    let mark = parser.open();
    parser.expect(SyntaxKind::OPEN_KW);
    parser.expect(SyntaxKind::IDENT);
    parser.expect(SyntaxKind::SEMICOLON);

    parser.close(mark, SyntaxKind::OPEN_DECL);
}

fn type_decl(parser: &mut Parser) {
    assert!(parser.at(SyntaxKind::TYPE_KW));

    let mark = parser.open();
    parser.expect(SyntaxKind::TYPE_KW);
    parser.expect(SyntaxKind::IDENT);
    parser.expect(SyntaxKind::EQUAL);
    type_expr::type_expr(parser);
    parser.expect(SyntaxKind::SEMICOLON);

    parser.close(mark, SyntaxKind::TYPE_DECL);
}

fn let_decl(parser: &mut Parser) {
    assert!(parser.at(SyntaxKind::LET_KW));

    let mark = parser.open();
    parser.expect(SyntaxKind::LET_KW);
    parser.expect(SyntaxKind::IDENT);
    params::params(parser);
    parser.expect(SyntaxKind::EQUAL);
    expr::expr(parser);
    parser.expect(SyntaxKind::SEMICOLON);

    parser.close(mark, SyntaxKind::LET_DECL);
}
