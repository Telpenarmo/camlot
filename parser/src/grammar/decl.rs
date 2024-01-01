use crate::{marker::CompletedMarker, parser::Parser, SyntaxKind};

#[allow(unused)]
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
                _ => parser.error(format!("Unexpected token: {:#?}", current)),
            }
        }
    }
    match parser.current() {
        SyntaxKind::LET_KW => {
            type_decl(parser);
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
    type_expr(parser);
    parser.expect(SyntaxKind::SEMICOLON);

    parser.close(mark, SyntaxKind::TYPE_DECL);
}

fn type_expr(parser: &mut Parser) {
    fn lhs(p: &mut Parser) -> CompletedMarker {
        let mark = p.open();
        match p.current() {
            SyntaxKind::IDENT => {
                let mark = p.open();
                p.advance();
                p.close(mark, SyntaxKind::TYPE_IDENT);
            }
            SyntaxKind::L_PAREN => {
                let mark = p.open();
                p.advance();
                type_expr(p);
                p.expect(SyntaxKind::R_PAREN);
                p.close(mark, SyntaxKind::TYPE_PAREN);
            }
            t => {
                p.error(format!("Unexpected token: {:?}", t));
            }
        }
        mark.complete()
    }

    let lhs_mark = lhs(parser);

    if parser.eat(SyntaxKind::ARROW) {
        let marker = parser.open_before(lhs_mark);
        type_expr(parser);
        parser.close(marker, SyntaxKind::TYPE_ARROW);
    }
}
