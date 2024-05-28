use crate::{
    grammar::{expr, params, type_expr},
    parser::Parser,
    SyntaxKind,
};

pub(crate) fn decl(parser: &mut Parser) {
    fn eat_error(parser: &mut Parser) {
        loop {
            if parser.at(SyntaxKind::EOF)
                || parser.at(SyntaxKind::DEF_KW)
                || parser.at(SyntaxKind::TYPE_KW)
                || parser.at(SyntaxKind::OPEN_KW)
            {
                break;
            }
            if parser.at(SyntaxKind::SEMICOLON) {
                parser.advance();
                break;
            }
            parser.unexpected();
            parser.advance();
        }
    }

    if parser.at(SyntaxKind::DEF_KW) {
        def_decl(parser);
    } else if parser.at(SyntaxKind::TYPE_KW) {
        type_decl(parser);
    } else if parser.at(SyntaxKind::OPEN_KW) {
        open_decl(parser);
    } else {
        eat_error(parser)
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

fn def_decl(parser: &mut Parser) {
    assert!(parser.at(SyntaxKind::DEF_KW));

    let mark = parser.open();
    parser.expect(SyntaxKind::DEF_KW);
    parser.expect(SyntaxKind::IDENT);
    params::params(parser);
    parser.expect(SyntaxKind::EQUAL);
    expr::expr(parser);
    parser.expect(SyntaxKind::SEMICOLON);

    parser.close(mark, SyntaxKind::DEF_DECL);
}
