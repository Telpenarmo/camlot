use crate::{
    grammar::params::params,
    parser::{CompletedMarker, Parser},
    SyntaxKind,
};

pub(crate) fn expr(parser: &mut Parser) {
    match parser.current() {
        SyntaxKind::LET_KW => {
            let_expr(parser);
        }
        SyntaxKind::LAMBDA => {
            lambda_expr(parser);
        }
        _ => {
            let mut prev_mark = None;
            while parser.at(SyntaxKind::L_PAREN)
                || parser.at(SyntaxKind::IDENT)
                || parser.at(SyntaxKind::INT)
                || parser.at(SyntaxKind::EMPTY_PAREN)
            {
                match prev_mark {
                    Some(prev) => {
                        let marker = parser.open_before(prev);
                        delimited_expr(parser);
                        prev_mark = Some(parser.close(marker, SyntaxKind::APP_EXPR));
                    }
                    None => prev_mark = Some(delimited_expr(parser)),
                };
            }
        }
    };
}

fn delimited_expr(parser: &mut Parser) -> CompletedMarker {
    match parser.current() {
        SyntaxKind::IDENT => ident_expr(parser),
        SyntaxKind::L_PAREN => paren_expr(parser),
        SyntaxKind::INT | SyntaxKind::EMPTY_PAREN => literal_expr(parser),
        _ => unreachable!(),
    }
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

#[cfg(test)]
mod tests {
    use crate::{check, check_file, PrefixEntryPoint};
    use expect_test::{expect, expect_file};

    #[test]
    fn parse_lambda() {
        check(
            PrefixEntryPoint::Decl,
            r"let f = \x -> x;",
            expect![[r#"
                LET_DECL@0..16
                  LET_KW@0..3 "let"
                  WHITESPACE@3..4 " "
                  IDENT@4..5 "f"
                  WHITESPACE@5..6 " "
                  PARAMS@6..6
                  EQUAL@6..7 "="
                  WHITESPACE@7..8 " "
                  LAMBDA_EXPR@8..15
                    LAMBDA@8..9 "\\"
                    PARAMS@9..11
                      PARAM@9..11
                        IDENT@9..10 "x"
                        WHITESPACE@10..11 " "
                    ARROW@11..13 "->"
                    WHITESPACE@13..14 " "
                    IDENT_EXPR@14..15
                      IDENT@14..15 "x"
                  SEMICOLON@15..16 ";""#]],
        );
    }

    #[test]
    fn parse_let() {
        eprintln!("{}", std::env::current_dir().unwrap().display());
        check_file(
            r"let f = \x -> let y = x in y;",
            expect_file!["../../test_data/parse_let.rml_cst"],
        );
    }
}
