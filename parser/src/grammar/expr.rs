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
            PrefixEntryPoint::Expr,
            r"\x -> x;",
            expect![[r#"
                LAMBDA_EXPR@0..7
                  LAMBDA@0..1 "\\"
                  PARAMS@1..3
                    PARAM@1..3
                      IDENT@1..2 "x"
                      WHITESPACE@2..3 " "
                  ARROW@3..5 "->"
                  WHITESPACE@5..6 " "
                  IDENT_EXPR@6..7
                    IDENT@6..7 "x"
            "#]],
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

    #[test]
    fn parse_paren() {
        check(
            PrefixEntryPoint::Expr,
            r"(x)",
            expect![[r#"
                PAREN_EXPR@0..3
                  L_PAREN@0..1 "("
                  IDENT_EXPR@1..2
                    IDENT@1..2 "x"
                  R_PAREN@2..3 ")"
            "#]],
        )
    }

    #[test]
    fn parse_literal() {
        check(
            PrefixEntryPoint::Expr,
            r"5",
            expect![[r#"
                LITERAL_EXPR@0..1
                  INT@0..1 "5"
            "#]],
        )
    }

    #[test]
    fn parse_ident() {
        check(
            PrefixEntryPoint::Expr,
            r"x",
            expect![[r#"
                IDENT_EXPR@0..1
                  IDENT@0..1 "x"
            "#]],
        )
    }

    #[test]
    fn parse_app() {
        check(
            PrefixEntryPoint::Expr,
            r"x y",
            expect![[r#"
                APP_EXPR@0..3
                  IDENT_EXPR@0..2
                    IDENT@0..1 "x"
                    WHITESPACE@1..2 " "
                  IDENT_EXPR@2..3
                    IDENT@2..3 "y"
            "#]],
        )
    }

    #[test]
    fn parse_app_left_associative() {
        check(
            PrefixEntryPoint::Expr,
            r"x y z",
            expect![[r#"
                APP_EXPR@0..5
                  APP_EXPR@0..4
                    IDENT_EXPR@0..2
                      IDENT@0..1 "x"
                      WHITESPACE@1..2 " "
                    IDENT_EXPR@2..4
                      IDENT@2..3 "y"
                      WHITESPACE@3..4 " "
                  IDENT_EXPR@4..5
                    IDENT@4..5 "z"
            "#]],
        )
    }

    #[test]
    fn parse_app_nested() {
        check(
            PrefixEntryPoint::Expr,
            r"x (y z)",
            expect![[r#"
                APP_EXPR@0..7
                  IDENT_EXPR@0..2
                    IDENT@0..1 "x"
                    WHITESPACE@1..2 " "
                  PAREN_EXPR@2..7
                    L_PAREN@2..3 "("
                    APP_EXPR@3..6
                      IDENT_EXPR@3..5
                        IDENT@3..4 "y"
                        WHITESPACE@4..5 " "
                      IDENT_EXPR@5..6
                        IDENT@5..6 "z"
                    R_PAREN@6..7 ")"
            "#]],
        )
    }

    #[test]
    fn parse_let_nested() {
        check_file(
            r"let a = let x = 5 in let y = x in y;",
            expect_file!["../../test_data/parse_let_nested.rml_cst"],
        )
    }
}
