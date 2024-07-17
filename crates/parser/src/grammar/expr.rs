use super::{block::block, params::params};
use crate::{
    grammar::decl,
    parser::{CompletedMarker, Parser},
    token_set::TokenSet,
    SyntaxKind,
};

const LAMBDA_TOKENS: TokenSet = TokenSet::new(&[SyntaxKind::LAMBDA, SyntaxKind::BACKSLASH]);
const LITERAL_EXPR_FIRST: TokenSet = TokenSet::new(&[SyntaxKind::INT]);
const ATOM_EXPR_FIRST: TokenSet =
    TokenSet::new(&[SyntaxKind::L_PAREN, SyntaxKind::L_BRACE, SyntaxKind::IDENT])
        .union(LITERAL_EXPR_FIRST);

pub(crate) const EXPR_FIRST: TokenSet = LAMBDA_TOKENS.union(ATOM_EXPR_FIRST);

pub(crate) fn expr(parser: &mut Parser) -> CompletedMarker {
    if parser.at_any(LAMBDA_TOKENS) {
        lambda_expr(parser)
    } else if parser.at_any(ATOM_EXPR_FIRST) {
        delimited_expr(parser)
    } else {
        parser.error("Expected expression".into())
    }
}
fn delimited_expr(parser: &mut Parser) -> CompletedMarker {
    if parser.at(SyntaxKind::IDENT) {
        ident_expr(parser)
    } else if parser.at(SyntaxKind::L_PAREN) {
        paren_expr(parser)
    } else if parser.at_any(LITERAL_EXPR_FIRST) {
        literal_expr(parser)
    } else if parser.at(SyntaxKind::L_BRACE) {
        block(parser)
    } else {
        unreachable!()
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
    if parser.at_any(LITERAL_EXPR_FIRST) {
        parser.advance();
    } else {
        unreachable!();
    }
    parser.close(mark, SyntaxKind::LITERAL_EXPR)
}

fn lambda_expr(parser: &mut Parser) -> CompletedMarker {
    assert!(parser.at_any(LAMBDA_TOKENS));

    let mark = parser.open();
    let _ = parser.eat_any(LAMBDA_TOKENS);
    params(parser);
    parser.expect(SyntaxKind::ARROW);
    expr(parser);
    parser.close(mark, SyntaxKind::LAMBDA_EXPR)
}

fn paren_expr(parser: &mut Parser) -> CompletedMarker {
    assert!(parser.at(SyntaxKind::L_PAREN));

    let mark = parser.open();
    parser.advance();

    let paren_expr_end = TokenSet::new(&[SyntaxKind::R_PAREN])
        .union(decl::DECL_END)
        .union(decl::DECL_START)
        .union(TokenSet::new(&[SyntaxKind::EOF]));

    if parser.at_any(LAMBDA_TOKENS) {
        lambda_expr(parser);
    } else {
        let mut prev_mark = None;
        while !parser.at_any(paren_expr_end) {
            if parser.at_any(ATOM_EXPR_FIRST) {
                prev_mark = Some(parse_app_part(parser, prev_mark));
            } else {
                parser.eat_error_until(
                    ATOM_EXPR_FIRST.union(paren_expr_end),
                    "Expected expression".into(),
                );
            }
        }
    };

    parser.expect(SyntaxKind::R_PAREN);
    parser.close(mark, SyntaxKind::PAREN_EXPR)
}

fn parse_app_part(parser: &mut Parser, prev_mark: Option<CompletedMarker>) -> CompletedMarker {
    match prev_mark {
        Some(prev) => {
            let marker = parser.open_before(prev);
            delimited_expr(parser);
            parser.close(marker, SyntaxKind::APP_EXPR)
        }
        None => delimited_expr(parser),
    }
}

#[cfg(test)]
mod tests {
    use crate::{check, PrefixEntryPoint};
    use expect_test::expect;

    #[test]
    fn parse_lambda() {
        check(
            PrefixEntryPoint::Expr,
            r"\x -> x;",
            &expect![[r#"
                LAMBDA_EXPR@0..7
                  BACKSLASH@0..1 "\\"
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
    fn parse_paren() {
        check(
            PrefixEntryPoint::Expr,
            r"(x)",
            &expect![[r#"
                PAREN_EXPR@0..3
                  L_PAREN@0..1 "("
                  IDENT_EXPR@1..2
                    IDENT@1..2 "x"
                  R_PAREN@2..3 ")"
            "#]],
        );
    }

    #[test]
    fn parse_literal() {
        check(
            PrefixEntryPoint::Expr,
            r"5",
            &expect![[r#"
                LITERAL_EXPR@0..1
                  INT@0..1 "5"
            "#]],
        );
    }

    #[test]
    fn parse_ident() {
        check(
            PrefixEntryPoint::Expr,
            r"x",
            &expect![[r#"
                IDENT_EXPR@0..1
                  IDENT@0..1 "x"
            "#]],
        );
    }

    #[test]
    fn parse_app() {
        check(
            PrefixEntryPoint::Expr,
            r"(x y)",
            &expect![[r#"
                PAREN_EXPR@0..5
                  L_PAREN@0..1 "("
                  APP_EXPR@1..4
                    IDENT_EXPR@1..3
                      IDENT@1..2 "x"
                      WHITESPACE@2..3 " "
                    IDENT_EXPR@3..4
                      IDENT@3..4 "y"
                  R_PAREN@4..5 ")"
            "#]],
        );
    }

    #[test]
    fn app_is_left_associative() {
        check(
            PrefixEntryPoint::Expr,
            r"(x y z)",
            &expect![[r#"
                PAREN_EXPR@0..7
                  L_PAREN@0..1 "("
                  APP_EXPR@1..6
                    APP_EXPR@1..5
                      IDENT_EXPR@1..3
                        IDENT@1..2 "x"
                        WHITESPACE@2..3 " "
                      IDENT_EXPR@3..5
                        IDENT@3..4 "y"
                        WHITESPACE@4..5 " "
                    IDENT_EXPR@5..6
                      IDENT@5..6 "z"
                  R_PAREN@6..7 ")"
            "#]],
        );
    }

    #[test]
    fn parse_app_nested() {
        check(
            PrefixEntryPoint::Expr,
            r"(x (y z))",
            &expect![[r#"
                PAREN_EXPR@0..9
                  L_PAREN@0..1 "("
                  APP_EXPR@1..8
                    IDENT_EXPR@1..3
                      IDENT@1..2 "x"
                      WHITESPACE@2..3 " "
                    PAREN_EXPR@3..8
                      L_PAREN@3..4 "("
                      APP_EXPR@4..7
                        IDENT_EXPR@4..6
                          IDENT@4..5 "y"
                          WHITESPACE@5..6 " "
                        IDENT_EXPR@6..7
                          IDENT@6..7 "z"
                      R_PAREN@7..8 ")"
                  R_PAREN@8..9 ")"
            "#]],
        );
    }
}
