use super::{block::block, params::params};
use crate::{
    grammar::{module_item, params::PARAM_START, type_expr},
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
    parser.eat_any(LAMBDA_TOKENS);

    if !parser.at_any(PARAM_START) {
        parser.error("Expected a parameter".into());
    }
    params(parser);
    if parser.at(SyntaxKind::COLON) {
        let annotation_mark = parser.open();
        parser.advance();
        type_expr::delimited_type_expr(parser);
        parser.close(annotation_mark, SyntaxKind::TYPE_ANNOTATION);
    }
    parser.expect(SyntaxKind::ARROW);
    expr(parser);
    parser.close(mark, SyntaxKind::LAMBDA_EXPR)
}

fn paren_expr(parser: &mut Parser) -> CompletedMarker {
    assert!(parser.at(SyntaxKind::L_PAREN));

    let mark = parser.open();
    parser.advance();

    let paren_expr_end = TokenSet::new(&[SyntaxKind::R_PAREN])
        .union(module_item::MODULE_ITEM_END)
        .union(module_item::MODULE_ITEM_START)
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
    use crate::{check, check_err, PrefixEntryPoint};
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
                      IDENT_PATTERN@1..3
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
    fn parse_lambda_with_param_annotation() {
        check(
            PrefixEntryPoint::Expr,
            r"\(x: Int) -> x;",
            &expect![[r#"
                LAMBDA_EXPR@0..14
                  BACKSLASH@0..1 "\\"
                  PARAMS@1..10
                    PARAM@1..10
                      L_PAREN@1..2 "("
                      IDENT_PATTERN@2..3
                        IDENT@2..3 "x"
                      TYPE_ANNOTATION@3..8
                        COLON@3..4 ":"
                        WHITESPACE@4..5 " "
                        TYPE_IDENT@5..8
                          IDENT@5..8 "Int"
                      R_PAREN@8..9 ")"
                      WHITESPACE@9..10 " "
                  ARROW@10..12 "->"
                  WHITESPACE@12..13 " "
                  IDENT_EXPR@13..14
                    IDENT@13..14 "x"
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

    #[test]
    fn lambda_with_return_type_annotation() {
        check(
            PrefixEntryPoint::Expr,
            r"\x: Int -> 10",
            &expect![[r#"
                LAMBDA_EXPR@0..13
                  BACKSLASH@0..1 "\\"
                  PARAMS@1..2
                    PARAM@1..2
                      IDENT_PATTERN@1..2
                        IDENT@1..2 "x"
                  TYPE_ANNOTATION@2..8
                    COLON@2..3 ":"
                    WHITESPACE@3..4 " "
                    TYPE_IDENT@4..8
                      IDENT@4..7 "Int"
                      WHITESPACE@7..8 " "
                  ARROW@8..10 "->"
                  WHITESPACE@10..11 " "
                  LITERAL_EXPR@11..13
                    INT@11..13 "10"
            "#]],
        );
    }

    #[test]
    fn lambda_with_return_type_annotation_in_paren() {
        check(
            PrefixEntryPoint::Expr,
            r"\x: (a -> a) -> \y -> y",
            &expect![[r#"
                LAMBDA_EXPR@0..23
                  BACKSLASH@0..1 "\\"
                  PARAMS@1..2
                    PARAM@1..2
                      IDENT_PATTERN@1..2
                        IDENT@1..2 "x"
                  TYPE_ANNOTATION@2..13
                    COLON@2..3 ":"
                    WHITESPACE@3..4 " "
                    TYPE_PAREN@4..13
                      L_PAREN@4..5 "("
                      TYPE_ARROW@5..11
                        TYPE_IDENT@5..7
                          IDENT@5..6 "a"
                          WHITESPACE@6..7 " "
                        ARROW@7..9 "->"
                        WHITESPACE@9..10 " "
                        TYPE_IDENT@10..11
                          IDENT@10..11 "a"
                      R_PAREN@11..12 ")"
                      WHITESPACE@12..13 " "
                  ARROW@13..15 "->"
                  WHITESPACE@15..16 " "
                  LAMBDA_EXPR@16..23
                    BACKSLASH@16..17 "\\"
                    PARAMS@17..19
                      PARAM@17..19
                        IDENT_PATTERN@17..19
                          IDENT@17..18 "y"
                          WHITESPACE@18..19 " "
                    ARROW@19..21 "->"
                    WHITESPACE@21..22 " "
                    IDENT_EXPR@22..23
                      IDENT@22..23 "y"
            "#]],
        );
    }

    #[test]
    fn lambda_with_unit_pattern() {
        check(
            PrefixEntryPoint::Expr,
            r"\() -> ()",
            &expect![[r#"
                LAMBDA_EXPR@0..9
                  BACKSLASH@0..1 "\\"
                  PARAMS@1..4
                    PARAM@1..4
                      UNIT_PATTERN@1..4
                        L_PAREN@1..2 "("
                        R_PAREN@2..3 ")"
                        WHITESPACE@3..4 " "
                  ARROW@4..6 "->"
                  WHITESPACE@6..7 " "
                  PAREN_EXPR@7..9
                    L_PAREN@7..8 "("
                    R_PAREN@8..9 ")"
            "#]],
        );
    }

    #[test]
    fn lambda_with_annotated_unit_pattern() {
        check(
            PrefixEntryPoint::Expr,
            r"\(() : unit) -> ()",
            &expect![[r#"
                LAMBDA_EXPR@0..18
                  BACKSLASH@0..1 "\\"
                  PARAMS@1..13
                    PARAM@1..13
                      L_PAREN@1..2 "("
                      UNIT_PATTERN@2..5
                        L_PAREN@2..3 "("
                        R_PAREN@3..4 ")"
                        WHITESPACE@4..5 " "
                      TYPE_ANNOTATION@5..11
                        COLON@5..6 ":"
                        WHITESPACE@6..7 " "
                        TYPE_IDENT@7..11
                          IDENT@7..11 "unit"
                      R_PAREN@11..12 ")"
                      WHITESPACE@12..13 " "
                  ARROW@13..15 "->"
                  WHITESPACE@15..16 " "
                  PAREN_EXPR@16..18
                    L_PAREN@16..17 "("
                    R_PAREN@17..18 ")"
            "#]],
        );
    }

    #[test]
    fn lambda_missing_param() {
        check_err(
            PrefixEntryPoint::Expr,
            r"\ -> ()",
            &expect![[r#"
                LAMBDA_EXPR@0..7
                  BACKSLASH@0..1 "\\"
                  ERROR@1..1
                  WHITESPACE@1..2 " "
                  PARAMS@2..2
                  ARROW@2..4 "->"
                  WHITESPACE@4..5 " "
                  PAREN_EXPR@5..7
                    L_PAREN@5..6 "("
                    R_PAREN@6..7 ")"
            "#]],
            &["Expected a parameter"],
        );
    }
}
