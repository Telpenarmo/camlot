use super::{
    expr::{self, expr},
    module_item::MODULE_ITEM_START,
    params::params,
};
use crate::{
    grammar::{params::pattern, type_expr},
    parser::{CompletedMarker, Parser},
    token_set::TokenSet,
    SyntaxKind,
};

const BLOCK_END: TokenSet =
    MODULE_ITEM_START.union(TokenSet::new(&[SyntaxKind::R_BRACE, SyntaxKind::EOF]));
const STMT_FIRST: TokenSet = TokenSet::new(&[SyntaxKind::LET_KW]).union(expr::EXPR_FIRST);

pub(crate) fn block(parser: &mut Parser) -> CompletedMarker {
    assert!(parser.at(SyntaxKind::L_BRACE));
    let mark = parser.open();
    parser.expect(SyntaxKind::L_BRACE);

    while !parser.at_any(BLOCK_END) {
        if parser.at(SyntaxKind::LET_KW) {
            let_stmt(parser);
        } else if parser.at_any(expr::EXPR_FIRST) {
            let expr_marker = expr::expr(parser);
            if parser.at_any(BLOCK_END) {
                break;
            }
            let marker = parser.open_before(expr_marker);
            parser.expect(SyntaxKind::SEMICOLON);
            parser.close(marker, SyntaxKind::EXPR_STMT);
        } else {
            parser.eat_error_until(BLOCK_END.union(STMT_FIRST), "Expected statement".into());
        }
    }

    parser.expect(SyntaxKind::R_BRACE);
    parser.close(mark, SyntaxKind::BLOCK_EXPR)
}

fn let_stmt(parser: &mut Parser) -> CompletedMarker {
    assert!(parser.at(SyntaxKind::LET_KW));

    let mark = parser.open();
    parser.expect(SyntaxKind::LET_KW);
    pattern(parser);
    params(parser);
    type_expr::type_annotation(parser);
    parser.expect(SyntaxKind::EQUAL);
    expr(parser);
    parser.expect(SyntaxKind::SEMICOLON);
    parser.close(mark, SyntaxKind::LET_STMT)
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::{check_err, PrefixEntryPoint};

    #[test]
    fn unclosed_block() {
        check_err(
            PrefixEntryPoint::Expr,
            r"{",
            &expect![[r#"
                BLOCK_EXPR@0..1
                  L_BRACE@0..1 "{"
                  ERROR@1..1
            "#]],
            &["Expected R_BRACE but found EOF"],
        );
    }

    #[test]
    fn let_missing_pattern() {
        check_err(
            PrefixEntryPoint::Expr,
            r"{ let = 42; }",
            &expect![[r#"
                BLOCK_EXPR@0..13
                  L_BRACE@0..1 "{"
                  WHITESPACE@1..2 " "
                  LET_STMT@2..12
                    LET_KW@2..5 "let"
                    ERROR@5..5
                    WHITESPACE@5..6 " "
                    PARAMS@6..6
                    EQUAL@6..7 "="
                    WHITESPACE@7..8 " "
                    LITERAL_EXPR@8..10
                      INT@8..10 "42"
                    SEMICOLON@10..11 ";"
                    WHITESPACE@11..12 " "
                  R_BRACE@12..13 "}"
            "#]],
            &["Expected pattern"],
        );
    }
}
