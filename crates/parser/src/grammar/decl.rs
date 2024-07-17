use super::{block, expr, params, type_expr};
use crate::{parser::Parser, token_set::TokenSet, SyntaxKind};

pub(crate) const DECL_START: TokenSet =
    TokenSet::new(&[SyntaxKind::DEF_KW, SyntaxKind::TYPE_KW, SyntaxKind::OPEN_KW]);
pub(crate) const DECL_END: TokenSet = TokenSet::new(&[SyntaxKind::SEMICOLON, SyntaxKind::R_BRACE]);

pub(crate) fn decl(parser: &mut Parser) {
    if parser.at(SyntaxKind::DEF_KW) {
        def_decl(parser);
    } else if parser.at(SyntaxKind::TYPE_KW) {
        type_decl(parser);
    } else if parser.at(SyntaxKind::OPEN_KW) {
        open_decl(parser);
    } else {
        parser.eat_error_until(DECL_START.union(DECL_END), "Expected declaration".into());
        if parser.at_any(DECL_END) {
            assert!(parser.eat(SyntaxKind::SEMICOLON));
        }
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

    let body = parser.open();
    if parser.eat(SyntaxKind::EQUAL) {
        expr::expr(parser);
        parser.expect(SyntaxKind::SEMICOLON);
    } else if parser.at(SyntaxKind::L_BRACE) {
        block::block(parser);
    }
    parser.close(body, SyntaxKind::DEF_BODY);

    parser.close(mark, SyntaxKind::DEF_DECL);
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use crate::{check, check_err, PrefixEntryPoint};

    #[test]
    fn parse_decl() {
        check(
            PrefixEntryPoint::Module,
            "def f = x;",
            &expect![[r#"
            MODULE@0..10
              DEF_DECL@0..10
                DEF_KW@0..3 "def"
                WHITESPACE@3..4 " "
                IDENT@4..5 "f"
                WHITESPACE@5..6 " "
                PARAMS@6..6
                DEF_BODY@6..10
                  EQUAL@6..7 "="
                  WHITESPACE@7..8 " "
                  IDENT_EXPR@8..9
                    IDENT@8..9 "x"
                  SEMICOLON@9..10 ";"
        "#]],
        );
    }

    #[test]
    fn parse_def_missing_body() {
        check_err(
            PrefixEntryPoint::Module,
            "def f =",
            &expect![[r#"
                MODULE@0..7
                  DEF_DECL@0..7
                    DEF_KW@0..3 "def"
                    WHITESPACE@3..4 " "
                    IDENT@4..5 "f"
                    WHITESPACE@5..6 " "
                    PARAMS@6..6
                    DEF_BODY@6..7
                      EQUAL@6..7 "="
                      ERROR@7..7
                      ERROR@7..7
            "#]],
            &["Expected expression", "Expected SEMICOLON but found EOF"],
        );
    }

    #[test]
    fn parse_def_only() {
        check_err(
            PrefixEntryPoint::Module,
            "def",
            &expect![[r#"
                MODULE@0..3
                  DEF_DECL@0..3
                    DEF_KW@0..3 "def"
                    ERROR@3..3
                    PARAMS@3..3
                    DEF_BODY@3..3
            "#]],
            &["Expected IDENT but found EOF"],
        );
    }
}
