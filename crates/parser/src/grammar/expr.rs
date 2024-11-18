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
    use crate::{check_err, PrefixEntryPoint};
    use expect_test::expect;

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
