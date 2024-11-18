use crate::{
    parser::{CompletedMarker, Parser},
    SyntaxKind,
};

pub(crate) fn delimited_type_expr(parser: &mut Parser) -> CompletedMarker {
    if parser.at(SyntaxKind::IDENT) {
        let mark = parser.open();
        parser.advance();
        parser.close(mark, SyntaxKind::TYPE_IDENT)
    } else if parser.at(SyntaxKind::L_PAREN) {
        let mark = parser.open();
        parser.advance();
        type_expr(parser);
        parser.expect(SyntaxKind::R_PAREN);
        parser.close(mark, SyntaxKind::TYPE_PAREN)
    } else {
        parser.error("Expected type expression".into())
    }
}

pub(crate) fn type_expr(parser: &mut Parser) -> CompletedMarker {
    let lhs_mark = delimited_type_expr(parser);

    if parser.eat(SyntaxKind::ARROW) {
        let marker = parser.open_before(lhs_mark);
        type_expr(parser);
        parser.close(marker, SyntaxKind::TYPE_ARROW)
    } else {
        lhs_mark
    }
}

pub(crate) fn type_annotation(parser: &mut Parser) -> Option<CompletedMarker> {
    if parser.at(SyntaxKind::COLON) {
        let marker = parser.open();
        parser.advance();
        type_expr(parser);
        Some(parser.close(marker, SyntaxKind::TYPE_ANNOTATION))
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use crate::{check_err, PrefixEntryPoint};
    use expect_test::expect;

    #[test]
    fn parse_type_arrow_without_lhs() {
        check_err(
            PrefixEntryPoint::TypeExpr,
            "-> a",
            &expect![[r#"
                TYPE_ARROW@0..4
                  ERROR@0..0
                  ARROW@0..2 "->"
                  WHITESPACE@2..3 " "
                  TYPE_IDENT@3..4
                    IDENT@3..4 "a"
            "#]],
            &["Expected type expression"],
        );
    }
}
