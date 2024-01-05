use crate::{
    parser::{CompletedMarker, Parser},
    SyntaxKind,
};

pub(crate) fn type_expr(parser: &mut Parser) {
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
        p.close(mark, SyntaxKind::TYPE_EXPR)
    }

    let lhs_mark = lhs(parser);

    if parser.eat(SyntaxKind::ARROW) {
        let marker = parser.open_before(lhs_mark);
        type_expr(parser);
        parser.close(marker, SyntaxKind::TYPE_ARROW);
    }
}
