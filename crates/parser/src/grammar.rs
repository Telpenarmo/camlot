use crate::{
    parser::{self, Parser},
    PrefixEntryPoint,
};

mod block;
mod expr;
mod module_item;
mod params;
mod type_expr;

pub(crate) fn module(parser: &mut Parser) -> parser::CompletedMarker {
    let marker = parser.open();

    while !parser.at(crate::SyntaxKind::EOF) {
        module_item::module_item(parser);
    }

    parser.close(marker, crate::SyntaxKind::MODULE)
}

pub(crate) fn parse(mut parser: Parser, entry_point: PrefixEntryPoint) -> Vec<crate::event::Event> {
    match entry_point {
        PrefixEntryPoint::Module => module(&mut parser),
        #[cfg(test)]
        PrefixEntryPoint::TypeExpr => type_expr::type_expr(&mut parser),
        #[cfg(test)]
        PrefixEntryPoint::Expr => expr::expr(&mut parser),
    };
    parser.finish()
}
