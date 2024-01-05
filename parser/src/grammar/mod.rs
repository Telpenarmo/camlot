use crate::parser::Parser;

mod decl;
mod expr;
mod params;
mod type_expr;

use decl::decl;

pub(crate) fn parse(mut parser: Parser) -> Vec<crate::event::Event> {
    let marker = parser.open();

    while !parser.at(crate::SyntaxKind::EOF) {
        decl(&mut parser);
    }

    parser.close(marker, crate::SyntaxKind::MODULE);

    parser.finish()
}
