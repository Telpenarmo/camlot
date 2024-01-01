use crate::parser::Parser;

mod decl;
use decl::decl;

#[allow(dead_code)]
pub(crate) fn parse(mut parser: Parser) -> crate::parser::ParsingResult {
    let marker = parser.open();

    while !parser.at(crate::SyntaxKind::EOF) {
        decl(&mut parser);
    }

    parser.close(marker, crate::SyntaxKind::MODULE);

    parser.finish()
}