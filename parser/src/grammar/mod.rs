use crate::parser::{CompletedMarker, Parser};

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

#[cfg(test)]
mod tests {
    use crate::check;
    use expect_test::expect;

    #[test]
    fn parse_multiple_declarations() {
        check(
            "open a; type t = a;",
            expect![[r#"
            MODULE@0..19
              OPEN_DECL@0..8
                OPEN_KW@0..4 "open"
                WHITESPACE@4..5 " "
                IDENT@5..6 "a"
                SEMICOLON@6..7 ";"
                WHITESPACE@7..8 " "
              TYPE_DECL@8..19
                TYPE_KW@8..12 "type"
                WHITESPACE@12..13 " "
                IDENT@13..14 "t"
                WHITESPACE@14..15 " "
                EQUAL@15..16 "="
                WHITESPACE@16..17 " "
                TYPE_EXPR@17..18
                  TYPE_IDENT@17..18
                    IDENT@17..18 "a"
                SEMICOLON@18..19 ";""#]],
        );
    }
}
