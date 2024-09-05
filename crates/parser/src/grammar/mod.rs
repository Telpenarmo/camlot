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

#[cfg(test)]
mod tests {
    use crate::{check, PrefixEntryPoint};
    use expect_test::expect;

    #[test]
    fn parse_multiple_module_items() {
        check(
            PrefixEntryPoint::Module,
            "open a; type t = a;",
            &expect![[r#"
                MODULE@0..19
                  OPEN@0..8
                    OPEN_KW@0..4 "open"
                    WHITESPACE@4..5 " "
                    IDENT@5..6 "a"
                    SEMICOLON@6..7 ";"
                    WHITESPACE@7..8 " "
                  TYPE_DEFINITION@8..19
                    TYPE_KW@8..12 "type"
                    WHITESPACE@12..13 " "
                    IDENT@13..14 "t"
                    WHITESPACE@14..15 " "
                    EQUAL@15..16 "="
                    WHITESPACE@16..17 " "
                    TYPE_IDENT@17..18
                      IDENT@17..18 "a"
                    SEMICOLON@18..19 ";"
            "#]],
        );
    }

    #[test]
    fn parse_empty_string_as_module() {
        check(PrefixEntryPoint::Module, "", &expect![["MODULE@0..0\n"]]);
    }
}
