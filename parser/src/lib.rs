mod ast;
mod event;
mod generated;
mod grammar;
mod language;
mod lexer;
mod parser;
mod sink;
mod source;

pub use ast::{AstChildren, AstNode, AstToken};
pub use generated::{nodes, syntax_kinds::SyntaxKind};
pub use language::*;

use rowan::GreenNode;

pub struct Parse {
    pub green_node: GreenNode,
    pub errors: Vec<String>,
}

pub fn parse(input: &str) -> Parse {
    parse_internal(input, PrefixEntryPoint::Module)
}

fn parse_internal(input: &str, entry_point: PrefixEntryPoint) -> Parse {
    let lexer = lexer::Lexer::new(input);
    let tokens: Vec<_> = lexer.collect();
    let source = source::Source::new(&tokens);
    let parser = parser::Parser::new(source);
    let events = grammar::parse(parser, entry_point);
    let sink = sink::Sink::new(&tokens, events);
    sink.finish()
}

pub(crate) enum PrefixEntryPoint {
    Module,
    #[cfg(test)]
    TypeExpr,
    #[cfg(test)]
    Expr,
    // Decl,
}

impl Parse {
    pub fn debug_tree(&self) -> String {
        let mut s = String::new();

        let tree = format!("{:#?}\n", self.syntax());

        // We cut off the last byte because formatting the SyntaxNode adds on a newline at the end.
        s.push_str(&tree[0..tree.len() - 1]);

        for error in &self.errors {
            s.push_str(&format!("{}\n", error));
        }

        s
    }

    pub fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green_node.clone())
    }
}

#[cfg(test)]
fn check(entry_point: PrefixEntryPoint, input: &str, expected_tree: expect_test::Expect) {
    let parse = parse_internal(input, entry_point);
    expected_tree.assert_eq(&parse.debug_tree());
}

#[cfg(test)]
fn check_file(input: &str, expected_tree: expect_test::ExpectFile) {
    let parse = parse(input);
    expected_tree.assert_eq(&parse.debug_tree());
}
