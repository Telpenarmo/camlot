mod ast;
mod event;
mod generated;
mod grammar;
mod handwritten_ast;
mod language;
mod lexer;
mod parser;
mod sink;
mod source;
mod token_set;

pub use ast::{AstChildren, AstNode, AstToken};
pub use generated::{nodes, syntax_kinds::SyntaxKind};
pub use language::*;

use rowan::GreenNode;

#[derive(Debug, PartialEq)]
pub struct SyntaxError {
    pub message: String,
}

pub struct Parse {
    pub green_node: GreenNode,
    pub errors: Vec<SyntaxError>,
}

#[must_use]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum PrefixEntryPoint {
    Module,
    #[cfg(test)]
    TypeExpr,
    #[cfg(test)]
    Expr,
}

impl Parse {
    #[must_use]
    pub fn debug_tree(&self) -> String {
        format!("{:#?}", self.syntax())
    }

    #[must_use]
    pub fn syntax(&self) -> SyntaxNode {
        SyntaxNode::new_root(self.green_node.clone())
    }

    fn tree<T: AstNode>(&self) -> T {
        <T as ast::AstNode>::cast(self.syntax()).unwrap()
    }

    /// Convert the valid parse into a typed tree.
    /// # Errors
    /// Returns parse errors if any
    pub fn ok<T: ast::AstNode>(self) -> Result<T, Vec<SyntaxError>> {
        if self.errors.is_empty() {
            Ok(self.tree())
        } else {
            Err(self.errors)
        }
    }

    #[must_use]
    pub fn module(&self) -> nodes::Module {
        self.tree()
    }
}

#[cfg(test)]
#[track_caller]
#[inline]
fn check_err(
    entry_point: PrefixEntryPoint,
    input: &str,
    expected_tree: &expect_test::Expect,
    expected_errors: &[&str],
) {
    let parse = parse_internal(input, entry_point);
    expected_tree.assert_eq(&parse.debug_tree());
    let actual_errors: Vec<_> = parse.errors.iter().map(|it| it.message.as_str()).collect();
    assert_eq!(actual_errors, expected_errors);
}
