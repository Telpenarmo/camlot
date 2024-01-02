mod ast;
mod event;
mod generated;
mod grammar;
mod language;
mod lexer;
mod parser;
mod source;
mod sink;

pub use ast::{AstChildren, AstNode, AstToken};
pub use generated::{nodes, syntax_kinds::SyntaxKind};
pub use language::*;

use rowan::GreenNode;

pub struct Parse {
    pub green_node: GreenNode,
    pub errors: Vec<String>,
}

pub fn parse(input: &str) -> Parse {
    let lexer = lexer::Lexer::new(input);
    let tokens: Vec<_> = lexer.collect();
    let source = source::Source::new(&tokens);
    let parser = parser::Parser::new(source);
    let events = grammar::parse(parser);
    let sink = sink::Sink::new(&tokens, events);
    sink.finish()
}
