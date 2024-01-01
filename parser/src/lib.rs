mod ast;
mod generated;
mod language;
mod event;
mod source;
mod marker;
mod parser;

pub use ast::{AstChildren, AstNode, AstToken};
pub use generated::{nodes, syntax_kinds::SyntaxKind};
pub use language::*;

use rowan::GreenNode;

pub struct Parse {
    pub green_node: GreenNode,
    #[allow(unused)]
    pub errors: Vec<String>,
}

#[allow(dead_code, unused)]
pub fn parse(input: &str) -> Parse {
    todo!()
}
