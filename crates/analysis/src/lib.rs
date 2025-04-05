use core::{ModuleAndNames, Type};
use std::ops::Range;

use line_index::{LineCol, LineIndex, TextRange};
use lsp_types::Position;

mod definitions;
mod diagnostics;
mod document;
mod env;
mod inlay_hints;
mod selection_range;
mod semantic_tokens;
mod symbols;

pub use document::*;
use parser::{SyntaxKind, TokenAtOffset};
pub use semantic_tokens::*;

pub struct ErrorDiagnostic {
    pub range: Range<usize>,
    pub message: String,
}

fn offset_to_position(line_index: &LineIndex, offset: u32) -> Position {
    let pos = line_index.line_col(line_index::TextSize::new(offset));
    Position::new(pos.line, pos.col)
}

pub(crate) fn position_to_offset(line_index: &LineIndex, pos: Position) -> line_index::TextSize {
    line_index
        .offset(LineCol {
            line: pos.line,
            col: pos.character,
        })
        .unwrap()
}

impl Document {
    pub(crate) fn display_type(&self, idx: core::TypeIdx) -> String {
        core::display_type(self.types(), self.names(), self.generalized_labels(), idx)
    }

    pub(crate) fn get_type(&self, idx: core::TypeIdx) -> &Type {
        self.types().get_type(idx)
    }

    pub(crate) fn syntax<T: core::HirNode, N: parser::AstNode>(
        &self,
        idx: core::ArenaIdx<T>,
    ) -> Option<N> {
        let ptr = self.hir().syntax(idx)?;
        self.ptr_to_node(ptr)
    }

    #[must_use]
    pub fn pretty_module(&self) -> String {
        let module = ModuleAndNames {
            module: self.hir(),
            names: self.names(),
        };
        format!("{module}")
    }

    pub(crate) fn syntax_at(&self, pos: Position) -> Option<parser::SyntaxToken> {
        let pos = position_to_offset(self.line_index(), pos);
        let token = self.parsed().syntax().token_at_offset(pos);
        match token {
            TokenAtOffset::None => None,
            TokenAtOffset::Single(token) => Some(token),
            TokenAtOffset::Between(left, right) => {
                match ((left.clone(), left.kind()), (right.clone(), right.kind())) {
                    ((id, SyntaxKind::IDENT), _) | (_, (id, SyntaxKind::IDENT)) => Some(id),
                    ((tok, _), _) => Some(tok),
                }
            }
        }
    }

    fn text_range_to_lsp_range(&self, range: TextRange) -> lsp_types::Range {
        let line_index = &self.line_index();
        let start = offset_to_position(line_index, range.start().into());
        let end = offset_to_position(line_index, range.end().into());
        lsp_types::Range::new(start, end)
    }

    fn ptr_to_node<N: parser::AstNode>(&self, ptr: parser::SyntaxNodePtr) -> Option<N> {
        let ptr = ptr.cast()?;
        Some(ptr.to_node(&self.parsed().syntax()))
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_offset_to_position() {
        let line_index = LineIndex::new("hello\nworld");

        assert_eq!(offset_to_position(&line_index, 0), Position::new(0, 0));
        assert_eq!(offset_to_position(&line_index, 5), Position::new(0, 5));
        assert_eq!(offset_to_position(&line_index, 6), Position::new(1, 0));
    }

    #[test]
    fn test_position_to_offset() {
        let line_index = LineIndex::new("hello\nworld");

        assert_eq!(
            position_to_offset(&line_index, Position::new(0, 0)),
            0.into()
        );
        assert_eq!(
            position_to_offset(&line_index, Position::new(0, 5)),
            5.into()
        );
        assert_eq!(
            position_to_offset(&line_index, Position::new(1, 0)),
            6.into()
        );
    }
}
