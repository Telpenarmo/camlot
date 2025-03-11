use core::{ModuleAndNames, Type};
use std::ops::Range;

use line_index::{LineCol, TextRange};
use lsp_types::Position;

mod diagnostics;
mod document;
mod inlay_hints;
mod selection_range;
mod semantic_tokens;

pub use document::*;
pub use semantic_tokens::*;

pub struct ErrorDiagnostic {
    pub range: Range<usize>,
    pub message: String,
}

fn offset_to_position(line_index: &line_index::LineIndex, offset: u32) -> Position {
    let pos = line_index.line_col(line_index::TextSize::new(offset));
    Position::new(pos.line, pos.col)
}

fn position_to_offset(line_index: &line_index::LineIndex, pos: Position) -> line_index::TextSize {
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

    pub(crate) fn syntax<T: core::StoredInArena, N: parser::AstNode>(
        &self,
        idx: core::ArenaIdx<T>,
    ) -> Option<N> {
        let ptr = self.hir().syntax(idx)?;
        let ptr = ptr.cast::<N>()?;
        Some(ptr.to_node(&self.parsed().syntax()))
    }

    #[must_use]
    pub fn pretty_module(&self) -> String {
        format!(
            "{}",
            ModuleAndNames {
                module: self.hir(),
                names: self.names()
            }
        )
    }

    pub(crate) fn syntax_at(&self, pos: Position) -> parser::SyntaxElement {
        let pos = position_to_offset(self.line_index(), pos);
        let range = TextRange::new(pos, pos);
        self.parsed().syntax().covering_element(range)
    }

    fn text_range_to_lsp_range(&self, range: TextRange) -> lsp_types::Range {
        let line_index = &self.line_index();
        let start = offset_to_position(line_index, range.start().into());
        let end = offset_to_position(line_index, range.end().into());
        lsp_types::Range::new(start, end)
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_offset_to_position() {
        let line_index = line_index::LineIndex::new("hello\nworld");

        assert_eq!(offset_to_position(&line_index, 0), Position::new(0, 0));
        assert_eq!(offset_to_position(&line_index, 5), Position::new(0, 5));
        assert_eq!(offset_to_position(&line_index, 6), Position::new(1, 0));
    }

    #[test]
    fn test_position_to_offset() {
        let line_index = line_index::LineIndex::new("hello\nworld");

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
