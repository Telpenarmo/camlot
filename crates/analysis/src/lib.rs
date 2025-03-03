use std::ops::Range;

use line_index::{LineCol, TextRange};
use lsp_types::Position;

mod diagnostics;
mod document;
mod inlay_hints;
mod selection_range;
mod semantic_tokens;

pub use diagnostics::*;
pub use document::*;
pub use inlay_hints::*;
pub use selection_range::*;
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

fn text_range_to_lsp_range(doc: &Document, range: TextRange) -> lsp_types::Range {
    let line_index = &doc.get_line_index();
    let start = offset_to_position(line_index, range.start().into());
    let end = offset_to_position(line_index, range.end().into());
    lsp_types::Range::new(start, end)
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
