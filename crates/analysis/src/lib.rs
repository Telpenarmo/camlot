use std::ops::Range;

mod diagnostics;
mod document;
mod semantic_tokens;

pub use diagnostics::*;
pub use document::*;
pub use semantic_tokens::*;

pub struct ErrorDiagnostic {
    pub range: Range<usize>,
    pub message: String,
}

fn offset_to_position(line_index: &line_index::LineIndex, offset: u32) -> lsp_types::Position {
    let end = line_index.line_col(line_index::TextSize::new(offset));
    lsp_types::Position::new(end.line, end.col)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_offset_to_position() {
        let line_index = line_index::LineIndex::new("hello\nworld");

        assert_eq!(
            offset_to_position(&line_index, 0),
            lsp_types::Position::new(0, 0)
        );
        assert_eq!(
            offset_to_position(&line_index, 5),
            lsp_types::Position::new(0, 5)
        );
        assert_eq!(
            offset_to_position(&line_index, 6),
            lsp_types::Position::new(1, 0)
        );
    }
}
