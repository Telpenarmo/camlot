use std::ops::Range;

pub struct ErrorDiagnostic {
    pub range: Range<usize>,
    pub message: String,
}

pub fn get_diagnostics(source: &str) -> Vec<lsp_types::Diagnostic> {
    parser::parse(source)
        .errors
        .iter()
        .map(|error| syntax_error_to_diagnostic(error, source))
        .collect()
}

fn syntax_error_to_diagnostic(error: &parser::SyntaxError, source: &str) -> lsp_types::Diagnostic {
    let line_index = line_index::LineIndex::new(source);

    let start = offset_to_position(&line_index, error.range.start as u32);
    let end = offset_to_position(&line_index, error.range.end as u32);
    let range = lsp_types::Range::new(start, end);

    let mut diagnostic = lsp_types::Diagnostic::new_simple(range, error.message.clone());
    diagnostic.source = Some("RideML".into());
    diagnostic
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
