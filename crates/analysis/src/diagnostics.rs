use crate::offset_to_position;

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
