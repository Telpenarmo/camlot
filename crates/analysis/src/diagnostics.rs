use crate::{offset_to_position, Document};

pub fn get_diagnostics(doc: &Document) -> Vec<lsp_types::Diagnostic> {
    doc.parsed()
        .errors
        .iter()
        .map(|error| syntax_error_to_diagnostic(error, doc))
        .collect()
}

fn syntax_error_to_diagnostic(
    error: &parser::SyntaxError,
    doc: &Document,
) -> lsp_types::Diagnostic {
    let line_index = &doc.get_line_index();
    let start = offset_to_position(line_index, error.range.start as u32);
    let end = offset_to_position(line_index, error.range.end as u32);
    let range = lsp_types::Range::new(start, end);

    let mut diagnostic = lsp_types::Diagnostic::new_simple(range, error.message.clone());
    diagnostic.source = Some("RideML".into());
    diagnostic
}
