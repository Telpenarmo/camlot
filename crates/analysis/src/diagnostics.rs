use crate::{offset_to_position, Document};

#[must_use]
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
    let start = u32::try_from(error.range.start).expect("Over u32::MAX characters in file?");
    let start = offset_to_position(line_index, start);
    let end = u32::try_from(error.range.end).expect("Over u32::MAX characters in file?");
    let end = offset_to_position(line_index, end);
    let range = lsp_types::Range::new(start, end);

    let mut diagnostic = lsp_types::Diagnostic::new_simple(range, error.message.clone());
    diagnostic.source = Some("RideML".into());
    diagnostic
}
