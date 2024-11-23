use core::TypeError;

use line_index::TextRange;
use parser::SyntaxKind;

use crate::{offset_to_position, Document};

/// # Panics
///
/// Panics if parser produced different number of error nodes than messages.
#[must_use]
pub fn get_diagnostics(doc: &Document) -> Vec<lsp_types::Diagnostic> {
    let mut diagnostics = parsing_errors(doc);
    diagnostics.extend(type_errors(doc));
    diagnostics
}

fn parsing_errors(doc: &Document) -> Vec<lsp_types::Diagnostic> {
    let parsed = doc.parsed();
    let mut error_idx = 0;
    parsed
        .syntax()
        .descendants_with_tokens()
        .filter(|node| node.kind() == SyntaxKind::ERROR || node.kind() == SyntaxKind::LEXING_ERROR)
        .map(|node| {
            let msg = &parsed
                .errors
                .get(error_idx)
                .expect("Missing error message")
                .message;
            error_idx += 1;
            syntax_error_to_diagnostic(msg, node.text_range(), doc)
        })
        .collect()
}

fn syntax_error_to_diagnostic(
    message: &str,
    range: TextRange,
    doc: &Document,
) -> lsp_types::Diagnostic {
    let line_index = &doc.get_line_index();
    let start = offset_to_position(line_index, range.start().into());
    let end = offset_to_position(line_index, range.end().into());
    let range = lsp_types::Range::new(start, end);

    let mut diagnostic = lsp_types::Diagnostic::new_simple(range, message.to_string());
    diagnostic.source = Some("Camlot".into());
    diagnostic
}

fn range_of_expr<T: core::StoredInArena>(
    hir: core::ArenaIdx<T>,
    doc: &Document,
) -> Option<TextRange> {
    doc.hir().syntax(hir).map(parser::SyntaxNodePtr::text_range)
}

fn type_errors(doc: &Document) -> Vec<lsp_types::Diagnostic> {
    doc.type_errors()
        .iter()
        .filter_map(|error| type_error_to_diagnostic(error, doc))
        .collect()
}

fn type_error_to_diagnostic(error: &TypeError, doc: &Document) -> Option<lsp_types::Diagnostic> {
    let module = doc.hir();
    match error {
        TypeError::UnboundVariable { expr, name } => {
            let msg = format!("Unbound variable {}", module.get_name(*name));
            let range = range_of_expr(*expr, doc)?;
            Some(syntax_error_to_diagnostic(&msg, range, doc))
        }
        TypeError::UnboundTypeVariable { type_expr, name } => {
            let msg = format!("Unbound type {}", module.get_name(*name));
            let range = range_of_expr(*type_expr, doc)?;
            Some(syntax_error_to_diagnostic(&msg, range, doc))
        }

        TypeError::CyclicType { src: _, typ: _ } => None,

        TypeError::TypeMismatch {
            src,
            expected,
            actual,
        } => {
            let range = match src {
                core::ConstraintReason::ApplicationTarget(expr)
                | core::ConstraintReason::Checking(expr) => range_of_expr(*expr, doc),
                core::ConstraintReason::UnitPattern(expr) => range_of_expr(*expr, doc),
            }?;

            let msg = format!(
                "Type mismatch: expected {} but got {}",
                doc.display_type(*expected),
                doc.display_type(*actual)
            );
            Some(syntax_error_to_diagnostic(&msg, range, doc))
        }
    }
}
