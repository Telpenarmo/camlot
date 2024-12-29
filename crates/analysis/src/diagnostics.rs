use core::{display_type, Interner, Module, Type, TypeError};

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
            diagnostic(msg, node.text_range(), doc)
        })
        .collect()
}

fn diagnostic(message: &str, range: TextRange, doc: &Document) -> lsp_types::Diagnostic {
    let range = convert_range(doc, range);
    let mut diagnostic = lsp_types::Diagnostic::new_simple(range, message.to_string());
    diagnostic.source = Some("Camlot".into());
    diagnostic
}

fn convert_range(doc: &Document, range: TextRange) -> lsp_types::Range {
    let line_index = &doc.get_line_index();
    let start = offset_to_position(line_index, range.start().into());
    let end = offset_to_position(line_index, range.end().into());
    lsp_types::Range::new(start, end)
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

#[must_use]
pub fn type_error_message(module: &Module, types: &Interner<Type>, error: &TypeError) -> String {
    match *error {
        TypeError::UnboundVariable { name, .. } => {
            format!("Unbound variable {}", module.get_name(name))
        }
        TypeError::UnboundTypeVariable { name, .. } => {
            format!("Unbound type {}", module.get_name(name))
        }
        TypeError::CyclicType { typ, var, .. } => {
            let typ = display_type(types, typ);
            format!("Cyclic type: {var} appears in {typ}")
        }
        TypeError::WrongArgument {
            expected, actual, ..
        } => {
            let expected = display_type(types, expected);
            let actual = display_type(types, actual);
            format!("Type mismatch: this function expects a value of type {expected} but {actual}.")
        }
        TypeError::TypeMismatch {
            expected, actual, ..
        } => {
            let expected = display_type(types, expected);
            let actual = display_type(types, actual);
            format!("Type mismatch: expected {expected} but got {actual}.")
        }
        TypeError::UnexpectedUnitPattern { expected, .. } => {
            let expected = display_type(types, expected);
            format!("This pattern constraints the value to type @unit, but it is expected to have type {expected}.")
        }
        TypeError::AppliedToNonFunction { func_type, .. } => {
            let func_type = display_type(types, func_type);
            format!(
                "This expression has a type {func_type}. It is not a function and cannot be applied.",
            )
        }
        TypeError::WrongAnnotation {
            actual, expected, ..
        } => {
            let actual = display_type(types, actual);
            let expected = display_type(types, expected);
            format!("This parameter was expected to have type {expected}, but was annotated as {actual}.")
        }
    }
}

#[must_use]
pub fn type_error_range(doc: &Document, error: &TypeError) -> Option<TextRange> {
    match *error {
        TypeError::AppliedToNonFunction { func: expr, .. }
        | TypeError::WrongArgument { arg: expr, .. }
        | TypeError::UnboundVariable { expr, .. }
        | TypeError::CyclicType { expr, .. }
        | TypeError::TypeMismatch { expr, .. } => range_of_expr(expr, doc),
        TypeError::UnexpectedUnitPattern { pattern, .. } => range_of_expr(pattern, doc),
        TypeError::UnboundTypeVariable { type_expr, .. }
        | TypeError::WrongAnnotation {
            annotation: type_expr,
            ..
        } => range_of_expr(type_expr, doc),
    }
}

fn type_error_to_diagnostic(error: &TypeError, doc: &Document) -> Option<lsp_types::Diagnostic> {
    let msg = type_error_message(doc.hir(), doc.types(), error);
    let range = type_error_range(doc, error)?;
    Some(diagnostic(&msg, range, doc))
}
