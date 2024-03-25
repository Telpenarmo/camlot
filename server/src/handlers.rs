use lsp_server::ResponseError;
use lsp_types::notification::PublishDiagnostics;
use lsp_types::{
    DocumentDiagnosticReport, DocumentDiagnosticReportResult, PublishDiagnosticsParams,
};

use crate::{lsp_utils::syntax_error_to_diagnostic, server::Server};

pub(crate) fn handle_document_diagnostic_request(
    req: &lsp_types::DocumentDiagnosticParams,
    _lsp: &Server,
) -> Result<DocumentDiagnosticReportResult, ResponseError> {
    let source = req.text_document.uri.path().to_string();
    let source = std::fs::read_to_string(source).unwrap();
    let diagnostics = get_diagnostics(&source);
    Ok(DocumentDiagnosticReportResult::Report(
        DocumentDiagnosticReport::Full(lsp_types::RelatedFullDocumentDiagnosticReport {
            related_documents: None,
            full_document_diagnostic_report: lsp_types::FullDocumentDiagnosticReport {
                result_id: None,
                items: diagnostics,
            },
        }),
    ))
}

pub(crate) fn handle_did_open_text_document_params(
    params: lsp_types::DidOpenTextDocumentParams,
    lsp: &Server,
) {
    let diagnostics = get_diagnostics(&params.text_document.text);
    let params = PublishDiagnosticsParams {
        uri: params.text_document.uri,
        diagnostics,
        version: None,
    };
    lsp.send_notification::<PublishDiagnostics>(params);
}

pub(crate) fn handle_did_change_text_document_params(
    params: lsp_types::DidChangeTextDocumentParams,
    lsp: &Server,
) {
    let diagnostics = get_diagnostics(&params.content_changes[0].text);
    let params = PublishDiagnosticsParams {
        uri: params.text_document.uri,
        diagnostics,
        version: None,
    };
    lsp.send_notification::<PublishDiagnostics>(params);
}

fn get_diagnostics(source: &str) -> Vec<lsp_types::Diagnostic> {
    parser::parse(source)
        .errors
        .iter()
        .map(|error| syntax_error_to_diagnostic(error, source))
        .collect()
}
