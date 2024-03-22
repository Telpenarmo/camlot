use lsp_server::{Message, ResponseError};
use lsp_types::{
    notification::PublishDiagnostics, DocumentDiagnosticReport, DocumentDiagnosticReportResult,
    PublishDiagnosticsParams,
};

use crate::lsp::{get_diagnostics, Lsp};

pub(crate) fn handle_document_diagnostic_request(
    req: &lsp_types::DocumentDiagnosticParams,
    _lsp: &Lsp,
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
    lsp: &Lsp,
) {
    let diagnostics = get_diagnostics(&params.text_document.text);
    let params = PublishDiagnosticsParams {
        uri: params.text_document.uri,
        diagnostics,
        version: None,
    };
    let params = serde_json::to_value(params).unwrap();
    let send = lsp
        .connection
        .sender
        .send(Message::Notification(lsp_server::Notification {
            method: <PublishDiagnostics as lsp_types::notification::Notification>::METHOD
                .to_string(),
            params,
        }));
    if let Err(e) = send {
        eprintln!("failed to send notification: {e:?}");
    }
}

pub(crate) fn handle_did_change_text_document_params(
    params: lsp_types::DidChangeTextDocumentParams,
    lsp: &Lsp,
) {
    let diagnostics = get_diagnostics(&params.content_changes[0].text);
    let params = PublishDiagnosticsParams {
        uri: params.text_document.uri,
        diagnostics,
        version: None,
    };
    let params = serde_json::to_value(params).unwrap();
    let method = <PublishDiagnostics as lsp_types::notification::Notification>::METHOD.to_string();
    let send = lsp
        .connection
        .sender
        .send(Message::Notification(lsp_server::Notification {
            method,
            params,
        }));
    if let Err(e) = send {
        eprintln!("failed to send notification: {e:?}");
    }
}
