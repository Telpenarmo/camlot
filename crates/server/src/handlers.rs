#![allow(clippy::unnecessary_wraps)]
#![allow(clippy::needless_pass_by_value)]

use lsp_server::ResponseError;
use lsp_types::notification::PublishDiagnostics;
use lsp_types::{
    DocumentDiagnosticReport, DocumentDiagnosticReportResult, PublishDiagnosticsParams,
    ServerCapabilities, WorkDoneProgressOptions,
};

use crate::server::{Context, Server};

pub(crate) fn handle_document_diagnostic_request(
    req: &lsp_types::DocumentDiagnosticParams,
    _lsp: &Server,
    ctx: &Context,
) -> Result<DocumentDiagnosticReportResult, ResponseError> {
    let doc = ctx
        .get_document(&req.text_document.uri)
        .ok_or_else(|| doc_not_found_error(&req.text_document.uri))?;

    let diagnostics = doc.get_diagnostics();
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
    ctx: &mut Context,
) {
    ctx.add_document(params.text_document.uri.clone(), params.text_document.text);

    let doc = ctx.get_document(&params.text_document.uri).unwrap();

    let diagnostics = doc.get_diagnostics();
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
    ctx: &mut Context,
) {
    ctx.update_document(
        &params.text_document.uri,
        params.content_changes.last().unwrap().text.clone(),
    );
    let doc = ctx.get_document(&params.text_document.uri).unwrap();
    let diagnostics = doc.get_diagnostics();
    let params = PublishDiagnosticsParams {
        uri: params.text_document.uri,
        diagnostics,
        version: None,
    };
    lsp.send_notification::<PublishDiagnostics>(params);
}

pub(crate) fn handle_did_close_text_document_params(
    params: lsp_types::DidCloseTextDocumentParams,
    _lsp: &Server,
    ctx: &mut Context,
) {
    ctx.remove_document(&params.text_document.uri);
}

pub(crate) enum SyntaxTree {}

#[derive(serde::Serialize, serde::Deserialize, Debug)]
pub(crate) struct SyntaxTreeParams {
    text_document: lsp_types::TextDocumentIdentifier,
}

impl lsp_types::request::Request for SyntaxTree {
    type Params = SyntaxTreeParams;
    type Result = String;
    const METHOD: &'static str = "camlot-analyzer/syntaxTree";
}

pub(crate) fn handle_syntax_tree_request(
    req: &SyntaxTreeParams,
    _lsp: &Server,
    ctx: &Context,
) -> Result<String, ResponseError> {
    let doc = ctx.get_document(&req.text_document.uri).unwrap();
    Ok(doc.parsed().debug_tree())
}

pub(crate) enum HirTree {}

#[derive(serde::Serialize, serde::Deserialize, Debug)]
pub(crate) struct HirTreeRequest {
    text_document: lsp_types::TextDocumentIdentifier,
}

impl lsp_types::request::Request for HirTree {
    type Params = HirTreeRequest;
    type Result = String;
    const METHOD: &'static str = "camlot-analyzer/hir";
}

pub(crate) fn handle_hir_tree_request(
    req: &HirTreeRequest,
    _lsp: &Server,
    ctx: &Context,
) -> Result<String, ResponseError> {
    let doc = ctx.get_document(&req.text_document.uri).unwrap();
    Ok(doc.pretty_module())
}

pub(crate) fn setup_semantic_tokens_capability(server_capabilities: &mut ServerCapabilities) {
    server_capabilities.semantic_tokens_provider = Some(
        lsp_types::SemanticTokensOptions {
            legend: lsp_types::SemanticTokensLegend {
                token_types: analysis::SUPPORTED_TOKENS.to_vec(),
                token_modifiers: vec![],
            },
            full: Some(lsp_types::SemanticTokensFullOptions::Bool(true)),
            range: Some(false),
            work_done_progress_options: WorkDoneProgressOptions::default(),
        }
        .into(),
    );
}

pub(crate) fn handle_semantic_tokens_full_request(
    req: &lsp_types::SemanticTokensParams,
    _lsp: &Server,
    ctx: &Context,
) -> Result<Option<lsp_types::SemanticTokensResult>, ResponseError> {
    let doc = ctx.get_document(&req.text_document.uri).unwrap();

    let tokens = doc.get_semantic_tokens();

    Ok(Some(lsp_types::SemanticTokensResult::Tokens(
        lsp_types::SemanticTokens {
            result_id: None,
            data: tokens,
        },
    )))
}

pub(crate) fn handle_inlay_hints_request(
    req: &lsp_types::InlayHintParams,
    _lsp: &Server,
    ctx: &Context,
) -> Result<Option<Vec<lsp_types::InlayHint>>, ResponseError> {
    let doc = ctx.get_document(&req.text_document.uri).unwrap();

    let hints = doc.get_inlay_hints();

    Ok(Some(hints))
}

pub(crate) fn handle_selection_range_request(
    req: &lsp_types::SelectionRangeParams,
    _lsp: &Server,
    ctx: &Context,
) -> Result<Option<Vec<lsp_types::SelectionRange>>, ResponseError> {
    let doc = ctx.get_document(&req.text_document.uri).unwrap();

    let ranges = req
        .positions
        .iter()
        .map(|&pos| doc.get_selection_ranges(pos))
        .collect();

    Ok(Some(ranges))
}

fn doc_not_found_error(uri: &lsp_types::Uri) -> ResponseError {
    ResponseError {
        code: 0,
        message: format!("Document not found: {}", uri.as_str()),
        data: None,
    }
}
