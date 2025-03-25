use std::error::Error;

use lsp_types::notification::{DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument};
use lsp_types::request::{
    DocumentDiagnosticRequest, DocumentSymbolRequest, InlayHintRequest, SelectionRangeRequest,
    SemanticTokensFullRequest,
};
use lsp_types::{
    OneOf, SelectionRangeProviderCapability, ServerCapabilities, TextDocumentSyncCapability,
    TextDocumentSyncKind,
};

use lsp_server::Connection;

use crate::handlers;
use crate::server::ServerBuilder;

pub(crate) fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    eprintln!("starting generic LSP server");

    let (connection, io_threads) = Connection::stdio();

    let mut server_builder = ServerBuilder::new(ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        ..Default::default()
    });

    server_builder.register_request::<_, DocumentDiagnosticRequest, _>(
        |_| {},
        handlers::handle_document_diagnostic_request,
    );

    server_builder.register_request::<_, handlers::SyntaxTree, _>(
        |_| {},
        handlers::handle_syntax_tree_request,
    );

    server_builder
        .register_request::<_, handlers::HirTree, _>(|_| {}, handlers::handle_hir_tree_request);

    server_builder.register_request::<_, SemanticTokensFullRequest, _>(
        handlers::setup_semantic_tokens_capability,
        handlers::handle_semantic_tokens_full_request,
    );

    server_builder.register_notification::<DidOpenTextDocument, _>(
        handlers::handle_did_open_text_document_params,
    );

    server_builder.register_notification::<DidChangeTextDocument, _>(
        handlers::handle_did_change_text_document_params,
    );

    server_builder.register_notification::<DidCloseTextDocument, _>(
        handlers::handle_did_close_text_document_params,
    );

    server_builder.register_request::<_, InlayHintRequest, _>(
        |caps| caps.inlay_hint_provider = Some(OneOf::Left(true)),
        handlers::handle_inlay_hints_request,
    );

    server_builder.register_request::<_, DocumentSymbolRequest, _>(
        |caps| caps.document_symbol_provider = Some(OneOf::Left(true)),
        handlers::handle_document_symbol_request,
    );

    server_builder.register_request::<_, SelectionRangeRequest, _>(
        |caps| {
            caps.selection_range_provider = Some(SelectionRangeProviderCapability::Simple(true));
        },
        handlers::handle_selection_range_request,
    );

    let (server, server_capabilities) = server_builder.build(connection);

    let params = match server.initialize(server_capabilities) {
        Ok(params) => params,
        Err(e) => {
            if e.channel_is_disconnected() {
                io_threads.join()?;
            }
            return Err(e.into());
        }
    };

    server.run(&params)?;

    io_threads.join()?;

    eprintln!("shutting down server");
    Ok(())
}
