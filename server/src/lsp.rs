use std::error::Error;

use lsp_types::request::DocumentDiagnosticRequest;
use lsp_types::ServerCapabilities;

use lsp_server::Connection;

use crate::handlers;
use crate::server::ServerBuilder;

pub(crate) fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    eprintln!("starting generic LSP server");

    let (connection, io_threads) = Connection::stdio();

    let mut server_builder = ServerBuilder::new();

    server_builder.register_request::<DocumentDiagnosticRequest, _>(
        handlers::handle_document_diagnostic_request,
    );

    server_builder
        .register_request::<handlers::SyntaxTree, _>(handlers::handle_syntax_tree_request);

    server_builder.register_notification::<lsp_types::notification::DidOpenTextDocument, _>(
        handlers::handle_did_open_text_document_params,
    );

    server_builder.register_notification::<lsp_types::notification::DidChangeTextDocument, _>(
        handlers::handle_did_change_text_document_params,
    );

    let server_capabilities = ServerCapabilities {
        text_document_sync: Some(lsp_types::TextDocumentSyncCapability::Kind(
            lsp_types::TextDocumentSyncKind::FULL,
        )),
        ..Default::default()
    };

    let server = server_builder.build(connection);

    let params = match server.initialize(server_capabilities) {
        Ok(params) => params,
        Err(e) => {
            if e.channel_is_disconnected() {
                io_threads.join()?;
            }
            return Err(e.into());
        }
    };

    server.run(params)?;

    io_threads.join()?;

    eprintln!("shutting down server");
    Ok(())
}
