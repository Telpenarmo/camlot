//! A minimal example LSP server that can only respond to the `gotoDefinition` request. To use
//! this example, execute it and then send an `initialize` request.
//!
//! ```no_run
//! Content-Length: 85
//!
//! {"jsonrpc": "2.0", "method": "initialize", "id": 1, "params": {"capabilities": {}}}
//! ```
//!
//! This will respond with a server response. Then send it a `initialized` notification which will
//! have no response.
//!
//! ```no_run
//! Content-Length: 59
//!
//! {"jsonrpc": "2.0", "method": "initialized", "params": {}}
//! ```
//!
//! Once these two are sent, then we enter the main loop of the server. The only request this
//! example can handle is `gotoDefinition`:
//!
//! ```no_run
//! Content-Length: 159
//!
//! {"jsonrpc": "2.0", "method": "textDocument/definition", "id": 2, "params": {"textDocument": {"uri": "file://temp"}, "position": {"line": 1, "character": 1}}}
//! ```
//!
//! To finish up without errors, send a shutdown request:
//!
//! ```no_run
//! Content-Length: 67
//!
//! {"jsonrpc": "2.0", "method": "shutdown", "id": 3, "params": null}
//! ```
//!
//! The server will exit the main loop and finally we send a `shutdown` notification to stop
//! the server.
//!
//! ```
//! Content-Length: 54
//!
//! {"jsonrpc": "2.0", "method": "exit", "params": null}
//! ```
use std::collections::HashMap;
use std::error::Error;

use lsp_types::notification::PublishDiagnostics;
use lsp_types::request::DocumentDiagnosticRequest;
use lsp_types::{
    DocumentDiagnosticReport, DocumentDiagnosticReportResult, PublishDiagnosticsParams,
};
use lsp_types::{InitializeParams, ServerCapabilities};

use lsp_server::{Connection, ExtractError, Message, Request, RequestId, Response, ResponseError};

pub(crate) fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    // Note that  we must have our logging only write out to stderr.
    eprintln!("starting generic LSP server");

    // Create the transport. Includes the stdio (stdin and stdout) versions but this could
    // also be implemented to use sockets or HTTP.
    let (connection, io_threads) = Connection::stdio();

    let mut lsp = Lsp::new();

    lsp.register_request::<DocumentDiagnosticRequest>(Box::new(|req| {
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
        // Err(ResponseError {
        // code: 1,
        // message: "not implemented".into(),
        // data: None,
        // })
    }));

    // Run the server and wait for the two threads to end (typically by trigger LSP Exit event).
    let server_capabilities = serde_json::to_value(&ServerCapabilities {
        text_document_sync: Some(lsp_types::TextDocumentSyncCapability::Kind(
            lsp_types::TextDocumentSyncKind::FULL,
        )),
        ..Default::default()
    })
    .unwrap();
    let initialization_params = match connection.initialize(server_capabilities) {
        Ok(it) => it,
        Err(e) => {
            if e.channel_is_disconnected() {
                io_threads.join()?;
            }
            return Err(e.into());
        }
    };
    main_loop(lsp, connection, initialization_params)?;
    io_threads.join()?;

    // Shut down gracefully.
    eprintln!("shutting down server");
    Ok(())
}

type RequestHandler = Box<dyn Fn(Request) -> Response>;

struct Lsp {
    request_handlers: HashMap<String, RequestHandler>,
}

impl Lsp {
    fn new() -> Lsp {
        Lsp {
            request_handlers: HashMap::new(),
        }
    }

    pub fn register_request<R>(
        &mut self,
        handler: Box<dyn Fn(&R::Params) -> Result<R::Result, ResponseError>>,
    ) where
        R: lsp_types::request::Request,
        R::Params: serde::de::DeserializeOwned,
    {
        self.request_handlers.insert(
            R::METHOD.into(),
            Box::new(move |req| match cast_req::<R>(req) {
                Ok((id, params)) => match handler(&params) {
                    Ok(result) => {
                        let result = serde_json::to_value(&result).unwrap();
                        Response {
                            id: id.clone(),
                            result: Some(result),
                            error: None,
                        }
                    }
                    Err(err) => Response {
                        id: id.clone(),
                        result: None,
                        error: Some(err),
                    },
                },
                Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                Err(ExtractError::MethodMismatch(_)) => unreachable!(),
            }),
        );
    }

    fn handle_request(&self, req: Request) -> Option<Response> {
        self.request_handlers
            .get(&req.method)
            .map(|handler| handler(req))
    }
}

fn main_loop(
    lsp: Lsp,
    connection: Connection,
    params: serde_json::Value,
) -> Result<(), Box<dyn Error + Sync + Send>> {
    let _params: InitializeParams = serde_json::from_value(params).unwrap();

    eprintln!("starting example main loop");
    for msg in &connection.receiver {
        eprintln!("got msg: {msg:?}");
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                eprintln!("got request: {req:?}");
                lsp.handle_request(req).map(|resp| {
                    let send = connection.sender.send(Message::Response(resp));
                    if let Err(e) = send {
                        eprintln!("failed to send response: {e:?}");
                    }
                });
            }
            Message::Response(resp) => {
                eprintln!("got response: {resp:?}");
            }
            Message::Notification(not) => {
                eprintln!("got notification: {not:?}");
                match cast_not::<lsp_types::notification::DidOpenTextDocument>(not) {
                    Ok(params) => {
                        let diagnostics = get_diagnostics(&params.text_document.text);
                        let params = PublishDiagnosticsParams {
                            uri: params.text_document.uri,
                            diagnostics,
                            version: None,
                        };
                        let params = serde_json::to_value(&params).unwrap();
                        let send = connection.sender.send(Message::Notification(
                            lsp_server::Notification {
                                method: <PublishDiagnostics as lsp_types::notification::Notification>::METHOD.to_string(),
                                params,
                            },
                        ));
                        if let Err(e) = send {
                            eprintln!("failed to send notification: {e:?}");
                        }
                    }
                    Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                    Err(ExtractError::MethodMismatch(not)) => {
                        match cast_not::<lsp_types::notification::DidChangeTextDocument>(not) {
                            Ok(params) => {
                                let diagnostics = get_diagnostics(&params.content_changes[0].text);
                                let params = PublishDiagnosticsParams {
                                    uri: params.text_document.uri,
                                    diagnostics,
                                    version: None,
                                };
                                let params = serde_json::to_value(&params).unwrap();
                                let send = connection.sender.send(Message::Notification(
                                    lsp_server::Notification {
                                        method: <PublishDiagnostics as lsp_types::notification::Notification>::METHOD.to_string(),
                                        params,
                                    },
                                ));
                                if let Err(e) = send {
                                    eprintln!("failed to send notification: {e:?}");
                                }
                            }
                            Err(err) => panic!("{err:?}"),
                        }
                    }
                }
            }
        }
    }
    Ok(())
}

fn cast_req<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}

fn cast_not<N>(
    not: lsp_server::Notification,
) -> Result<N::Params, ExtractError<lsp_server::Notification>>
where
    N: lsp_types::notification::Notification,
    N::Params: serde::de::DeserializeOwned,
{
    not.extract(N::METHOD)
}

fn get_diagnostics(source: &str) -> Vec<lsp_types::Diagnostic> {
    let parsed = parser::parse(source);

    parsed
        .errors
        .iter()
        .map(|error| syntax_error_to_diagnostic(error, source))
        .collect()
}

fn syntax_error_to_diagnostic(error: &parser::SyntaxError, source: &str) -> lsp_types::Diagnostic {
    let line_index = line_index::LineIndex::new(source);
    let start = line_index.line_col(line_index::TextSize::new(error.range.start as u32));
    let end = line_index.line_col(line_index::TextSize::new(error.range.end as u32));
    let start = lsp_types::Position::new(start.line, start.col);
    let end = lsp_types::Position::new(end.line, end.col);
    let range = lsp_types::Range::new(start, end);
    let mut diagnostic = lsp_types::Diagnostic::new_simple(range, error.message.clone());
    diagnostic.source = Some("RideML".into());
    diagnostic
}
