use std::collections::HashMap;
use std::error::Error;

use lsp_types::notification::PublishDiagnostics;
use lsp_types::request::DocumentDiagnosticRequest;
use lsp_types::{
    DocumentDiagnosticReport, DocumentDiagnosticReportResult, PublishDiagnosticsParams,
};
use lsp_types::{InitializeParams, ServerCapabilities};

use lsp_server::{
    Connection, ExtractError, Message, Notification, Request, RequestId, Response, ResponseError,
};

pub(crate) fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    eprintln!("starting generic LSP server");

    let (connection, io_threads) = Connection::stdio();

    let mut lsp = Lsp::new(connection);

    lsp.register_request::<DocumentDiagnosticRequest>(Box::new(|req, _lsp| {
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

    lsp.register_notification::<lsp_types::notification::DidOpenTextDocument>(Box::new(
        |params, lsp| {
            let diagnostics = get_diagnostics(&params.text_document.text);
            let params = PublishDiagnosticsParams {
                uri: params.text_document.uri,
                diagnostics,
                version: None,
            };
            let params = serde_json::to_value(&params).unwrap();
            let send =
                lsp.connection
                    .sender
                    .send(Message::Notification(lsp_server::Notification {
                        method:
                            <PublishDiagnostics as lsp_types::notification::Notification>::METHOD
                                .to_string(),
                        params,
                    }));
            if let Err(e) = send {
                eprintln!("failed to send notification: {e:?}");
            }
        },
    ));

    lsp.register_notification::<lsp_types::notification::DidChangeTextDocument>(Box::new(
        |params, lsp| {
            let diagnostics = get_diagnostics(&params.content_changes[0].text);
            let params = PublishDiagnosticsParams {
                uri: params.text_document.uri,
                diagnostics,
                version: None,
            };
            let params = serde_json::to_value(&params).unwrap();
            let method =
                <PublishDiagnostics as lsp_types::notification::Notification>::METHOD.to_string();
            let send =
                lsp.connection
                    .sender
                    .send(Message::Notification(lsp_server::Notification {
                        method,
                        params,
                    }));
            if let Err(e) = send {
                eprintln!("failed to send notification: {e:?}");
            }
        },
    ));

    lsp.register_notification::<lsp_types::notification::DidOpenTextDocument>(Box::new(
        |params, lsp| {
            let diagnostics = get_diagnostics(&params.text_document.text);
            let params = PublishDiagnosticsParams {
                uri: params.text_document.uri,
                diagnostics,
                version: None,
            };
            let params = serde_json::to_value(&params).unwrap();
            let send =
                lsp.connection
                    .sender
                    .send(Message::Notification(lsp_server::Notification {
                        method:
                            <PublishDiagnostics as lsp_types::notification::Notification>::METHOD
                                .to_string(),
                        params,
                    }));
            if let Err(e) = send {
                eprintln!("failed to send notification: {e:?}");
            }
        },
    ));

    lsp.register_notification::<lsp_types::notification::DidChangeTextDocument>(Box::new(
        |params, lsp| {
            let diagnostics = get_diagnostics(&params.content_changes[0].text);
            let params = PublishDiagnosticsParams {
                uri: params.text_document.uri,
                diagnostics,
                version: None,
            };
            let params = serde_json::to_value(&params).unwrap();
            let method =
                <PublishDiagnostics as lsp_types::notification::Notification>::METHOD.to_string();
            let send =
                lsp.connection
                    .sender
                    .send(Message::Notification(lsp_server::Notification {
                        method,
                        params,
                    }));
            if let Err(e) = send {
                eprintln!("failed to send notification: {e:?}");
            }
        },
    ));

    let server_capabilities = serde_json::to_value(&ServerCapabilities {
        text_document_sync: Some(lsp_types::TextDocumentSyncCapability::Kind(
            lsp_types::TextDocumentSyncKind::FULL,
        )),
        ..Default::default()
    })
    .unwrap();
    let initialization_params = match lsp.connection.initialize(server_capabilities) {
        Ok(it) => it,
        Err(e) => {
            if e.channel_is_disconnected() {
                io_threads.join()?;
            }
            return Err(e.into());
        }
    };
    main_loop(lsp, initialization_params)?;
    io_threads.join()?;

    eprintln!("shutting down server");
    Ok(())
}

type RequestHandler = Box<dyn Fn(Request, &Lsp) -> Response>;
type NotificationHandler = Box<dyn Fn(Notification, &Lsp) -> ()>;

struct Lsp {
    connection: Connection,
    request_handlers: HashMap<String, RequestHandler>,
    notification_handlers: HashMap<String, NotificationHandler>,
}

impl Lsp {
    fn new(connection: Connection) -> Lsp {
        Lsp {
            connection,
            request_handlers: HashMap::new(),
            notification_handlers: HashMap::new(),
        }
    }

    pub fn register_request<R>(
        &mut self,
        handler: Box<dyn Fn(&R::Params, &Lsp) -> Result<R::Result, ResponseError>>,
    ) where
        R: lsp_types::request::Request,
        R::Params: serde::de::DeserializeOwned,
    {
        self.request_handlers.insert(
            R::METHOD.into(),
            Box::new(move |req, lsp| match cast_req::<R>(req) {
                Ok((id, params)) => match handler(&params, lsp) {
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

    pub fn register_notification<N>(&mut self, handler: Box<dyn Fn(N::Params, &Lsp) -> ()>)
    where
        N: lsp_types::notification::Notification,
    {
        self.notification_handlers.insert(
            N::METHOD.into(),
            Box::new(move |not, lsp| match cast_not::<N>(not) {
                Ok(params) => handler(params, lsp),
                Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                Err(ExtractError::MethodMismatch(_)) => unreachable!(),
            }),
        );
    }

    fn handle_request(&self, req: Request) -> Option<Response> {
        self.request_handlers
            .get(&req.method)
            .map(|handler| handler(req, self))
    }

    fn handle_notification(&self, not: Notification) {
        self.notification_handlers
            .get(&not.method)
            .map(|handler| handler(not, self));
    }
}

fn main_loop(lsp: Lsp, params: serde_json::Value) -> Result<(), Box<dyn Error + Sync + Send>> {
    let _params: InitializeParams = serde_json::from_value(params).unwrap();

    eprintln!("starting example main loop");
    for msg in &lsp.connection.receiver {
        eprintln!("got msg: {msg:?}");
        match msg {
            Message::Request(req) => {
                if lsp.connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                eprintln!("got request: {req:?}");
                lsp.handle_request(req).map(|resp| {
                    let send = lsp.connection.sender.send(Message::Response(resp));
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
                lsp.handle_notification(not);
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
