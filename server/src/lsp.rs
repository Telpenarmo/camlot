use std::collections::HashMap;
use std::error::Error;

use lsp_types::request::DocumentDiagnosticRequest;
use lsp_types::{InitializeParams, ServerCapabilities};

use lsp_server::{
    Connection, ExtractError, Message, Notification, Request, Response, ResponseError,
};

use crate::{handlers, lsp_utils};

pub(crate) fn main() -> Result<(), Box<dyn Error + Sync + Send>> {
    eprintln!("starting generic LSP server");

    let (connection, io_threads) = Connection::stdio();

    let mut lsp = Lsp::new(connection);

    lsp.register_request::<DocumentDiagnosticRequest, _>(
        handlers::handle_document_diagnostic_request,
    );

    lsp.register_notification::<lsp_types::notification::DidOpenTextDocument, _>(
        handlers::handle_did_open_text_document_params,
    );

    lsp.register_notification::<lsp_types::notification::DidChangeTextDocument, _>(
        handlers::handle_did_change_text_document_params,
    );

    let server_capabilities = serde_json::to_value(ServerCapabilities {
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
type NotificationHandler = Box<dyn Fn(Notification, &Lsp)>;

pub(crate) struct Lsp {
    pub connection: Connection,
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

    pub fn register_request<R, F>(&mut self, handler: F)
    where
        R: lsp_types::request::Request,
        R::Params: serde::de::DeserializeOwned,
        F: Fn(&R::Params, &Lsp) -> Result<R::Result, ResponseError> + 'static,
    {
        self.request_handlers.insert(
            R::METHOD.into(),
            Box::new(move |req, lsp| match lsp_utils::cast_req::<R>(req) {
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

    pub fn register_notification<N, F>(&mut self, handler: F)
    where
        N: lsp_types::notification::Notification,
        F: Fn(N::Params, &Lsp) + 'static,
    {
        self.notification_handlers.insert(
            N::METHOD.into(),
            Box::new(move |not, lsp| match lsp_utils::cast_not::<N>(not) {
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
        if let Some(handler) = self.notification_handlers.get(&not.method) {
            handler(not, self)
        }
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
                if let Some(resp) = lsp.handle_request(req) {
                    let send = lsp.connection.sender.send(Message::Response(resp));
                    if let Err(e) = send {
                        eprintln!("failed to send response: {e:?}");
                    }
                };
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
