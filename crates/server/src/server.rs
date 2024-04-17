use std::{collections::HashMap, error::Error};

use lsp_server::{
    Connection, ExtractError, Message, Notification, Request, Response, ResponseError,
};
use lsp_types::{InitializeParams, ServerCapabilities};

use crate::lsp_utils;

type RequestHandler = Box<dyn Fn(Request, &Server) -> Response>;

type NotificationHandler = Box<dyn Fn(Notification, &Server)>;

pub(crate) struct Server {
    connection: Connection,
    request_handlers: HashMap<String, RequestHandler>,
    notification_handlers: HashMap<String, NotificationHandler>,
}

impl Server {
    fn send_message(&self, msg: Message) {
        let sent = self.connection.sender.send(msg);
        if let Err(e) = sent {
            eprintln!("failed to send message: {}", e);
        }
    }

    pub(crate) fn send_notification<N>(&self, params: N::Params)
    where
        N: lsp_types::notification::Notification,
    {
        let params = serde_json::to_value(params).unwrap();
        let msg = Message::Notification(Notification::new(N::METHOD.into(), params));
        self.send_message(msg);
    }

    pub(crate) fn send_response(&self, response: Response) {
        self.send_message(Message::Response(response));
    }
}

impl Server {
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

    pub(crate) fn initialize(
        &self,
        server_capabilities: ServerCapabilities,
    ) -> Result<InitializeParams, lsp_server::ProtocolError> {
        let server_capabilities = serde_json::to_value(server_capabilities).unwrap();
        self.connection
            .initialize(server_capabilities)
            .map(|it| serde_json::from_value(it).unwrap())
    }

    pub(crate) fn run(self, _params: InitializeParams) -> Result<(), Box<dyn Error + Sync + Send>> {
        eprintln!("starting example main loop");
        for msg in &self.connection.receiver {
            eprintln!("got msg: {msg:?}");
            match msg {
                Message::Request(req) => {
                    if self.connection.handle_shutdown(&req)? {
                        return Ok(());
                    }
                    eprintln!("got request: {req:?}");
                    if let Some(resp) = self.handle_request(req) {
                        self.send_response(resp);
                    };
                }
                Message::Response(resp) => {
                    eprintln!("got response: {resp:?}");
                }
                Message::Notification(not) => {
                    eprintln!("got notification: {not:?}");
                    self.handle_notification(not);
                }
            }
        }
        Ok(())
    }
}

pub(crate) struct ServerBuilder {
    request_handlers: HashMap<String, RequestHandler>,
    notification_handlers: HashMap<String, NotificationHandler>,
}

impl ServerBuilder {
    pub(crate) fn new() -> ServerBuilder {
        ServerBuilder {
            request_handlers: HashMap::new(),
            notification_handlers: HashMap::new(),
        }
    }

    pub(crate) fn build(self, connection: Connection) -> Server {
        Server {
            connection,
            request_handlers: self.request_handlers,
            notification_handlers: self.notification_handlers,
        }
    }
}

impl ServerBuilder {
    pub(crate) fn register_request<R, F>(&mut self, handler: F)
    where
        R: lsp_types::request::Request,
        R::Params: serde::de::DeserializeOwned,
        F: Fn(&R::Params, &Server) -> Result<R::Result, ResponseError> + 'static,
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

    pub(crate) fn register_notification<N, F>(&mut self, handler: F)
    where
        N: lsp_types::notification::Notification,
        F: Fn(N::Params, &Server) + 'static,
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
}
