use lsp_server::{ExtractError, Request, RequestId};

pub(crate) fn cast_req<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}

pub(crate) fn cast_not<N>(
    not: lsp_server::Notification,
) -> Result<N::Params, ExtractError<lsp_server::Notification>>
where
    N: lsp_types::notification::Notification,
    N::Params: serde::de::DeserializeOwned,
{
    not.extract(N::METHOD)
}

pub(crate) fn syntax_error_to_diagnostic(
    error: &parser::SyntaxError,
    source: &str,
) -> lsp_types::Diagnostic {
    let line_index = line_index::LineIndex::new(source);

    let start = offset_to_position(&line_index, error.range.start as u32);
    let end = offset_to_position(&line_index, error.range.end as u32);
    let range = lsp_types::Range::new(start, end);

    let mut diagnostic = lsp_types::Diagnostic::new_simple(range, error.message.clone());
    diagnostic.source = Some("RideML".into());
    diagnostic
}

fn offset_to_position(line_index: &line_index::LineIndex, offset: u32) -> lsp_types::Position {
    let end = line_index.line_col(line_index::TextSize::new(offset));
    lsp_types::Position::new(end.line, end.col)
}
