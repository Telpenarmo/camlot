use crate::SyntaxKind;

pub(crate) type ParseError = String;

#[derive(Debug, PartialEq)]
pub(crate) enum Event {
    Open {
        kind: SyntaxKind,
        forward_parent: Option<usize>,
    },
    OpenError {
        error: ParseError,
        forward_parent: Option<usize>,
    },
    Advance,
    Close,
    CloseError,
    UnmatchedOpen,
}
