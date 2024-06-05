use crate::SyntaxKind;

#[derive(PartialEq, Debug, Clone, Copy)]
pub(crate) enum ErrorPlacement {
    PrevTokenEnd,
    NextToken,
}

#[derive(PartialEq, Debug)]
pub(crate) struct ParseError {
    pub message: String,
    pub location: ErrorPlacement,
}

#[derive(Debug, PartialEq)]
pub(crate) enum Event {
    Open {
        kind: SyntaxKind,
        forward_parent: Option<usize>,
    },
    Advance,
    Close,
    Error(ParseError),
    UnmatchedOpen,
}
