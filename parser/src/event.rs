// use crate::parser::ParseError;
use crate::SyntaxKind;

#[derive(Debug, PartialEq)]
pub(crate) enum Event {
    Open {
        kind: SyntaxKind,
        forward_parent: Option<usize>,
    },
    Advance,
    Close,
    // Error(ParseError),
    UnmatchedOpen,
}
