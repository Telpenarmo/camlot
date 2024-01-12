use rowan::Language;

use crate::generated::syntax_kinds::SyntaxKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RideMLLanguage {}
impl Language for RideMLLanguage {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> SyntaxKind {
        SyntaxKind::from_raw(raw.0)
    }

    fn kind_to_raw(kind: SyntaxKind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind.into_raw())
    }
}

pub type SyntaxNode = rowan::SyntaxNode<RideMLLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<RideMLLanguage>;
pub type SyntaxElement = rowan::SyntaxElement<RideMLLanguage>;
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<RideMLLanguage>;
pub type SyntaxElementChildren = rowan::SyntaxElementChildren<RideMLLanguage>;
pub type PreorderWithTokens = rowan::api::PreorderWithTokens<RideMLLanguage>;
