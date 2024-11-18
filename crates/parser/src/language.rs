use rowan::Language;

use crate::{
    generated::syntax_kinds::SyntaxKind,
    nodes::{Definition, Param, TypeDefinition, TypeExpr},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum CamlotLanguage {}
impl Language for CamlotLanguage {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> SyntaxKind {
        SyntaxKind::from_raw(raw.0)
    }

    fn kind_to_raw(kind: SyntaxKind) -> rowan::SyntaxKind {
        rowan::SyntaxKind(kind.into_raw())
    }
}

pub type SyntaxNode = rowan::SyntaxNode<CamlotLanguage>;
pub type SyntaxToken = rowan::SyntaxToken<CamlotLanguage>;
pub type SyntaxElement = rowan::SyntaxElement<CamlotLanguage>;
pub type SyntaxNodeChildren = rowan::SyntaxNodeChildren<CamlotLanguage>;
pub type SyntaxElementChildren = rowan::SyntaxElementChildren<CamlotLanguage>;
pub type PreorderWithTokens = rowan::api::PreorderWithTokens<CamlotLanguage>;
pub type SyntaxNodePtr = rowan::ast::SyntaxNodePtr<CamlotLanguage>;

pub type DefinitionPtr = rowan::ast::AstPtr<Definition>;
pub type TypeDefinitionPtr = rowan::ast::AstPtr<TypeDefinition>;
pub type TypeExprPtr = rowan::ast::AstPtr<TypeExpr>;
pub type ParamPtr = rowan::ast::AstPtr<Param>;
pub type ExprPtr = rowan::ast::SyntaxNodePtr<CamlotLanguage>;
