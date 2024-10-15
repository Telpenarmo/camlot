use std::marker::PhantomData;

use crate::generated::syntax_kinds::SyntaxKind;
use crate::language::{SyntaxNode, SyntaxNodeChildren, SyntaxToken};
use crate::CamlotLanguage;

pub trait AstNode: rowan::ast::AstNode<Language = CamlotLanguage> {
    fn can_cast(kind: SyntaxKind) -> bool
    where
        Self: Sized;

    fn cast(syntax: SyntaxNode) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxNode;
}

impl<N: rowan::ast::AstNode<Language = CamlotLanguage>> AstNode for N {
    fn can_cast(kind: SyntaxKind) -> bool
    where
        Self: Sized,
    {
        <Self as rowan::ast::AstNode>::can_cast(kind)
    }

    fn cast(syntax: SyntaxNode) -> Option<Self>
    where
        Self: Sized,
    {
        rowan::ast::AstNode::cast(syntax)
    }

    fn syntax(&self) -> &SyntaxNode {
        rowan::ast::AstNode::syntax(self)
    }
}

/// Like `AstNode`, but wraps tokens rather than interior nodes.
pub trait AstToken {
    fn can_cast(token: SyntaxKind) -> bool
    where
        Self: Sized;

    fn cast(syntax: SyntaxToken) -> Option<Self>
    where
        Self: Sized;

    fn syntax(&self) -> &SyntaxToken;

    fn text(&self) -> &str {
        self.syntax().text()
    }
}

#[derive(Debug, Clone)]
pub struct AstChildren<N> {
    inner: SyntaxNodeChildren,
    ph: PhantomData<N>,
}

impl<N> AstChildren<N> {
    fn new(parent: &SyntaxNode) -> Self {
        AstChildren {
            inner: parent.children(),
            ph: PhantomData,
        }
    }
}

impl<N: AstNode> Iterator for AstChildren<N> {
    type Item = N;
    fn next(&mut self) -> Option<N> {
        self.inner.find_map(<N as AstNode>::cast)
    }
}

pub(crate) mod support {
    use super::{AstChildren, AstNode, AstToken, SyntaxKind, SyntaxNode, SyntaxToken};

    pub(crate) fn child<N: AstNode>(parent: &SyntaxNode) -> Option<N> {
        parent.children().find_map(<N as AstNode>::cast)
    }

    pub(crate) fn token_child<N: AstToken>(parent: &SyntaxNode) -> Option<N> {
        parent.children_with_tokens().find_map(|n| match n {
            rowan::NodeOrToken::Node(_) => None,
            rowan::NodeOrToken::Token(t) => N::cast(t),
        })
    }

    pub(crate) fn children<N: AstNode>(parent: &SyntaxNode) -> AstChildren<N> {
        AstChildren::new(parent)
    }

    pub(crate) fn token(parent: &SyntaxNode, kind: SyntaxKind) -> Option<SyntaxToken> {
        parent
            .children_with_tokens()
            .filter_map(rowan::NodeOrToken::into_token)
            .find(|it| it.kind() == kind)
    }
}
