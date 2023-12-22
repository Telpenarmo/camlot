//! This is a generated file, please do not edit manually. Changes can be
//! made in codegeneration that lives in `xtask` top-level dir.

#![allow(non_snake_case, clippy::match_like_matches_macro)]
use crate::{
    ast::{support, AstChildren, AstNode, AstToken},
    SyntaxKind::{self, *},
    SyntaxNode, SyntaxToken,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Module {
    pub(crate) syntax: SyntaxNode,
}
impl Module {
    pub fn decls(&self) -> AstChildren<Decl> {
        support::children(&self.syntax)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Decl {
    pub(crate) syntax: SyntaxNode,
}
impl Decl {
    pub fn let_decl(&self) -> Option<LetDecl> {
        support::child(&self.syntax)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LetDecl {
    pub(crate) syntax: SyntaxNode,
}
impl LetDecl {
    pub fn let_kw_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, LET_KW)
    }
    pub fn ident_lit(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, IDENT)
    }
    pub fn equal_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, EQUAL)
    }
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
    pub fn semicolon_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, SEMICOLON)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VarExpr {
    pub(crate) syntax: SyntaxNode,
}
impl VarExpr {
    pub fn ident_lit(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, IDENT)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IntExpr {
    pub(crate) syntax: SyntaxNode,
}
impl IntExpr {
    pub fn int_lit(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, INT)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AppExpr {
    pub(crate) syntax: SyntaxNode,
}
impl AppExpr {
    pub fn func(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
    pub fn arg(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LambdaExpr {
    pub(crate) syntax: SyntaxNode,
}
impl LambdaExpr {
    pub fn lambda_lit(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, LAMBDA)
    }
    pub fn ident_lit(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, IDENT)
    }
    pub fn params(&self) -> AstChildren<Expr> {
        support::children(&self.syntax)
    }
    pub fn arrow_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, ARROW)
    }
    pub fn body(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LetExpr {
    pub(crate) syntax: SyntaxNode,
}
impl LetExpr {
    pub fn let_kw_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, LET_KW)
    }
    pub fn ident_lit(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, IDENT)
    }
    pub fn params(&self) -> AstChildren<Expr> {
        support::children(&self.syntax)
    }
    pub fn equal_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, EQUAL)
    }
    pub fn def(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
    pub fn in_kw_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, IN_KW)
    }
    pub fn body(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParensExpr {
    pub(crate) syntax: SyntaxNode,
}
impl ParensExpr {
    pub fn l_paren_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, L_PAREN)
    }
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
    pub fn r_paren_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, R_PAREN)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct UnitExpr {
    pub(crate) syntax: SyntaxNode,
}
impl UnitExpr {
    pub fn empty_paren_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, EMPTY_PAREN)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    VarExpr(VarExpr),
    IntExpr(IntExpr),
    AppExpr(AppExpr),
    LambdaExpr(LambdaExpr),
    LetExpr(LetExpr),
    ParensExpr(ParensExpr),
    UnitExpr(UnitExpr),
}
impl AstNode for Module {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == MODULE
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for Decl {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == DECL
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for LetDecl {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == LET_DECL
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for VarExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == VAR_EXPR
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for IntExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == INT_EXPR
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for AppExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == APP_EXPR
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for LambdaExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == LAMBDA_EXPR
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for LetExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == LET_EXPR
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for ParensExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == PARENS_EXPR
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl AstNode for UnitExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == UNIT_EXPR
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        if Self::can_cast(syntax.kind()) {
            Some(Self { syntax })
        } else {
            None
        }
    }
    fn syntax(&self) -> &SyntaxNode {
        &self.syntax
    }
}
impl From<VarExpr> for Expr {
    fn from(node: VarExpr) -> Expr {
        Expr::VarExpr(node)
    }
}
impl From<IntExpr> for Expr {
    fn from(node: IntExpr) -> Expr {
        Expr::IntExpr(node)
    }
}
impl From<AppExpr> for Expr {
    fn from(node: AppExpr) -> Expr {
        Expr::AppExpr(node)
    }
}
impl From<LambdaExpr> for Expr {
    fn from(node: LambdaExpr) -> Expr {
        Expr::LambdaExpr(node)
    }
}
impl From<LetExpr> for Expr {
    fn from(node: LetExpr) -> Expr {
        Expr::LetExpr(node)
    }
}
impl From<ParensExpr> for Expr {
    fn from(node: ParensExpr) -> Expr {
        Expr::ParensExpr(node)
    }
}
impl From<UnitExpr> for Expr {
    fn from(node: UnitExpr) -> Expr {
        Expr::UnitExpr(node)
    }
}
impl AstNode for Expr {
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            VAR_EXPR | INT_EXPR | APP_EXPR | LAMBDA_EXPR | LET_EXPR | PARENS_EXPR | UNIT_EXPR => {
                true
            }
            _ => false,
        }
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        let res = match syntax.kind() {
            VAR_EXPR => Expr::VarExpr(VarExpr { syntax }),
            INT_EXPR => Expr::IntExpr(IntExpr { syntax }),
            APP_EXPR => Expr::AppExpr(AppExpr { syntax }),
            LAMBDA_EXPR => Expr::LambdaExpr(LambdaExpr { syntax }),
            LET_EXPR => Expr::LetExpr(LetExpr { syntax }),
            PARENS_EXPR => Expr::ParensExpr(ParensExpr { syntax }),
            UNIT_EXPR => Expr::UnitExpr(UnitExpr { syntax }),
            _ => return None,
        };
        Some(res)
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Expr::VarExpr(it) => &it.syntax,
            Expr::IntExpr(it) => &it.syntax,
            Expr::AppExpr(it) => &it.syntax,
            Expr::LambdaExpr(it) => &it.syntax,
            Expr::LetExpr(it) => &it.syntax,
            Expr::ParensExpr(it) => &it.syntax,
            Expr::UnitExpr(it) => &it.syntax,
        }
    }
}
impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for Decl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for LetDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for VarExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for IntExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for AppExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for LambdaExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for LetExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for ParensExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for UnitExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
