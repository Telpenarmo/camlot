//! This is a generated file, please do not edit manually. Changes can be
//! made in codegeneration that lives in `xtask` top-level dir.

#![allow(
    non_snake_case,
    clippy::match_like_matches_macro,
    clippy::enum_glob_use
)]
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
    #[must_use]
    pub fn decls(&self) -> AstChildren<Decl> {
        support::children(&self.syntax)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DefDecl {
    pub(crate) syntax: SyntaxNode,
}
impl DefDecl {
    #[must_use]
    pub fn def_body(&self) -> Option<DefBody> {
        support::child(&self.syntax)
    }
    #[must_use]
    pub fn params(&self) -> Option<Params> {
        support::child(&self.syntax)
    }
    #[must_use]
    pub fn def_kw_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, DEF_KW)
    }
    #[must_use]
    pub fn ident_lit(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, IDENT)
    }
    #[must_use]
    pub fn type_annotation(&self) -> Option<TypeAnnotation> {
        support::child(&self.syntax)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct OpenDecl {
    pub(crate) syntax: SyntaxNode,
}
impl OpenDecl {
    #[must_use]
    pub fn open_kw_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, OPEN_KW)
    }
    #[must_use]
    pub fn ident_lit(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, IDENT)
    }
    #[must_use]
    pub fn semicolon_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, SEMICOLON)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeDecl {
    pub(crate) syntax: SyntaxNode,
}
impl TypeDecl {
    #[must_use]
    pub fn type_kw_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, TYPE_KW)
    }
    #[must_use]
    pub fn ident_lit(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, IDENT)
    }
    #[must_use]
    pub fn equal_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, EQUAL)
    }
    #[must_use]
    pub fn semicolon_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, SEMICOLON)
    }
    #[must_use]
    pub fn type_expr(&self) -> Option<TypeExpr> {
        support::child(&self.syntax)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Params {
    pub(crate) syntax: SyntaxNode,
}
impl Params {
    #[must_use]
    pub fn params(&self) -> AstChildren<Param> {
        support::children(&self.syntax)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeAnnotation {
    pub(crate) syntax: SyntaxNode,
}
impl TypeAnnotation {
    #[must_use]
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, COLON)
    }
    #[must_use]
    pub fn type_expr(&self) -> Option<TypeExpr> {
        support::child(&self.syntax)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DefBody {
    pub(crate) syntax: SyntaxNode,
}
impl DefBody {
    #[must_use]
    pub fn block_expr(&self) -> Option<BlockExpr> {
        support::child(&self.syntax)
    }
    #[must_use]
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
    #[must_use]
    pub fn equal_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, EQUAL)
    }
    #[must_use]
    pub fn semicolon_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, SEMICOLON)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BlockExpr {
    pub(crate) syntax: SyntaxNode,
}
impl BlockExpr {
    #[must_use]
    pub fn tail_expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
    #[must_use]
    pub fn statements(&self) -> AstChildren<Stmt> {
        support::children(&self.syntax)
    }
    #[must_use]
    pub fn l_brace_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, L_BRACE)
    }
    #[must_use]
    pub fn r_brace_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, R_BRACE)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeIdent {
    pub(crate) syntax: SyntaxNode,
}
impl TypeIdent {
    #[must_use]
    pub fn ident_lit(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, IDENT)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeArrow {
    pub(crate) syntax: SyntaxNode,
}
impl TypeArrow {
    #[must_use]
    pub fn arrow_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, ARROW)
    }
    #[must_use]
    pub fn from(&self) -> Option<TypeExpr> {
        crate::handwritten_ast::type_arrow_from(&self.syntax)
    }
    #[must_use]
    pub fn to(&self) -> Option<TypeExpr> {
        crate::handwritten_ast::type_arrow_to(&self.syntax)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeParen {
    pub(crate) syntax: SyntaxNode,
}
impl TypeParen {
    #[must_use]
    pub fn l_paren_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, L_PAREN)
    }
    #[must_use]
    pub fn r_paren_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, R_PAREN)
    }
    #[must_use]
    pub fn type_expr(&self) -> Option<TypeExpr> {
        support::child(&self.syntax)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExprStmt {
    pub(crate) syntax: SyntaxNode,
}
impl ExprStmt {
    #[must_use]
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
    #[must_use]
    pub fn semicolon_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, SEMICOLON)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LetStmt {
    pub(crate) syntax: SyntaxNode,
}
impl LetStmt {
    #[must_use]
    pub fn def(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
    #[must_use]
    pub fn params(&self) -> Option<Params> {
        support::child(&self.syntax)
    }
    #[must_use]
    pub fn let_kw_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, LET_KW)
    }
    #[must_use]
    pub fn ident_lit(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, IDENT)
    }
    #[must_use]
    pub fn equal_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, EQUAL)
    }
    #[must_use]
    pub fn semicolon_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, SEMICOLON)
    }
    #[must_use]
    pub fn type_annotation(&self) -> Option<TypeAnnotation> {
        support::child(&self.syntax)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IdentExpr {
    pub(crate) syntax: SyntaxNode,
}
impl IdentExpr {
    #[must_use]
    pub fn ident_lit(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, IDENT)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LiteralExpr {
    pub(crate) syntax: SyntaxNode,
}
impl LiteralExpr {
    #[must_use]
    pub fn literal(&self) -> Option<Literal> {
        support::token_child(&self.syntax)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LambdaExpr {
    pub(crate) syntax: SyntaxNode,
}
impl LambdaExpr {
    #[must_use]
    pub fn body(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
    #[must_use]
    pub fn params(&self) -> Option<Params> {
        support::child(&self.syntax)
    }
    #[must_use]
    pub fn lambda_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, LAMBDA)
    }
    #[must_use]
    pub fn backslash_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, BACKSLASH)
    }
    #[must_use]
    pub fn arrow_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, ARROW)
    }
    #[must_use]
    pub fn type_annotation(&self) -> Option<TypeAnnotation> {
        support::child(&self.syntax)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParenExpr {
    pub(crate) syntax: SyntaxNode,
}
impl ParenExpr {
    #[must_use]
    pub fn app_expr(&self) -> Option<AppExpr> {
        support::child(&self.syntax)
    }
    #[must_use]
    pub fn expr(&self) -> Option<Expr> {
        support::child(&self.syntax)
    }
    #[must_use]
    pub fn l_paren_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, L_PAREN)
    }
    #[must_use]
    pub fn r_paren_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, R_PAREN)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BinaryExpr {
    pub(crate) syntax: SyntaxNode,
}
impl BinaryExpr {
    #[must_use]
    pub fn lhs(&self) -> Option<Expr> {
        crate::handwritten_ast::binary_expr_lhs(&self.syntax)
    }
    #[must_use]
    pub fn rhs(&self) -> Option<Expr> {
        crate::handwritten_ast::binary_expr_rhs(&self.syntax)
    }
    #[must_use]
    pub fn infix_symbol(&self) -> Option<InfixSymbol> {
        support::token_child(&self.syntax)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AppExpr {
    pub(crate) syntax: SyntaxNode,
}
impl AppExpr {
    #[must_use]
    pub fn app_func(&self) -> Option<AppExpr> {
        support::child(&self.syntax)
    }
    #[must_use]
    pub fn func(&self) -> Option<Expr> {
        crate::handwritten_ast::app_expr_func(&self.syntax)
    }
    #[must_use]
    pub fn arg(&self) -> Option<Expr> {
        crate::handwritten_ast::app_expr_arg(&self.syntax)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Param {
    pub(crate) syntax: SyntaxNode,
}
impl Param {
    #[must_use]
    pub fn ident_lit(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, IDENT)
    }
    #[must_use]
    pub fn l_paren_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, L_PAREN)
    }
    #[must_use]
    pub fn r_paren_token(&self) -> Option<SyntaxToken> {
        support::token(&self.syntax, R_PAREN)
    }
    #[must_use]
    pub fn type_annotation(&self) -> Option<TypeAnnotation> {
        support::child(&self.syntax)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Decl {
    DefDecl(DefDecl),
    OpenDecl(OpenDecl),
    TypeDecl(TypeDecl),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeExpr {
    TypeIdent(TypeIdent),
    TypeArrow(TypeArrow),
    TypeParen(TypeParen),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expr {
    IdentExpr(IdentExpr),
    LiteralExpr(LiteralExpr),
    LambdaExpr(LambdaExpr),
    ParenExpr(ParenExpr),
    BinaryExpr(BinaryExpr),
    BlockExpr(BlockExpr),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Stmt {
    ExprStmt(ExprStmt),
    LetStmt(LetStmt),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Literal {
    syntax: SyntaxToken,
    kind: LiteralKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LiteralKind {
    Int,
    DummyKw,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InfixSymbol {
    syntax: SyntaxToken,
    kind: InfixSymbolKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InfixSymbolKind {
    Plus,
    Minus,
    Star,
    Slash,
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
impl AstNode for DefDecl {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == DEF_DECL
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
impl AstNode for OpenDecl {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == OPEN_DECL
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
impl AstNode for TypeDecl {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == TYPE_DECL
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
impl AstNode for Params {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == PARAMS
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
impl AstNode for TypeAnnotation {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == TYPE_ANNOTATION
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
impl AstNode for DefBody {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == DEF_BODY
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
impl AstNode for BlockExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == BLOCK_EXPR
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
impl AstNode for TypeIdent {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == TYPE_IDENT
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
impl AstNode for TypeArrow {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == TYPE_ARROW
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
impl AstNode for TypeParen {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == TYPE_PAREN
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
impl AstNode for ExprStmt {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == EXPR_STMT
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
impl AstNode for LetStmt {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == LET_STMT
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
impl AstNode for IdentExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == IDENT_EXPR
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
impl AstNode for LiteralExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == LITERAL_EXPR
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
impl AstNode for ParenExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == PAREN_EXPR
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
impl AstNode for BinaryExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == BINARY_EXPR
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
impl AstNode for Param {
    fn can_cast(kind: SyntaxKind) -> bool {
        kind == PARAM
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
impl From<DefDecl> for Decl {
    fn from(node: DefDecl) -> Decl {
        Decl::DefDecl(node)
    }
}
impl From<OpenDecl> for Decl {
    fn from(node: OpenDecl) -> Decl {
        Decl::OpenDecl(node)
    }
}
impl From<TypeDecl> for Decl {
    fn from(node: TypeDecl) -> Decl {
        Decl::TypeDecl(node)
    }
}
impl AstNode for Decl {
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            DEF_DECL | OPEN_DECL | TYPE_DECL => true,
            _ => false,
        }
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        let res = match syntax.kind() {
            DEF_DECL => Decl::DefDecl(DefDecl { syntax }),
            OPEN_DECL => Decl::OpenDecl(OpenDecl { syntax }),
            TYPE_DECL => Decl::TypeDecl(TypeDecl { syntax }),
            _ => return None,
        };
        Some(res)
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Decl::DefDecl(it) => &it.syntax,
            Decl::OpenDecl(it) => &it.syntax,
            Decl::TypeDecl(it) => &it.syntax,
        }
    }
}
impl From<TypeIdent> for TypeExpr {
    fn from(node: TypeIdent) -> TypeExpr {
        TypeExpr::TypeIdent(node)
    }
}
impl From<TypeArrow> for TypeExpr {
    fn from(node: TypeArrow) -> TypeExpr {
        TypeExpr::TypeArrow(node)
    }
}
impl From<TypeParen> for TypeExpr {
    fn from(node: TypeParen) -> TypeExpr {
        TypeExpr::TypeParen(node)
    }
}
impl AstNode for TypeExpr {
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            TYPE_IDENT | TYPE_ARROW | TYPE_PAREN => true,
            _ => false,
        }
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        let res = match syntax.kind() {
            TYPE_IDENT => TypeExpr::TypeIdent(TypeIdent { syntax }),
            TYPE_ARROW => TypeExpr::TypeArrow(TypeArrow { syntax }),
            TYPE_PAREN => TypeExpr::TypeParen(TypeParen { syntax }),
            _ => return None,
        };
        Some(res)
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            TypeExpr::TypeIdent(it) => &it.syntax,
            TypeExpr::TypeArrow(it) => &it.syntax,
            TypeExpr::TypeParen(it) => &it.syntax,
        }
    }
}
impl From<IdentExpr> for Expr {
    fn from(node: IdentExpr) -> Expr {
        Expr::IdentExpr(node)
    }
}
impl From<LiteralExpr> for Expr {
    fn from(node: LiteralExpr) -> Expr {
        Expr::LiteralExpr(node)
    }
}
impl From<LambdaExpr> for Expr {
    fn from(node: LambdaExpr) -> Expr {
        Expr::LambdaExpr(node)
    }
}
impl From<ParenExpr> for Expr {
    fn from(node: ParenExpr) -> Expr {
        Expr::ParenExpr(node)
    }
}
impl From<BinaryExpr> for Expr {
    fn from(node: BinaryExpr) -> Expr {
        Expr::BinaryExpr(node)
    }
}
impl From<BlockExpr> for Expr {
    fn from(node: BlockExpr) -> Expr {
        Expr::BlockExpr(node)
    }
}
impl AstNode for Expr {
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            IDENT_EXPR | LITERAL_EXPR | LAMBDA_EXPR | PAREN_EXPR | BINARY_EXPR | BLOCK_EXPR => true,
            _ => false,
        }
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        let res = match syntax.kind() {
            IDENT_EXPR => Expr::IdentExpr(IdentExpr { syntax }),
            LITERAL_EXPR => Expr::LiteralExpr(LiteralExpr { syntax }),
            LAMBDA_EXPR => Expr::LambdaExpr(LambdaExpr { syntax }),
            PAREN_EXPR => Expr::ParenExpr(ParenExpr { syntax }),
            BINARY_EXPR => Expr::BinaryExpr(BinaryExpr { syntax }),
            BLOCK_EXPR => Expr::BlockExpr(BlockExpr { syntax }),
            _ => return None,
        };
        Some(res)
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Expr::IdentExpr(it) => &it.syntax,
            Expr::LiteralExpr(it) => &it.syntax,
            Expr::LambdaExpr(it) => &it.syntax,
            Expr::ParenExpr(it) => &it.syntax,
            Expr::BinaryExpr(it) => &it.syntax,
            Expr::BlockExpr(it) => &it.syntax,
        }
    }
}
impl From<ExprStmt> for Stmt {
    fn from(node: ExprStmt) -> Stmt {
        Stmt::ExprStmt(node)
    }
}
impl From<LetStmt> for Stmt {
    fn from(node: LetStmt) -> Stmt {
        Stmt::LetStmt(node)
    }
}
impl AstNode for Stmt {
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            EXPR_STMT | LET_STMT => true,
            _ => false,
        }
    }
    fn cast(syntax: SyntaxNode) -> Option<Self> {
        let res = match syntax.kind() {
            EXPR_STMT => Stmt::ExprStmt(ExprStmt { syntax }),
            LET_STMT => Stmt::LetStmt(LetStmt { syntax }),
            _ => return None,
        };
        Some(res)
    }
    fn syntax(&self) -> &SyntaxNode {
        match self {
            Stmt::ExprStmt(it) => &it.syntax,
            Stmt::LetStmt(it) => &it.syntax,
        }
    }
}
impl AstToken for Literal {
    fn can_cast(kind: SyntaxKind) -> bool {
        LiteralKind::can_cast(kind)
    }
    fn cast(syntax: SyntaxToken) -> Option<Self> {
        let kind = LiteralKind::cast(syntax.kind())?;
        Some(Literal { syntax, kind })
    }
    fn syntax(&self) -> &SyntaxToken {
        &self.syntax
    }
}
impl LiteralKind {
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            INT | DUMMY_KW => true,
            _ => false,
        }
    }
    #[must_use]
    pub fn cast(kind: SyntaxKind) -> Option<Self> {
        let res = match kind {
            INT => Self::Int,
            DUMMY_KW => Self::DummyKw,
            _ => return None,
        };
        Some(res)
    }
}
impl Literal {
    #[must_use]
    pub fn kind(&self) -> LiteralKind {
        self.kind
    }
}
impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl AstToken for InfixSymbol {
    fn can_cast(kind: SyntaxKind) -> bool {
        InfixSymbolKind::can_cast(kind)
    }
    fn cast(syntax: SyntaxToken) -> Option<Self> {
        let kind = InfixSymbolKind::cast(syntax.kind())?;
        Some(InfixSymbol { syntax, kind })
    }
    fn syntax(&self) -> &SyntaxToken {
        &self.syntax
    }
}
impl InfixSymbolKind {
    fn can_cast(kind: SyntaxKind) -> bool {
        match kind {
            PLUS | MINUS | STAR | SLASH => true,
            _ => false,
        }
    }
    #[must_use]
    pub fn cast(kind: SyntaxKind) -> Option<Self> {
        let res = match kind {
            PLUS => Self::Plus,
            MINUS => Self::Minus,
            STAR => Self::Star,
            SLASH => Self::Slash,
            _ => return None,
        };
        Some(res)
    }
}
impl InfixSymbol {
    #[must_use]
    pub fn kind(&self) -> InfixSymbolKind {
        self.kind
    }
}
impl std::fmt::Display for InfixSymbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for Decl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for TypeExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for DefDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for OpenDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for TypeDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for Params {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for TypeAnnotation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for DefBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for BlockExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for TypeIdent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for TypeArrow {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for TypeParen {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for ExprStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for LetStmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for IdentExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for LiteralExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for LambdaExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for ParenExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for BinaryExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for AppExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
impl std::fmt::Display for Param {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self.syntax(), f)
    }
}
