//! This is a generated file, please do not edit manually. Changes can be
//! made in codegeneration that lives in `xtask` top-level dir.

#![allow(
    bad_style,
    missing_docs,
    unreachable_pub,
    clippy::manual_non_exhaustive,
    clippy::match_like_matches_macro,
    clippy::enum_glob_use
)]
use logos::Logos;
#[doc = r" The kind of syntax node, e.g. `IDENT`, `USE_KW`, or `STRUCT`."]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Logos)]
#[repr(u16)]
pub enum SyntaxKind {
    #[doc(hidden)]
    TOMBSTONE,
    #[doc(hidden)]
    EOF,
    #[token("(")]
    L_PAREN,
    #[token(")")]
    R_PAREN,
    #[token("{")]
    L_BRACE,
    #[token("}")]
    R_BRACE,
    #[token(":")]
    COLON,
    #[token(";")]
    SEMICOLON,
    #[token(",")]
    COMMA,
    #[token("=")]
    EQUAL,
    #[token("->")]
    ARROW,
    #[token("+")]
    PLUS,
    #[token("-")]
    MINUS,
    #[token("*")]
    STAR,
    #[token("/")]
    SLASH,
    #[token("\\")]
    BACKSLASH,
    #[token("λ")]
    LAMBDA,
    #[regex("(?:0|[1-9][0-9]*)")]
    INT,
    #[regex("\"(?s:[^\"\\\\]|\\\\.)*\"")]
    STRING,
    #[regex("[_a-zA-Z][_a-zA-Z0-9]*")]
    IDENT,
    #[regex("[ \\t\\n\\r]+")]
    WHITESPACE,
    #[regex("#[^\\r\\n]*(\\r\\n|\\n)?")]
    COMMENT,
    #[token("def")]
    DEF_KW,
    #[token("open")]
    OPEN_KW,
    #[token("type")]
    TYPE_KW,
    #[token("let")]
    LET_KW,
    #[token("dummy")]
    DUMMY_KW,
    #[doc = r" Also acts as __LAST_TOKEN"]
    #[error]
    LEXING_ERROR,
    ERROR,
    MODULE,
    DEFINITION,
    OPEN,
    TYPE_DEFINITION,
    PARAMS,
    TYPE_ANNOTATION,
    DEF_BODY,
    BLOCK_EXPR,
    TYPE_IDENT,
    TYPE_ARROW,
    TYPE_PAREN,
    EXPR_STMT,
    LET_STMT,
    IDENT_EXPR,
    LITERAL_EXPR,
    LAMBDA_EXPR,
    PAREN_EXPR,
    BINARY_EXPR,
    APP_EXPR,
    PARAM,
    MODULE_ITEM,
    TYPE_EXPR,
    EXPR,
    STMT,
    LITERAL,
    INFIX_SYMBOL,
    #[doc(hidden)]
    __LAST,
}
use self::SyntaxKind::*;
impl SyntaxKind {
    #[must_use]
    pub fn is_keyword(self) -> bool {
        match self {
            DEF_KW | OPEN_KW | TYPE_KW | LET_KW | DUMMY_KW => true,
            _ => false,
        }
    }
    #[must_use]
    pub fn is_operator(self) -> bool {
        match self {
            L_PAREN | R_PAREN | L_BRACE | R_BRACE | COLON | SEMICOLON | COMMA | EQUAL | ARROW
            | PLUS | MINUS | STAR | SLASH | BACKSLASH | LAMBDA => true,
            _ => false,
        }
    }
    #[must_use]
    pub fn is_enum(self) -> bool {
        match self {
            MODULE_ITEM | TYPE_EXPR | EXPR | STMT | LITERAL | INFIX_SYMBOL => true,
            _ => false,
        }
    }
    #[must_use]
    pub fn is_trivial(self) -> bool {
        match self {
            WHITESPACE | COMMENT | LEXING_ERROR => true,
            _ => false,
        }
    }
    #[doc = r" Returns the corresponding [`SyntaxKind`] for the given raw value."]
    #[doc = r" # Panics"]
    #[doc = r" Panics if the raw value does not correspond to any `SyntaxKind`."]
    #[must_use]
    pub fn from_raw(r: u16) -> Self {
        assert!(r < Self::__LAST as u16);
        unsafe { std::mem::transmute(r) }
    }
    #[must_use]
    pub fn into_raw(self) -> u16 {
        self as u16
    }
}
