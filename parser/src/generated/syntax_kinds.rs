//! This is a generated file, please do not edit manually. Changes can be
//! made in codegeneration that lives in `xtask` top-level dir.

#![allow(
    bad_style,
    missing_docs,
    unreachable_pub,
    clippy::manual_non_exhaustive,
    clippy::match_like_matches_macro
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
    #[token("()")]
    EMPTY_PAREN,
    #[token("+")]
    PLUS,
    #[token("-")]
    MINUS,
    #[token("*")]
    STAR,
    #[token("/")]
    SLASH,
    #[regex("λ|\\\\")]
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
    #[token("let")]
    LET_KW,
    #[token("open")]
    OPEN_KW,
    #[token("type")]
    TYPE_KW,
    #[token("in")]
    IN_KW,
    #[doc = r" Also acts as __LAST_TOKEN"]
    #[error]
    LEXING_ERROR,
    ERROR,
    MODULE,
    LET_DECL,
    OPEN_DECL,
    TYPE_DECL,
    PARAMS,
    TYPE_IDENT,
    TYPE_ARROW,
    TYPE_PAREN,
    IDENT_EXPR,
    LITERAL_EXPR,
    APP_EXPR,
    LAMBDA_EXPR,
    LET_EXPR,
    PAREN_EXPR,
    BINARY_EXPR,
    PARAM,
    DECL,
    EXPR,
    TYPE_EXPR,
    LITERAL,
    INFIX_SYMBOL,
    #[doc(hidden)]
    __LAST,
}
use self::SyntaxKind::*;
impl SyntaxKind {
    pub fn is_keyword(self) -> bool {
        match self {
            L_PAREN | R_PAREN | COLON | SEMICOLON | COMMA | EQUAL | ARROW | EMPTY_PAREN | PLUS
            | MINUS | STAR | SLASH | LET_KW | OPEN_KW | TYPE_KW | IN_KW => true,
            _ => false,
        }
    }
    pub fn is_enum(self) -> bool {
        match self {
            DECL | EXPR | TYPE_EXPR | LITERAL | INFIX_SYMBOL => true,
            _ => false,
        }
    }
    pub fn is_trivial(self) -> bool {
        match self {
            WHITESPACE | COMMENT | LEXING_ERROR => true,
            _ => false,
        }
    }
    pub fn from_raw(r: u16) -> Self {
        assert!(r < Self::__LAST as u16);
        unsafe { std::mem::transmute(r) }
    }
    pub fn into_raw(self) -> u16 {
        self as u16
    }
}
