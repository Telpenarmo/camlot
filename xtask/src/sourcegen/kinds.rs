#[derive(Debug)]
pub(crate) struct KindsSrc {
    /// Key - how this token appears in ungrammar
    defined_tokens: IndexMap<String, TokenKind>,
    defined_node_names: HashSet<String>,
    pub(crate) nodes: Vec<String>,
}

#[derive(Debug, Clone)]
pub(crate) enum TokenKind {
    /// Keyword - literal match of token
    Keyword {
        /// How this keyword appears in grammar/code, should be same as Kinds key
        code: String,
        name: String,
    },
    /// Literal - something defined by user, i.e strings, identifiers, smth
    Literal {
        /// How this keyword appears in grammar, should be same as Kinds key
        grammar_name: String,
        name: String,
        /// Regex for Logos lexer
        regex: String,
        /// Path to custom lexer
        lexer: Option<String>,
    },
}

impl TokenKind {
    pub(crate) fn grammar_name(&self) -> &str {
        match self {
            TokenKind::Keyword { code, .. } => code,
            TokenKind::Literal { grammar_name, .. } => grammar_name,
        }
    }
    /// How this keyword should appear in kinds enum, screaming snake cased
    pub(crate) fn name(&self) -> String {
        match self {
            TokenKind::Keyword { name, .. } => name.to_uppercase(),
            TokenKind::Literal { name, .. } => name.to_uppercase(),
        }
    }
    pub(crate) fn expand_kind(&self) -> TokenStream {
        let name = format_ident!("{}", self.name());
        let attr = match self {
            TokenKind::Keyword { code, .. } => quote! {#[token(#code)]},
            TokenKind::Literal { regex, lexer, .. } => {
                let lexer = lexer
                    .as_deref()
                    .map(TokenStream::from_str)
                    .map(|r| r.expect("path is correct"));
                quote! {#[regex(#regex, #lexer)]}
            }
        };
        quote! {
            #attr
            #name
        }
    }

    /// How this token should be referenced in code
    pub(crate) fn reference(&self) -> TokenStream {
        let name = self.name();
        let ident = format_ident!("{name}");
        quote! {#ident}
    }

    pub(crate) fn method_name(&self) -> Ident {
        match self {
            TokenKind::Keyword { name, .. } => {
                format_ident!("{}_token", name.to_lowercase())
            }
            TokenKind::Literal { name, .. } => {
                format_ident!("{}_lit", name.to_lowercase())
            }
        }
    }
}

#[macro_export]
macro_rules! define_kinds {
	($into:ident = lit($name:literal) => $regex:literal $(, $lexer:literal)? $(; $($rest:tt)*)?) => {{
		$into.define_token(TokenKind::Literal {
			grammar_name: format!($name),
			name: $name.to_owned(),
			regex: $regex.to_owned(),
			lexer: None $(.or_else(|| Some($lexer.to_string())))?,
		});
		$(define_kinds!($into = $($rest)*))?
	}};
	($into:ident = $tok:literal => $name:literal $(; $($rest:tt)*)?) => {{
		$into.define_token(TokenKind::Keyword {
			code: format!("{}", $tok),
			name: $name.to_owned(),
		});
		$(define_kinds!($into = $($rest)*))?
	}};
	($into:ident =) => {{}}
}
use std::{collections::HashSet, str::FromStr};

pub(crate) use define_kinds;
use indexmap::IndexMap;
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};

impl KindsSrc {
    pub(crate) fn new() -> Self {
        Self {
            defined_tokens: IndexMap::new(),
            defined_node_names: HashSet::new(),
            nodes: Vec::new(),
        }
    }
    pub(crate) fn define_token(&mut self, token: TokenKind) {
        assert!(
            self.defined_node_names.insert(token.name().to_owned()),
            "node name already defined: {}",
            token.name()
        );
        assert!(
            self.defined_tokens
                .insert(token.grammar_name().to_owned(), token.clone())
                .is_none(),
            "token already defined: {}",
            token.grammar_name()
        )
    }
    pub(crate) fn define_node(&mut self, node: &str) {
        assert!(
            self.defined_node_names.insert(node.to_owned()),
            "node name already defined: {}",
            node
        );
        self.nodes.push(node.to_string())
    }
    pub(crate) fn token(&self, tok: &str) -> Option<&TokenKind> {
        self.defined_tokens.get(tok)
    }
    pub(crate) fn is_token(&self, tok: &str) -> bool {
        self.defined_tokens.contains_key(tok)
    }
    pub(crate) fn tokens(&self) -> impl Iterator<Item = &TokenKind> {
        self.defined_tokens.iter().map(|(_, v)| v)
    }
}

pub(crate) fn kinds() -> KindsSrc {
    let mut kinds = KindsSrc::new();
    define_kinds![kinds =
        "(" => "L_Paren";
        ")" => "R_Paren";
        ":" => "Colon";
        ";" => "Semicolon";
        "," => "Comma";
        "=" => "Equal";
        "->" => "Arrow";
        "()" => "Empty_Paren";
        "+" => "Plus";
        "-" => "Minus";
        "*" => "Star";
        "/" => "Slash";
        "\\" => "Backslash";
        "λ" => "Lambda";

        // Literals
        lit("Int") => r"(?:0|[1-9][0-9]*)";
        lit("String") => "\"(?s:[^\"\\\\]|\\\\.)*\"";
        lit("Ident") => r"[_a-zA-Z][_a-zA-Z0-9]*";
        lit("Whitespace") => r"[ \t\n\r]+";
        lit("Comment") => r"#[^\r\n]*(\r\n|\n)?";
    ];
    kinds
}
