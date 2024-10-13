use std::{collections::HashMap, path::PathBuf};

use anyhow::Result;
use ast::{lower, AstEnumSrc, AstSrc, AstTokenEnumSrc};
use itertools::Itertools;
use kinds::{KindsSrc, TokenKind};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use ungrammar::Grammar;
use util::{
    ensure_file_contents, reformat, to_lower_snake_case, to_pascal_case, to_upper_snake_case,
};

mod ast;
mod kinds;
mod util;

pub(crate) fn generate_ungrammar() -> Result<()> {
    let grammar: Grammar = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../crates/parser/camlot.ungram"
    ))
    .parse()?;

    let mut kinds = kinds::kinds();
    let ast = lower(&kinds, &grammar);

    for token in grammar.tokens() {
        let token = &grammar[token];
        let token = &token.name.clone();
        if !kinds.is_token(token) {
            let name = to_upper_snake_case(token);
            eprintln!("implicit kw: {token}");
            kinds.define_token(&TokenKind::Keyword {
                code: token.to_owned(),
                name: format!("{name}_KW"),
            });
        }
    }
    for node in &ast.nodes {
        let name = to_upper_snake_case(&node.name);
        kinds.define_node(&name);
    }
    for enum_ in &ast.enums {
        let name = to_upper_snake_case(&enum_.name);
        kinds.define_node(&name);
    }
    for token_enum in &ast.token_enums {
        let name = to_upper_snake_case(&token_enum.name);
        kinds.define_node(&name);
    }

    let syntax_kinds = generate_syntax_kinds(&kinds, &ast)?;

    let nodes = generate_nodes(&kinds, &ast)?;
    ensure_file_contents(
        &PathBuf::from(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../crates/parser/src/generated/syntax_kinds.rs",
        )),
        &syntax_kinds,
    )?;
    ensure_file_contents(
        &PathBuf::from(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../crates/parser/src/generated/nodes.rs",
        )),
        &nodes,
    )?;
    Ok(())
}

fn generate_syntax_kinds(kinds: &KindsSrc, grammar: &AstSrc) -> Result<String> {
    let token_kinds = kinds.tokens().map(TokenKind::expand_kind);

    let keywords = kinds
        .tokens()
        .filter(|k| matches!(k, TokenKind::Keyword { name, .. } if name.ends_with("_KW")))
        .map(TokenKind::name)
        .map(|n| format_ident!("{n}"));

    let operators = kinds
        .tokens()
        .filter(|k| matches!(k, TokenKind::Keyword { name, .. } if !name.ends_with("_KW")))
        .map(TokenKind::name)
        .map(|n| format_ident!("{n}"));

    let nodes = kinds
        .nodes
        .iter()
        .map(|name| format_ident!("{}", name))
        .collect::<Vec<_>>();

    let enums = grammar
        .enums
        .iter()
        .map(|e| format_ident!("{}", to_upper_snake_case(&e.name)))
        .chain(
            grammar
                .token_enums
                .iter()
                .map(|e| format_ident!("{}", to_upper_snake_case(&e.name))),
        );

    let ast = quote! {
        #![allow(bad_style, missing_docs, unreachable_pub, clippy::manual_non_exhaustive, clippy::match_like_matches_macro, clippy::enum_glob_use)]
        use logos::Logos;

        /// The kind of syntax node, e.g. `IDENT`, `USE_KW`, or `STRUCT`.
        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Logos)]
        #[repr(u16)]
        pub enum SyntaxKind {
            #[doc(hidden)]
            TOMBSTONE,
            #[doc(hidden)]
            EOF,
            #(#token_kinds,)*
            /// Also acts as __LAST_TOKEN
            #[error]
            LEXING_ERROR,
            ERROR,
            #(#nodes,)*
            #[doc(hidden)]
            __LAST,
        }
        use self::SyntaxKind::*;

        impl SyntaxKind {
            #[must_use]
            pub fn is_keyword(self) -> bool {
                match self {
                    #(#keywords)|* => true,
                    _ => false,
                }
            }

            #[must_use]
            pub fn is_operator(self) -> bool {
                match self {
                    #(#operators)|* => true,
                    _ => false,
                }
            }

            #[must_use]
            pub fn is_enum(self) -> bool {
                match self {
                    #(#enums)|* => true,
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

            /// Returns the corresponding [`SyntaxKind`] for the given raw value.
            /// # Panics
            /// Panics if the raw value does not correspond to any `SyntaxKind`.
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
    };

    reformat(&ast.to_string())
}

fn get_method_parts(
    only_of_type: bool,
    field: &ast::Field,
    kinds: &KindsSrc,
    grammar: &AstSrc,
) -> (TokenStream, Option<TokenStream>) {
    let ty = field.ty();

    if field.is_many() {
        (
            quote!(AstChildren<#ty>),
            only_of_type.then_some(quote!(support::children(&self.syntax))),
        )
    } else if let Some(token_kind) = field.token_kind(kinds) {
        (
            quote!(Option<#ty>),
            Some(quote!(
                support::token(&self.syntax, #token_kind)
            )),
        )
    } else if field.is_token_enum(grammar) {
        (
            quote!(Option<#ty>),
            only_of_type.then_some(quote!(support::token_child(&self.syntax))),
        )
    } else {
        (
            quote!(Option<#ty>),
            only_of_type.then_some(quote!(support::child(&self.syntax))),
        )
    }
}

/*
What AST accessors does this generate?

For each node:

1. Group all its fields by their type,
2.a. If there is only one field of a type, as it most likely is,
    generate a method that returns first (hopefully the only) child of that type.
2.b otherwise, we deem the case too complex and fall back to a handwritten implementation,
    which is expected to live in `handwritten_ast` module in target crate.
 */

fn generate_field_method(
    only: bool,
    field: &ast::Field,
    kinds: &KindsSrc,
    grammar: &AstSrc,
    node_name: &str,
) -> TokenStream {
    let (result, method_body) = get_method_parts(only, field, kinds, grammar);
    let method_name = format_ident!("{}", field.method_name(kinds));

    let method_body = if let Some(body) = method_body {
        body
    } else {
        let name = format_ident!("{}_{}", node_name, method_name);
        quote!(crate::handwritten_ast::#name(&self.syntax))
    };

    quote! {
        #[must_use]
        pub fn #method_name(&self) -> #result {
            #method_body
        }
    }
}

enum OneOrMany<'a, T> {
    Single(&'a T),
    Multiple(Vec<&'a T>),
}

impl<'a, T> OneOrMany<'a, T> {
    fn push_field(&mut self, field: &'a T) {
        match self {
            OneOrMany::Single(prev_field) => {
                *self = OneOrMany::Multiple(vec![prev_field, field]);
            }
            OneOrMany::Multiple(fields) => fields.push(field),
        };
    }
}

fn generate_node(
    kinds: &KindsSrc,
    grammar: &AstSrc,
    node: &ast::AstNodeSrc,
) -> (TokenStream, TokenStream) {
    let name = format_ident!("{}", node.name);
    let kind = format_ident!("{}", to_upper_snake_case(&node.name));

    let mut fields_by_type: HashMap<_, OneOrMany<ast::Field>> = HashMap::new();
    for field in &node.fields {
        fields_by_type
            .entry(field.ty())
            .and_modify(|v| v.push_field(field))
            .or_insert(OneOrMany::Single(field));
    }

    let mut methods: Vec<TokenStream> = Vec::new();
    let node_name = to_lower_snake_case(&node.name);

    for one_or_many in fields_by_type
        .iter()
        .sorted_by_key(|(k, _)| *k)
        .map(|(_, v)| v)
    {
        match one_or_many {
            OneOrMany::Single(field) => {
                methods.push(generate_field_method(
                    true, field, kinds, grammar, &node_name,
                ));
            }
            OneOrMany::Multiple(fields) => {
                methods.extend(
                    &mut fields.iter().map(|field| {
                        generate_field_method(false, field, kinds, grammar, &node_name)
                    }),
                );
            }
        }
    }

    let node = quote! {
        #[pretty_doc_comment_placeholder_workaround]
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub struct #name {
            pub(crate) syntax: SyntaxNode,
        }

        impl #name {
            #(#methods)*
        }
    };
    let impls = quote! {
        impl AstNode for #name {
            fn can_cast(kind: SyntaxKind) -> bool {
                kind == #kind
            }
            fn cast(syntax: SyntaxNode) -> Option<Self> {
                if Self::can_cast(syntax.kind()) { Some(Self { syntax }) } else { None }
            }
            fn syntax(&self) -> &SyntaxNode { &self.syntax }
        }
    };

    (node, impls)
}

fn generate_enum_def(en: &AstEnumSrc) -> (TokenStream, TokenStream) {
    let variants: Vec<_> = en
        .variants
        .iter()
        .map(|var| format_ident!("{}", var))
        .collect();
    let name = format_ident!("{}", en.name);
    let kinds: Vec<_> = variants
        .iter()
        .map(|name| format_ident!("{}", to_upper_snake_case(&name.to_string())))
        .collect();
    let traits = en.traits.iter().map(|trait_name| {
        let trait_name = format_ident!("{}", trait_name);
        quote!(impl ast::#trait_name for #name {})
    });

    let ast_node = quote! {
        impl AstNode for #name {
            fn can_cast(kind: SyntaxKind) -> bool {
                match kind {
                    #(#kinds)|* => true,
                    _ => false,
                }
            }
            fn cast(syntax: SyntaxNode) -> Option<Self> {
                let res = match syntax.kind() {
                    #(
                    #kinds => #name::#variants(#variants { syntax }),
                    )*
                    _ => return None,
                };
                Some(res)
            }
            fn syntax(&self) -> &SyntaxNode {
                match self {
                    #(
                    #name::#variants(it) => &it.syntax,
                    )*
                }
            }
        }
    };

    let def = quote! {
        #[pretty_doc_comment_placeholder_workaround]
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub enum #name {
            #(#variants(#variants),)*
        }

        #(#traits)*
    };
    let impls = quote! {
        #(
            impl From<#variants> for #name {
                fn from(node: #variants) -> #name {
                    #name::#variants(node)
                }
            }
        )*
        #ast_node
    };

    (def, impls)
}

fn generate_token_def(kinds: &KindsSrc, en: &AstTokenEnumSrc) -> (TokenStream, TokenStream) {
    let variants: Vec<_> = en
        .variants
        .iter()
        .map(|token| {
            format_ident!(
                "{}",
                to_pascal_case(&kinds.token(token).expect("token exists").name())
            )
        })
        .collect();
    let name = format_ident!("{}", en.name);
    let kind_name = format_ident!("{}Kind", en.name);
    let kinds: Vec<_> = variants
        .iter()
        .map(|name| format_ident!("{}", to_upper_snake_case(&name.to_string())))
        .collect();

    let ast_node = quote! {
        impl AstToken for #name {
            fn can_cast(kind: SyntaxKind) -> bool {
                #kind_name::can_cast(kind)
            }
            fn cast(syntax: SyntaxToken) -> Option<Self> {
                let kind = #kind_name::cast(syntax.kind())?;
                Some(#name { syntax, kind })
            }
            fn syntax(&self) -> &SyntaxToken {
                &self.syntax
            }
        }

        impl #kind_name {
            fn can_cast(kind: SyntaxKind) -> bool {
                match kind {
                    #(#kinds)|* => true,
                    _ => false,
                }
            }

            #[must_use]
            pub fn cast(kind: SyntaxKind) -> Option<Self> {
                let res = match kind {
                    #(#kinds => Self::#variants,)*
                    _ => return None,
                };
                Some(res)
            }
        }
    };

    let def = quote! {
        #[pretty_doc_comment_placeholder_workaround]
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub struct #name { syntax: SyntaxToken, kind: #kind_name }

        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        pub enum #kind_name {
            #(#variants,)*
        }
    };

    let impls = quote! {
        #ast_node

        impl #name {
            #[must_use]
            pub fn kind(&self) -> #kind_name {
                self.kind
            }
        }

        impl std::fmt::Display for #name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                std::fmt::Display::fmt(self.syntax(), f)
            }
        }
    };

    (def, impls)
}

fn generate_nodes(kinds: &KindsSrc, grammar: &AstSrc) -> Result<String> {
    let (node_defs, node_boilerplate_impls): (Vec<_>, Vec<_>) = grammar
        .nodes
        .iter()
        .map(|node| generate_node(kinds, grammar, node))
        .unzip();

    let (enum_defs, enum_boilerplate_impls): (Vec<_>, Vec<_>) =
        grammar.enums.iter().map(generate_enum_def).unzip();

    let (token_enum_defs, token_enum_boilerplate_impls): (Vec<_>, Vec<_>) = grammar
        .token_enums
        .iter()
        .map(|en| generate_token_def(kinds, en))
        .unzip();

    let enum_names = grammar.enums.iter().map(|it| &it.name);
    let node_names = grammar.nodes.iter().map(|it| &it.name);

    let display_impls = enum_names
        .chain(node_names.clone())
        .map(|it| format_ident!("{}", it))
        .map(|name| {
            quote! {
                impl std::fmt::Display for #name {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                        std::fmt::Display::fmt(self.syntax(), f)
                    }
                }
            }
        });

    let ast = quote! {
        #![allow(non_snake_case, clippy::match_like_matches_macro, clippy::enum_glob_use)]

        use crate::{
            SyntaxNode, SyntaxToken, SyntaxKind::{self, *},
            ast::{AstNode, AstToken, AstChildren, support}
        };

        #(#node_defs)*
        #(#enum_defs)*
        #(#token_enum_defs)*
        #(#node_boilerplate_impls)*
        #(#enum_boilerplate_impls)*
        #(#token_enum_boilerplate_impls)*
        #(#display_impls)*
    };

    let ast = ast.to_string().replace("T ! [", "T![");

    let mut res = String::with_capacity(ast.len() * 2);

    let mut docs = grammar
        .nodes
        .iter()
        .map(|it| &it.doc)
        .chain(grammar.enums.iter().map(|it| &it.doc));

    for chunk in ast.split("# [pretty_doc_comment_placeholder_workaround] ") {
        res.push_str(chunk);
        if let Some(doc) = docs.next() {
            write_doc_comment(doc, &mut res);
        }
    }

    let res = reformat(&res)?;
    Ok(res.replace("#[derive", "\n#[derive"))
}

fn write_doc_comment(contents: &[String], dest: &mut String) {
    use std::fmt::Write;
    for line in contents {
        writeln!(dest, "///{line}").unwrap();
    }
}
