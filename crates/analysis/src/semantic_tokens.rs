use line_index::LineCol;
use parser::{nodes, AstNode, SyntaxKind, SyntaxToken};

use crate::Document;

pub const SUPPORTED_TOKENS: &[lsp_types::SemanticTokenType] = &[
    lsp_types::SemanticTokenType::KEYWORD,
    lsp_types::SemanticTokenType::OPERATOR,
    lsp_types::SemanticTokenType::PARAMETER,
    lsp_types::SemanticTokenType::TYPE,
    lsp_types::SemanticTokenType::FUNCTION,
    lsp_types::SemanticTokenType::VARIABLE,
    lsp_types::SemanticTokenType::STRING,
    lsp_types::SemanticTokenType::NUMBER,
    lsp_types::SemanticTokenType::COMMENT,
    lsp_types::SemanticTokenType::NAMESPACE,
];

fn token_index(kind: lsp_types::SemanticTokenType) -> u32 {
    SUPPORTED_TOKENS
        .iter()
        .position(|t| *t == kind)
        .unwrap_or_else(|| panic!("Token {:?} should be added to supported tokens list", kind))
        as u32
}

struct SemanticTokensBuilder<'a> {
    line_index: &'a line_index::LineIndex,
    prev_pos: line_index::LineCol,
}

impl<'a> SemanticTokensBuilder<'a> {
    fn new(line_index: &'a line_index::LineIndex) -> Self {
        Self {
            line_index,
            prev_pos: line_index::LineCol { line: 0, col: 0 },
        }
    }

    fn next(&mut self, token: &SyntaxToken) -> Option<lsp_types::SemanticToken> {
        let start = token.text_range().start();
        let end = token.text_range().end();
        let length = (end - start).into();

        let prev = self.prev_pos;
        let start = self.line_index.line_col(start);

        if let Some(kind) = get_semantic_token_type(token) {
            self.prev_pos = start;
            Some(make_semantic_token(prev, start, length, kind))
        } else {
            None
        }
    }
}

pub fn get_semantic_tokens(doc: &Document) -> Vec<lsp_types::SemanticToken> {
    let mut builder = SemanticTokensBuilder::new(doc.get_line_index());

    doc.parsed()
        .syntax()
        .descendants_with_tokens()
        .filter_map(|node| node.as_token().and_then(|token| builder.next(token)))
        .collect()
}

fn make_semantic_token(
    prev: LineCol,
    start: LineCol,
    length: u32,
    kind: lsp_types::SemanticTokenType,
) -> lsp_types::SemanticToken {
    let delta = line_col_delta(prev, start);
    lsp_types::SemanticToken {
        delta_line: delta.line as u32,
        delta_start: delta.col as u32,
        length,
        token_type: token_index(kind),
        token_modifiers_bitset: 0,
    }
}

fn line_col_delta(prev: line_index::LineCol, next: line_index::LineCol) -> line_index::LineCol {
    line_index::LineCol {
        line: next.line - prev.line,
        col: {
            if prev.line == next.line {
                next.col - prev.col
            } else {
                next.col
            }
        },
    }
}

fn get_semantic_token_type(token: &SyntaxToken) -> Option<lsp_types::SemanticTokenType> {
    let parent = token.parent();
    let parent = parent.as_ref();
    match token.kind() {
        kw if kw.is_operator() => Some(lsp_types::SemanticTokenType::OPERATOR),
        kw if kw.is_keyword() => Some(lsp_types::SemanticTokenType::KEYWORD),
        SyntaxKind::COMMENT => Some(lsp_types::SemanticTokenType::COMMENT),
        SyntaxKind::INT => Some(lsp_types::SemanticTokenType::NUMBER),
        SyntaxKind::STRING => Some(lsp_types::SemanticTokenType::STRING),
        SyntaxKind::IDENT => match parent.map(|n| n.kind()) {
            Some(SyntaxKind::TYPE_IDENT | SyntaxKind::TYPE_DECL) => {
                Some(lsp_types::SemanticTokenType::TYPE)
            }
            Some(SyntaxKind::OPEN_DECL) => Some(lsp_types::SemanticTokenType::NAMESPACE),
            Some(SyntaxKind::PARAM) => Some(lsp_types::SemanticTokenType::PARAMETER),
            Some(SyntaxKind::DEF_DECL) => Some(lsp_types::SemanticTokenType::FUNCTION),
            Some(SyntaxKind::LET_EXPR)
                if parent.is_some_and(|n| {
                    nodes::LetExpr::cast(n.clone()).is_some_and(|let_expr| {
                        let_expr
                            .params()
                            .is_some_and(|params| params.params().next().is_some())
                    })
                }) =>
            {
                Some(lsp_types::SemanticTokenType::FUNCTION)
            }
            _ => Some(lsp_types::SemanticTokenType::VARIABLE),
        },
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use super::*;
    use crate::Document;

    fn debug_print_tokens(tokens: &[lsp_types::SemanticToken]) -> String {
        tokens
            .iter()
            .map(|token| {
                let end = token.delta_start + token.length;
                let token_type = SUPPORTED_TOKENS
                    .get(token.token_type as usize)
                    .unwrap_or_else(|| panic!("Unknown token type {}", token.token_type));
                format!(
                    "{:?}:{:?}..{:?} - {}\n",
                    token.delta_line,
                    token.delta_start,
                    end,
                    token_type.as_str()
                )
            })
            .collect::<Vec<String>>()
            .concat()
    }

    #[test]
    fn test_debug_print_tokens() {
        let tokens = vec![
            lsp_types::SemanticToken {
                delta_line: 0,
                delta_start: 0,
                length: 3,
                token_type: 0,
                token_modifiers_bitset: 0,
            },
            lsp_types::SemanticToken {
                delta_line: 0,
                delta_start: 4,
                length: 1,
                token_type: 1,
                token_modifiers_bitset: 0,
            },
        ];
        assert_eq!(
            debug_print_tokens(&tokens),
            "0:0..3 - keyword\n0:4..5 - operator\n"
        );
    }

    #[test]
    fn test_get_semantic_tokens_in_def_func() {
        let text = "def f g = g a 1;";
        let document = Document::new(text.to_string());
        let tokens = get_semantic_tokens(&document);

        let actual = debug_print_tokens(tokens.as_slice());

        expect![[r#"
            0:0..3 - keyword
            0:4..5 - function
            0:2..3 - parameter
            0:2..3 - operator
            0:2..3 - variable
            0:2..3 - variable
            0:2..3 - number
            0:1..2 - operator
        "#]]
        .assert_eq(&actual);
    }

    #[test]
    fn test_get_semantic_tokens_in_type_arrow() {
        let text = "type Tp = (X -> Y) -> Z;";
        let document = Document::new(text.to_string());
        let tokens = get_semantic_tokens(&document);

        let actual = debug_print_tokens(tokens.as_slice());

        expect![[r#"
            0:0..4 - keyword
            0:5..7 - type
            0:3..4 - operator
            0:2..3 - operator
            0:1..2 - type
            0:2..4 - operator
            0:3..4 - type
            0:1..2 - operator
            0:2..4 - operator
            0:3..4 - type
            0:1..2 - operator
        "#]]
        .assert_eq(&actual);
    }

    #[test]
    fn when_prev_ln_eq_next_ln_then_delta_ln_is_0_and_delta_col_is_next_col_minus_prev_col() {
        let prev = line_index::LineCol { line: 2, col: 1 };
        let next = line_index::LineCol { line: 2, col: 2 };
        assert_eq!(
            line_col_delta(prev, next),
            line_index::LineCol { line: 0, col: 1 }
        );

        let prev = line_index::LineCol { line: 2, col: 1 };
        let next = line_index::LineCol { line: 2, col: 1 };
        assert_eq!(
            line_col_delta(prev, next),
            line_index::LineCol { line: 0, col: 0 }
        );
    }

    #[test]
    fn when_prev_ln_neq_next_ln_then_delta_ln_is_next_ln_minus_prev_ln_and_delta_col_is_next_col() {
        let prev = line_index::LineCol { line: 0, col: 0 };
        let next = line_index::LineCol { line: 1, col: 1 };
        assert_eq!(
            line_col_delta(prev, next),
            line_index::LineCol { line: 1, col: 1 }
        );

        let prev = line_index::LineCol { line: 0, col: 2 };
        let next = line_index::LineCol { line: 2, col: 2 };
        assert_eq!(
            line_col_delta(prev, next),
            line_index::LineCol { line: 2, col: 2 }
        );

        let prev = line_index::LineCol { line: 1, col: 4 };
        let next = line_index::LineCol { line: 3, col: 3 };
        assert_eq!(
            line_col_delta(prev, next),
            line_index::LineCol { line: 2, col: 3 }
        );
    }
}
