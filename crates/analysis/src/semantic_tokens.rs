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
            Some(SyntaxKind::LET_DECL) => Some(lsp_types::SemanticTokenType::FUNCTION),
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
