use lsp_types::Position;

use crate::{text_range_to_lsp_range, Document};

fn range_of_syntax(doc: &Document, syntax: &parser::SyntaxElement) -> lsp_types::Range {
    let range = syntax.text_range();
    text_range_to_lsp_range(doc, range)
}

#[must_use]
fn collect_selection_ranges(doc: &Document, pos: Position) -> Vec<lsp_types::Range> {
    let mut top_syntax = doc.syntax_at(pos);
    let mut ranges = vec![range_of_syntax(doc, &top_syntax)];

    while let Some(parent) = top_syntax.parent() {
        let parent = parent.into();
        ranges.push(range_of_syntax(doc, &parent));
        top_syntax = parent;
    }

    ranges
}

#[must_use]
#[allow(clippy::missing_panics_doc)]
pub fn get_selection_ranges(doc: &Document, pos: Position) -> lsp_types::SelectionRange {
    let ranges = collect_selection_ranges(doc, pos);
    let mut ret = lsp_types::SelectionRange {
        range: *ranges.last().unwrap(),
        parent: None,
    };
    for &range in ranges.iter().rev().skip(1) {
        ret = lsp_types::SelectionRange {
            range,
            parent: Some(ret.into()),
        };
    }

    ret
}
