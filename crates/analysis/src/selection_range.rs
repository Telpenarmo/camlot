use lsp_types::Position;

impl crate::Document {
    fn range_of_syntax(&self, syntax: &parser::SyntaxElement) -> lsp_types::Range {
        let range = syntax.text_range();
        self.text_range_to_lsp_range(range)
    }

    #[must_use]
    fn collect_selection_ranges(&self, pos: Position) -> Vec<lsp_types::Range> {
        let mut top_syntax = self.syntax_at(pos).expect("Empty file").into();
        let mut ranges = vec![self.range_of_syntax(&top_syntax)];

        while let Some(parent) = top_syntax.parent() {
            let parent = parent.into();
            ranges.push(self.range_of_syntax(&parent));
            top_syntax = parent;
        }

        ranges
    }

    /// # Panics
    #[must_use]
    pub fn get_selection_ranges(&self, pos: Position) -> lsp_types::SelectionRange {
        let ranges = self.collect_selection_ranges(pos);
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
}
