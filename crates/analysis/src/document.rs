pub struct Document {
    line_index: line_index::LineIndex,
    text: String,
}

impl Document {
    #[must_use]
    pub fn new(text: String) -> Document {
        let line_index = line_index::LineIndex::new(&text);
        Document { line_index, text }
    }

    pub fn update(&mut self, text: String) {
        self.text = text;
        self.line_index = line_index::LineIndex::new(&self.text);
    }

    #[must_use]
    pub fn parsed(&self) -> parser::Parse {
        parser::parse(&self.text)
    }

    pub(crate) fn get_line_index(&self) -> &line_index::LineIndex {
        &self.line_index
    }
}
