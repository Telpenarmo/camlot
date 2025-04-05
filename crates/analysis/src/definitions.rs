use parser::SyntaxKind;

use crate::position_to_offset;

impl crate::Document {
    #[must_use]
    pub fn get_definition(&self, pos: lsp_types::Position) -> Option<lsp_types::Range> {
        let syntax = self.syntax_at(pos)?;

        if !matches!(syntax.kind(), SyntaxKind::IDENT) {
            return None;
        }

        let binding = match syntax.parent()?.kind() {
            SyntaxKind::IDENT_EXPR => {
                let name = self.names().idx_of(syntax.text());
                let pos = position_to_offset(self.line_index(), pos);
                self.env_at(pos).get(&name).cloned()
            }
            #[allow(clippy::match_same_arms)]
            SyntaxKind::TYPE_IDENT => None,
            _ => None,
        }?;

        Some(self.text_range_to_lsp_range(binding.text_range()))
    }
}
