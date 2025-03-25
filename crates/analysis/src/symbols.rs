use core::{DefinitionIdx, TypeDefinitionIdx, TypeIdx};

use lsp_types::{DocumentSymbol, SymbolKind};
use parser::{nodes, AstNode};

impl crate::Document {
    #[must_use]
    pub fn get_symbols(&self) -> Vec<DocumentSymbol> {
        let defs = self
            .defn_types()
            .iter()
            .filter_map(|(defn_idx, type_idx)| self.defn_symbol(defn_idx, *type_idx));
        let types = self
            .hir()
            .type_definitions()
            .filter_map(|(idx, _)| self.type_definition_symbol(idx));
        defs.chain(types).collect()
    }

    fn defn_symbol(&self, defn_idx: DefinitionIdx, type_idx: TypeIdx) -> Option<DocumentSymbol> {
        let defn = &self.hir()[defn_idx];
        let name = self.names().lookup(defn.name).to_string();
        let typ = self.display_type(type_idx);
        let kind = if defn.params.is_empty() {
            SymbolKind::CONSTANT
        } else {
            SymbolKind::FUNCTION
        };

        let syntax: nodes::Definition = self.syntax(defn_idx).unwrap();
        let selection_range = self.text_range_to_lsp_range(syntax.ident_lit()?.text_range());

        let res = self.minimal_symbol(name, kind, syntax.syntax());
        Some(DocumentSymbol {
            detail: Some(typ),
            selection_range,
            ..res
        })
    }

    pub(crate) fn type_definition_symbol(&self, idx: TypeDefinitionIdx) -> Option<DocumentSymbol> {
        let defn = &self.hir()[idx];
        let name = self.names().lookup(defn.name).to_string();
        let syntax: nodes::TypeDefinition = self.syntax(idx).unwrap();
        let selection_range = self.text_range_to_lsp_range(syntax.ident_lit()?.text_range());

        let res = self.minimal_symbol(name, SymbolKind::VARIABLE, syntax.syntax());
        Some(DocumentSymbol {
            selection_range,
            ..res
        })
    }

    fn minimal_symbol(
        &self,
        name: String,
        kind: SymbolKind,
        syntax: &parser::SyntaxNode,
    ) -> DocumentSymbol {
        let range = self.text_range_to_lsp_range(syntax.text_range());

        #[allow(deprecated)]
        DocumentSymbol {
            name,
            detail: None,
            kind,
            tags: None,
            deprecated: None,
            range,
            selection_range: range,
            children: None,
        }
    }
}
