use core::TypeDefinitionIdx;
use std::collections::HashMap;

use line_index::TextSize;
use parser::{nodes::ModuleItem, AstNode};

use crate::Environment;

impl crate::Document {
    pub(crate) fn type_env_at(&self, needle: TextSize) -> Environment {
        let mut env = HashMap::new();

        let hir = self.hir();
        let module = self.parsed().module();
        for it in module.module_items() {
            if let ModuleItem::TypeDefinition(ref def_syntax) = it {
                if it.syntax().text_range().contains_inclusive(needle) {
                    break;
                }
                if def_syntax.ident_lit().is_none() {
                    continue;
                };
                let defn: TypeDefinitionIdx = hir.idx_at(it.syntax()).unwrap();
                let defn = &hir[defn];

                env.insert(defn.name, def_syntax.ident_lit().unwrap());
            }
        }
        env
    }
}
