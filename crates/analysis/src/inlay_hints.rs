use core::{GeneralizedLabels, TypeExpr};

use line_index::TextSize;
use parser::{nodes, AstNode};

impl crate::Document {
    #[must_use]
    pub fn get_inlay_hints(&self) -> Vec<lsp_types::InlayHint> {
        let generalized_labels = self.generalized_labels();

        let defns = self
            .defn_types()
            .iter()
            .flat_map(|(defn_idx, typ)| self.defn_inlay_hints(generalized_labels, defn_idx, *typ));

        let exprs = self.expr_types().iter().flat_map(|(expr, typ)| {
            self.expr_inlay_hints(generalized_labels, expr, *typ)
                .unwrap_or_default()
        });

        defns.chain(exprs).collect()
    }

    fn defn_inlay_hints(
        &self,
        generalized_labels: &GeneralizedLabels,
        defn_idx: core::DefinitionIdx,
        typ: core::TypeIdx,
    ) -> Vec<lsp_types::InlayHint> {
        let defn = &self.hir()[defn_idx];

        let defn_syntax = self
            .syntax::<_, parser::nodes::Definition>(defn_idx)
            .unwrap();

        let params = defn_syntax.params().unwrap();

        self.hints_for_function(&params, generalized_labels, defn.return_type, typ)
    }

    fn expr_inlay_hints(
        &self,
        generalized_labels: &GeneralizedLabels,
        expr: core::ExprIdx,
        _typ: core::TypeIdx,
    ) -> Option<Vec<lsp_types::InlayHint>> {
        match &self.hir()[expr] {
            // core::Expr::LambdaExpr(lambda) => {
            //     let syntax = &self.syntax::<_, parser::nodes::LambdaExpr>(expr)?;
            //     let parent_kind = syntax.syntax().parent().unwrap().kind();
            //     if parent_kind == parser::SyntaxKind::LET_STMT {
            //         return None;
            //     }
            //     let params = syntax.params().unwrap();
            //     let typ = self.expr_types().get(expr).unwrap();
            //     Some(hints_for_function(doc, &params, lambda.return_type, *typ))
            // }
            core::Expr::LetExpr(let_expr) => {
                let syntax = &self.syntax::<_, parser::nodes::LetStmt>(expr)?;
                let params = syntax.params().unwrap();
                let typ = self.expr_types().get(let_expr.defn).unwrap();
                Some(self.hints_for_function(
                    &params,
                    generalized_labels,
                    let_expr.return_type,
                    *typ,
                ))
            }
            _ => Some(vec![]),
        }
    }

    fn hints_for_function(
        &self,
        params: &parser::nodes::Params,
        generalized_labels: &GeneralizedLabels,
        return_type: core::TypeExprIdx,
        typ: core::TypeIdx,
    ) -> Vec<lsp_types::InlayHint> {
        let return_type_missing = matches!(self.hir()[return_type], TypeExpr::Missing);

        let mut typ = typ;

        let mut hints = Vec::new();

        for var in core::bound_variables(typ, self.types()) {
            let Some(label) = generalized_labels.get_label(self.names(), var) else {
                continue;
            };
            if !label.starts_with('#') {
                continue;
            }

            let label = format!("'{label} ",);
            let offset = params.syntax().text_range().start();
            hints.push(make_hint(self.line_index(), offset, label));
        }

        params.params().for_each(|param| {
            let ty = self.get_type(typ);
            if let core::Type::Arrow(lhs, rhs) = ty {
                if param.type_annotation().is_none()
                    && param
                        .pattern()
                        .is_some_and(|p| !matches!(p, nodes::Pattern::UnitPattern(_)))
                {
                    let param_typ = self.display_type(*lhs);
                    let offset = param.syntax().text_range().start();
                    hints.push(make_hint(self.line_index(), offset, "(".to_string()));
                    let label_end = format!(": {param_typ})");
                    let offset = param.syntax().text_range().end();
                    hints.push(make_hint(self.line_index(), offset, label_end));
                }
                typ = *rhs;
            } else {
                panic!("Expected arrow type, got {}", self.display_type(typ));
            }
        });

        if return_type_missing {
            let typ = self.display_type(typ);
            let label = format!(": {typ}");
            let offset = params.syntax().text_range().end();
            hints.push(make_hint(self.line_index(), offset, label));
        }

        hints
    }
}

fn make_hint(
    line_index: &line_index::LineIndex,
    offset: TextSize,
    label: String,
) -> lsp_types::InlayHint {
    let pos = line_index.line_col(offset);
    lsp_types::InlayHint {
        position: lsp_types::Position {
            line: pos.line,
            character: pos.col,
        },
        label: lsp_types::InlayHintLabel::String(label),
        kind: None,
        text_edits: None,
        tooltip: None,
        padding_left: None,
        padding_right: None,
        data: None,
    }
}
