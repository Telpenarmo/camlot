use line_index::TextSize;
use parser::AstNode;

use crate::Document;

#[must_use]
pub fn get_inlay_hints(doc: &Document) -> Vec<lsp_types::InlayHint> {
    let defns = doc
        .defn_types()
        .iter()
        .flat_map(|(defn_idx, typ)| defn_inlay_hints(doc, defn_idx, *typ));

    let exprs = doc
        .expr_types()
        .iter()
        .flat_map(|(expr, typ)| expr_inlay_hints(doc, expr, *typ).unwrap_or_default());

    defns.chain(exprs).collect()
}

fn defn_inlay_hints(
    doc: &Document,
    defn_idx: core::DefinitionIdx,
    typ: core::TypeIdx,
) -> Vec<lsp_types::InlayHint> {
    let return_type = &doc.hir()[defn_idx].return_type;

    let defn_syntax = doc
        .syntax::<_, parser::nodes::Definition>(defn_idx)
        .unwrap();

    let params = defn_syntax.params().unwrap();

    hints_for_function(doc, &params, *return_type, typ)
}

fn expr_inlay_hints(
    doc: &Document,
    expr: core::ExprIdx,
    _typ: core::TypeIdx,
) -> Option<Vec<lsp_types::InlayHint>> {
    match &doc.hir()[expr] {
        // core::Expr::LambdaExpr(lambda) => {
        //     let syntax = &doc.syntax::<_, parser::nodes::LambdaExpr>(expr)?;
        //     let parent_kind = syntax.syntax().parent().unwrap().kind();
        //     if parent_kind == parser::SyntaxKind::LET_STMT {
        //         return None;
        //     }
        //     let params = syntax.params().unwrap();
        //     let typ = doc.expr_types().get(expr).unwrap();
        //     Some(hints_for_function(doc, &params, lambda.return_type, *typ))
        // }
        core::Expr::LetExpr(let_expr) => {
            let syntax = &doc.syntax::<_, parser::nodes::LetStmt>(expr)?;
            let params = syntax.params().unwrap();
            let typ = doc.expr_types().get(let_expr.defn).unwrap();
            Some(hints_for_function(doc, &params, let_expr.return_type, *typ))
        }
        _ => Some(vec![]),
    }
}

fn hints_for_function(
    doc: &Document,
    params: &parser::nodes::Params,
    return_type: core::TypeExprIdx,
    typ: core::TypeIdx,
) -> Vec<lsp_types::InlayHint> {
    let return_type_missing = matches!(doc.hir()[return_type], core::TypeExpr::Missing);

    let mut typ = typ;

    let mut hints = Vec::new();

    params.params().for_each(|param| {
        let ty = doc.get_type(typ);
        if let core::Type::Arrow(lhs, rhs) = ty {
            eprintln!("lhs: {lhs:?}, rhs: {rhs:?}");
            if param.type_annotation().is_none()
                && param
                    .pattern()
                    .is_some_and(|p| !matches!(p, parser::nodes::Pattern::UnitPattern(_)))
            {
                let param_typ = doc.display_type(*lhs);
                let offset = param.syntax().text_range().start();
                hints.push(make_hint(doc.get_line_index(), offset, "(".to_string()));
                let label_end = format!(": {param_typ})");
                let offset = param.syntax().text_range().end();
                hints.push(make_hint(doc.get_line_index(), offset, label_end));
            }
            typ = *rhs;
        } else {
            panic!("Expected arrow type, got {}", doc.display_type(typ));
        }
    });

    if return_type_missing {
        let typ = doc.display_type(typ);
        let label = format!(": {typ}");
        let offset = params.syntax().text_range().end();
        hints.push(make_hint(doc.get_line_index(), offset, label));
    }

    hints
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
