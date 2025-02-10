use std::collections::HashSet;

use crate::hir::{Expr, ExprIdx};
use crate::types::{Type, TypeIdx, Unique};
use crate::{DefinitionIdx, Interner, Name};

use super::{nth_ident, TypeInference};

fn next_ident(names: &mut Interner<String>, reserved: &mut HashSet<Name>, prefix: char) -> Name {
    let mut i = 0;
    loop {
        let name = nth_ident::nth_ident(i);
        let name = names.name(format!("{prefix}{name}"));
        if reserved.insert(name) {
            return name;
        }
        i += 1;
    }
}

impl TypeInference<'_> {
    fn assign_label(&mut self, reserved: &mut HashSet<Name>, prefix: char, tag: Unique) {
        self.generalized_labels
            .add_if_absent(tag, || next_ident(self.names, reserved, prefix));
    }

    fn assign_labels_in_type(&mut self, reserved: &mut HashSet<Name>, typ: TypeIdx) {
        match *self.types.get_type(typ) {
            Type::Bound(tag) => self.assign_label(reserved, '#', tag),
            Type::Skolem(skolem) => self.assign_label(reserved, '$', skolem.tag),
            Type::Unifier(unifier) => self.assign_label(reserved, '?', unifier.tag),

            Type::Arrow(l, r) => {
                self.assign_labels_in_type(reserved, l);
                self.assign_labels_in_type(reserved, r);
            }
            Type::Link(t, _) => self.assign_labels_in_type(reserved, t),
            Type::Int | Type::Bool | Type::Unit | Type::Error => (),
        }
    }

    fn assign_labels_in_subexprs(&mut self, reserved: &mut HashSet<Name>, expr: ExprIdx) {
        self.assign_labels_in_type(reserved, self.expr_types[expr]);
        match &self.module[expr] {
            Expr::LetExpr(let_expr) => {
                let mut reserved_in_let = reserved.clone();
                self.assign_labels_in_subexprs(&mut reserved_in_let, let_expr.defn);
                self.assign_labels_in_subexprs(reserved, let_expr.body);
            }
            Expr::LambdaExpr(lambda_expr) => {
                self.assign_labels_in_subexprs(reserved, lambda_expr.body);
            }
            &Expr::AppExpr { func, arg } => {
                self.assign_labels_in_subexprs(reserved, func);
                self.assign_labels_in_subexprs(reserved, arg);
            }
            Expr::IdentExpr { .. } | Expr::Missing | Expr::LiteralExpr(_) => {}
        }
    }

    pub(crate) fn assign_labels(&mut self, defn_idx: DefinitionIdx) {
        let defn = &self.module[defn_idx];
        let mut reserved = HashSet::new();
        self.assign_labels_in_type(&mut reserved, self.defn_types[defn_idx]);
        self.assign_labels_in_subexprs(&mut reserved, defn.defn);
    }
}
