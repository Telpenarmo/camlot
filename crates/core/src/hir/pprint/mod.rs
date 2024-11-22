mod pretty_lambda;
mod pretty_let;
mod pretty_type;

use std::fmt::Display;

use crate::hir::{Expr, ExprIdx};

use super::{LetExpr, Module, ParamIdx, Pattern, PatternIdx, TypeExpr};

impl Module {
    fn fmt_pattern(&self, f: &mut std::fmt::Formatter<'_>, patt: PatternIdx) -> std::fmt::Result {
        match self.get_pattern(patt) {
            Pattern::Ident(interned) => f.write_str(self.get_name(*interned)),
            Pattern::Wildcard => f.write_str("_"),
            Pattern::Unit => f.write_str("()"),
        }
    }

    fn fmt_param(&self, f: &mut std::fmt::Formatter<'_>, param: ParamIdx) -> std::fmt::Result {
        let param = self.get_param(param);
        if self[param.typ] == TypeExpr::Missing {
            self.fmt_pattern(f, param.pattern)
        } else {
            f.write_str("(")?;
            self.fmt_pattern(f, param.pattern)?;
            f.write_str(": ")?;
            self.fmt_type_expr(f, param.typ)?;
            f.write_str(")")
        }
    }

    fn fmt_expr_atomic(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        expr: ExprIdx,
        indent: usize,
    ) -> std::fmt::Result {
        match self[expr] {
            Expr::LambdaExpr(_) => {
                f.write_str("(")?;
                self.fmt_expr(f, expr, false, indent)?;
                f.write_str(")")
            }
            _ => self.fmt_expr(f, expr, false, indent),
        }
    }

    fn fmt_expr(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        expr: ExprIdx,
        already_in_block: bool,
        indent: usize,
    ) -> std::fmt::Result {
        match &self[expr] {
            Expr::Missing => f.write_str("_"),
            Expr::LiteralExpr(lit) => f.write_fmt(format_args!("{lit}")),
            &Expr::IdentExpr { name } => f.write_str(self.get_name(name)),
            &Expr::AppExpr { func, arg } => {
                f.write_str("(")?;
                self.fmt_expr_atomic(f, func, indent)?;
                f.write_str(" ")?;
                self.fmt_expr_atomic(f, arg, indent)?;
                f.write_str(")")
            }
            Expr::LambdaExpr(e) => self.fmt_lambda(f, e, indent),
            Expr::LetExpr(e) => self.fmt_let(f, e, indent, already_in_block),
        }
    }
}

impl Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut written_types = false;
        for (_, typ) in self.iter_type_definitions() {
            written_types = true;
            f.write_str("type ")?;
            f.write_str(self.get_name(typ.name))?;
            f.write_str(" = ")?;
            self.fmt_type_expr(f, typ.defn)?;
            f.write_str(";\n")?;
        }
        if written_types {
            f.write_str("\n")?;
        }
        for (_, defn) in self.iter_definitions() {
            f.write_str("def ")?;
            f.write_str(self.get_name(defn.name))?;
            for param in &defn.params {
                f.write_str(" ")?;
                self.fmt_param(f, *param)?;
            }
            if self[defn.return_type] != TypeExpr::Missing {
                f.write_str(" : ")?;
                self.fmt_type_expr(f, defn.return_type)?;
            }
            f.write_str(" = ")?;
            self.fmt_expr(f, defn.defn, false, 0)?;
            f.write_str(";\n")?;
        }
        Ok(())
    }
}

fn new_line(f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
    f.write_str("\n")?;
    f.write_str(&"    ".repeat(indent))
}

#[allow(dead_code)]
struct InModule<T>(Module, T);
