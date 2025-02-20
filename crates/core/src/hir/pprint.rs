mod pretty_lambda;
mod pretty_let;
mod pretty_type;

use std::fmt::{self, Display, Formatter};

use crate::{
    hir::{Expr, ExprIdx},
    Interner,
};

use super::{LetExpr, Module, ParamIdx, Pattern, PatternIdx, TypeExpr};

struct ModulePrinter<'a, 'b> {
    module: &'a Module,
    names: &'a Interner<String>,
    f: &'a mut Formatter<'b>,
    indent: &'a mut usize,
}

impl ModulePrinter<'_, '_> {
    fn fmt_pattern(&mut self, patt: PatternIdx) -> fmt::Result {
        match self.module[patt] {
            Pattern::Ident(interned) => self.f.write_str(self.names.get_name(interned)),
            Pattern::Wildcard => self.f.write_str("_"),
            Pattern::Unit => self.f.write_str("()"),
        }
    }

    fn fmt_param(&mut self, param: ParamIdx) -> fmt::Result {
        let param = &self.module[param];
        if self.module[param.typ] == TypeExpr::Missing {
            self.fmt_pattern(param.pattern)
        } else {
            self.f.write_str("(")?;
            self.fmt_pattern(param.pattern)?;
            self.f.write_str(": ")?;
            self.fmt_type_expr(param.typ)?;
            self.f.write_str(")")
        }
    }

    fn fmt_expr_atomic(&mut self, expr: ExprIdx) -> fmt::Result {
        match self.module[expr] {
            Expr::LambdaExpr(_) => {
                self.f.write_str("(")?;
                self.fmt_expr(expr, false)?;
                self.f.write_str(")")
            }
            _ => self.fmt_expr(expr, false),
        }
    }

    fn fmt_expr(&mut self, expr: ExprIdx, already_in_block: bool) -> fmt::Result {
        match &self.module[expr] {
            Expr::Missing => self.f.write_str("_"),
            Expr::LiteralExpr(lit) => self.f.write_fmt(format_args!("{lit}")),
            &Expr::IdentExpr { name } => self.f.write_str(self.names.get_name(name)),
            &Expr::AppExpr { func, arg } => {
                self.f.write_str("(")?;
                self.fmt_expr_atomic(func)?;
                self.f.write_str(" ")?;
                self.fmt_expr_atomic(arg)?;
                self.f.write_str(")")
            }
            Expr::LambdaExpr(e) => self.fmt_lambda(e),
            Expr::LetExpr(e) => self.fmt_let(e, already_in_block),
        }
    }

    fn fmt_module(&mut self) -> fmt::Result {
        let mut written_types = false;
        for (_, typ) in self.module.iter_type_definitions() {
            written_types = true;
            self.f.write_str("type ")?;
            self.f.write_str(self.names.get_name(typ.name))?;
            self.f.write_str(" = ")?;
            self.fmt_type_expr(typ.defn)?;
            self.f.write_str(";\n")?;
        }
        if written_types {
            self.f.write_str("\n")?;
        }
        for (_, defn) in self.module.iter_definitions() {
            self.f.write_str("def ")?;
            self.f.write_str(self.names.get_name(defn.name))?;
            for param in &defn.params {
                self.f.write_str(" ")?;
                self.fmt_param(*param)?;
            }
            if self.module[defn.return_type] != TypeExpr::Missing {
                self.f.write_str(" : ")?;
                self.fmt_type_expr(defn.return_type)?;
            }
            self.f.write_str(" = ")?;
            self.fmt_expr(defn.defn, false)?;
            self.f.write_str(";\n")?;
        }
        Ok(())
    }
}

fn new_line(f: &mut Formatter<'_>, indent: usize) -> fmt::Result {
    f.write_str("\n")?;
    f.write_str(&"    ".repeat(indent))
}

#[allow(dead_code)]
struct InModule<T>(Module, T);

pub struct ModuleAndNames<'a> {
    pub module: &'a Module,
    pub names: &'a Interner<String>,
}

impl Display for ModuleAndNames<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut printer = ModulePrinter {
            module: self.module,
            names: self.names,
            f,
            indent: &mut 0,
        };

        printer.fmt_module()
    }
}
