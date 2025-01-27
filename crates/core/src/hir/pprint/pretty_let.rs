use crate::TypeExpr;

use super::{new_line, LetExpr, ModulePrinter};

impl ModulePrinter<'_, '_> {
    pub(super) fn fmt_let(&mut self, e: &LetExpr, already_in_block: bool) -> std::fmt::Result {
        if !already_in_block {
            *self.indent += 1;
            self.f.write_str("{")?;
            new_line(self.f, *self.indent)?;
        }
        self.f.write_str("let ")?;
        self.fmt_pattern(e.lhs)?;
        if self.module[e.return_type] != TypeExpr::Missing {
            self.f.write_str(" : ")?;
            self.fmt_type_expr(e.return_type)?;
        }
        self.f.write_str(" = ")?;
        self.fmt_expr(e.defn, false)?;
        self.f.write_str(";")?;
        new_line(self.f, *self.indent)?;
        self.fmt_expr(e.body, true)?;
        if !already_in_block {
            *self.indent -= 1;
            new_line(self.f, *self.indent)?;
            self.f.write_str("}")?;
        }
        Ok(())
    }
}
