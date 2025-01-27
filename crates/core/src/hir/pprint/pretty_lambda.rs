use crate::TypeExpr;

use super::ModulePrinter;

impl ModulePrinter<'_, '_> {
    pub(super) fn fmt_lambda(&mut self, e: &crate::hir::LambdaExpr) -> std::fmt::Result {
        self.f.write_str("\\")?;
        self.fmt_param(e.param)?;
        match self.module[e.return_type] {
            TypeExpr::TypeArrow { .. } => {
                self.f.write_str(" : ")?;
                self.f.write_str("(")?;
                self.fmt_type_expr(e.return_type)?;
                self.f.write_str(")")?;
            }
            TypeExpr::Missing => {}
            TypeExpr::IdentTypeExpr { .. } => {
                self.f.write_str(" : ")?;
                self.fmt_type_expr(e.return_type)?;
            }
        }
        self.f.write_str(" -> ")?;
        self.fmt_expr(e.body, false)?;
        Ok(())
    }
}
