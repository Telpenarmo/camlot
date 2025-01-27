use crate::{TypeExpr, TypeExprIdx};

use super::ModulePrinter;

impl ModulePrinter<'_, '_> {
    pub(super) fn fmt_type_expr(&mut self, ty: TypeExprIdx) -> std::fmt::Result {
        match &self.module[ty] {
            TypeExpr::Missing => self.f.write_str("_"),
            &TypeExpr::IdentTypeExpr { name } => self.f.write_str(self.names.get_name(name)),
            &TypeExpr::TypeArrow { from, to } => {
                match self.module[from] {
                    TypeExpr::TypeArrow { .. } => {
                        self.f.write_str("(")?;
                        self.fmt_type_expr(from)?;
                        self.f.write_str(")")?;
                    }
                    _ => self.fmt_type_expr(from)?,
                }
                self.f.write_str(" -> ")?;
                self.fmt_type_expr(to)?;
                Ok(())
            }
        }
    }
}
