use crate::{Module, TypeExpr, TypeExprIdx};

impl Module {
    pub(super) fn fmt_type_expr(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        ty: TypeExprIdx,
    ) -> std::fmt::Result {
        match self.get_type_expr(ty) {
            TypeExpr::Missing => f.write_str("_"),
            &TypeExpr::IdentTypeExpr { name } => f.write_str(self.get_name(name)),
            &TypeExpr::TypeArrow { from, to } => {
                match self.get_type_expr(from) {
                    TypeExpr::TypeArrow { .. } => {
                        f.write_str("(")?;
                        self.fmt_type_expr(f, from)?;
                        f.write_str(")")?;
                    }
                    _ => self.fmt_type_expr(f, from)?,
                }
                f.write_str(" -> ")?;
                self.fmt_type_expr(f, to)?;
                Ok(())
            }
        }
    }
}
