use crate::{Module, TypeExpr};

impl Module {
    pub(super) fn fmt_lambda(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        e: &crate::hir::LambdaExpr,
        indent: usize,
    ) -> std::fmt::Result {
        f.write_str("\\")?;
        self.fmt_param(f, e.param)?;
        match *self.get_type_expr(e.return_type) {
            TypeExpr::TypeArrow { .. } => {
                f.write_str(" : ")?;
                f.write_str("(")?;
                self.fmt_type_expr(f, e.return_type)?;
                f.write_str(")")?;
            }
            TypeExpr::Missing => {}
            TypeExpr::IdentTypeExpr { .. } => {
                f.write_str(" : ")?;
                self.fmt_type_expr(f, e.return_type)?;
            }
        }
        f.write_str(" -> ")?;
        self.fmt_expr(f, e.body, false, indent)?;
        Ok(())
    }
}
