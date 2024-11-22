use crate::TypeExpr;

use super::{new_line, LetExpr, Module};

impl Module {
    pub(super) fn fmt_let(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        e: &LetExpr,
        indent: usize,
        already_in_block: bool,
    ) -> std::fmt::Result {
        let mut indent = indent;
        if !already_in_block {
            indent += 1;
            f.write_str("{")?;
            new_line(f, indent)?;
        }
        f.write_str("let ")?;
        self.fmt_pattern(f, e.lhs)?;
        if self[e.return_type] != TypeExpr::Missing {
            f.write_str(" : ")?;
            self.fmt_type_expr(f, e.return_type)?;
        }
        f.write_str(" = ")?;
        self.fmt_expr(f, e.defn, false, indent)?;
        f.write_str(";")?;
        new_line(f, indent)?;
        self.fmt_expr(f, e.body, true, indent)?;
        if !already_in_block {
            indent -= 1;
            new_line(f, indent)?;
            f.write_str("}")?;
        }
        Ok(())
    }
}
