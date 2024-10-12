mod pretty_lambda;
mod pretty_let;
mod pretty_type;

use std::fmt::Display;

use crate::hir::{Expr, ExprIdx, Param};

use super::{LetExpr, Module, TypeExpr};

impl Module {
    fn fmt_param(&self, f: &mut std::fmt::Formatter<'_>, param: &Param) -> std::fmt::Result {
        if self.get_type_expr(param.typ) == &TypeExpr::Missing {
            f.write_str(self.get_name(param.name))
        } else {
            f.write_str("(")?;
            f.write_str(self.get_name(param.name))?;
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
        match self.get_expr(expr) {
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
        match self.get_expr(expr) {
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
        for typ in self.type_definitions.values() {
            written_types = true;
            f.write_str("type ")?;
            f.write_str(self.names.lookup(typ.name))?;
            f.write_str(" = ")?;
            self.fmt_type_expr(f, typ.defn)?;
            f.write_str(";\n")?;
        }
        if written_types {
            f.write_str("\n")?;
        }
        for defn in self.definitions.values() {
            f.write_str("def ")?;
            f.write_str(self.names.lookup(defn.name))?;
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

#[cfg(test)]
mod tests {
    use super::*;
    use expect_test::{self, expect};

    #[track_caller]
    #[inline]
    fn display_hir(src: &str) -> String {
        let ast = parser::parse(src);
        let mut module = Module::new();
        module.lower_module(&ast.module());
        format!("{module}")
    }

    #[test]
    fn test_module_display() {
        let actual = display_hir("def b { let x = 1; (f x) }");

        let expected = expect![[r"
            def b = {
                let x = 1;
                (f x)
            };
        "]];
        expected.assert_eq(&actual);
    }

    #[test]
    fn test_module_display_let() {
        let actual = display_hir(r"def f { let f (x: int): int = x; }");

        let expected = expect![[r"
            def f = {
                let f (x: int) : int = x;
                ()
            };
        "]];
        expected.assert_eq(&actual);
    }

    #[test]
    fn test_module_display_let_let() {
        let actual = display_hir(
            r"def f {
                let f (x: int) = x;
                let f x: int = x;
                (a b c)
            }",
        );

        let expected = expect![[r"
            def f = {
                let f (x: int) = x;
                let f x : int = x;
                ((a b) c)
            };
        "]];
        expected.assert_eq(&actual);
    }

    #[test]
    fn test_module_display_let_nested() {
        let actual = display_hir(
            r"def f {
                let g = {
                    let x : int = 1;
                };
                ()
            }",
        );

        let expected = expect![[r"
            def f = {
                let g = {
                    let x : int = 1;
                    ()
                };
                ()
            };
        "]];
        expected.assert_eq(&actual);
    }

    #[test]
    fn test_module_display_apply_to_block() {
        let actual = display_hir(r"def f = ({ let f x = x; f } 1)");

        let expected = expect![[r"
            def f = ({
                let f x = x;
                f
            } 1);
        "]];
        expected.assert_eq(&actual);
    }

    #[test]
    fn test_module_display_apply_to_lambda() {
        let actual = display_hir(r"def f = ((\x -> x) 1);");

        let expected = expect![[r"
            def f = ((\x -> x) 1);
        "]];
        expected.assert_eq(&actual);
    }

    #[test]
    fn test_module_display_apply_lambda() {
        let actual = display_hir(r"def g = (f (\x -> x));");

        let expected = expect![[r"
            def g = (f (\x -> x));
        "]];
        expected.assert_eq(&actual);
    }
}
