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
        f.write_str(self.names.lookup(e.name))?;
        for param in &e.params {
            f.write_str(" ")?;
            self.fmt_param(f, param)?;
        }
        if *self.get_type_expr(e.return_type) != TypeExpr::Missing {
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

#[cfg(test)]
mod tests {
    use crate::{hir::pprint::InModule, Expr, LetExpr, Param, TypeExpr};

    use super::*;
    use expect_test::{self, expect};

    impl std::fmt::Debug for InModule<LetExpr> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            self.0.fmt_let(f, &self.1, 0, false)
        }
    }

    #[test]
    fn test_fmt_two_lets() {
        let mut module = Module::new();
        let a_name = module.names.intern("a".into());
        let a = module.expressions.alloc(Expr::IdentExpr { name: a_name });
        let b_name = module.names.intern("b".into());
        let b = module.expressions.alloc(Expr::IdentExpr { name: b_name });
        let c_name = module.names.intern("c".into());
        let c = module.expressions.alloc(Expr::IdentExpr { name: c_name });
        let int = module.names.intern("int".into());
        let int = module
            .type_expressions
            .alloc(TypeExpr::IdentTypeExpr { name: int });
        let params = vec![Param {
            name: b_name,
            typ: int,
        }]
        .into_boxed_slice();
        let int_to_int = module
            .type_expressions
            .alloc(TypeExpr::TypeArrow { from: int, to: int });
        let inner_let = Expr::let_expr(c_name, vec![].into_boxed_slice(), int_to_int, a, c);
        let inner_let = module.expressions.alloc(inner_let);
        let let_expr = LetExpr {
            name: a_name,
            params,
            return_type: int,
            defn: b,
            body: inner_let,
        };
        let expected = expect![[r"
            {
                let a (b: int) : int = b;
                let c : int -> int = a;
                c
            }"]];
        expected.assert_eq(&format!("{:?}", InModule(module, let_expr)));
    }

    #[test]
    fn test_fmt_nested_let() {
        let mut module = Module::new();
        let a_name = module.names.intern("a".into());
        let a = module.expressions.alloc(Expr::IdentExpr { name: a_name });
        let b_name = module.names.intern("b".into());
        let b = module.expressions.alloc(Expr::IdentExpr { name: b_name });
        let c_name = module.names.intern("c".into());
        let c = module.expressions.alloc(Expr::IdentExpr { name: c_name });
        let int = module.names.intern("int".into());
        let int = module
            .type_expressions
            .alloc(TypeExpr::IdentTypeExpr { name: int });
        let params = vec![Param {
            name: b_name,
            typ: int,
        }]
        .into_boxed_slice();
        let inner_let = Expr::let_expr(c_name, vec![].into_boxed_slice(), int, b, c);
        let inner_let = module.expressions.alloc(inner_let);
        let let_expr = LetExpr {
            name: a_name,
            params,
            return_type: int,
            defn: inner_let,
            body: a,
        };
        let expected = expect![[r"
            {
                let a (b: int) : int = {
                    let c : int = b;
                    c
                };
                a
            }"]];
        expected.assert_eq(&format!("{:?}", InModule(module, let_expr)));
    }

    #[test]
    fn test_fmt_let_without_return_type() {
        let mut module = Module::new();

        let a_name = module.names.intern("a".into());
        let b_name = module.names.intern("b".into());
        let a = module.expressions.alloc(Expr::IdentExpr { name: a_name });
        let b = module.expressions.alloc(Expr::IdentExpr { name: b_name });

        let int = module.names.intern("int".into());
        let int = module
            .type_expressions
            .alloc(TypeExpr::IdentTypeExpr { name: int });
        let missing_type = module.type_expressions.alloc(TypeExpr::Missing);

        let param = Param {
            name: b_name,
            typ: int,
        };
        let params = vec![param].into_boxed_slice();
        let let_expr = LetExpr {
            name: a_name,
            params,
            return_type: missing_type,
            defn: b,
            body: a,
        };
        let expected = expect![[r"
            {
                let a (b: int) = b;
                a
            }"]];
        expected.assert_eq(&format!("{:?}", InModule(module, let_expr)));
    }

    #[test]
    fn test_fmt_let_without_param_type() {
        let mut module = Module::new();

        let a_name = module.names.intern("a".into());
        let a = module.expressions.alloc(Expr::IdentExpr { name: a_name });
        let b_name = module.names.intern("b".into());
        let b = module.expressions.alloc(Expr::IdentExpr { name: b_name });

        let int = module.names.intern("int".into());
        let int = module
            .type_expressions
            .alloc(TypeExpr::IdentTypeExpr { name: int });
        let missing_type = module.type_expressions.alloc(TypeExpr::Missing);
        let param = Param {
            name: b_name,
            typ: missing_type,
        };
        let params = vec![param].into_boxed_slice();
        let let_expr = LetExpr {
            name: a_name,
            params,
            return_type: int,
            defn: b,
            body: a,
        };
        let expected = expect![[r"
            {
                let a b : int = b;
                a
            }"]];
        expected.assert_eq(&format!("{:?}", InModule(module, let_expr)));
    }
}
