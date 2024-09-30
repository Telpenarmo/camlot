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
        f.write_str(self.get_name(e.name))?;
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
    use crate::{hir::pprint::InModule, Expr, Interner, LetExpr, Param, TypeExpr};

    use super::*;
    use expect_test::{self, expect};

    impl std::fmt::Debug for InModule<LetExpr> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            self.0.fmt_let(f, &self.1, 0, false)
        }
    }

    fn module() -> Module {
        Module::new(&mut Interner::new())
    }

    #[test]
    fn test_fmt_two_lets() {
        let mut module = module();
        let a_name = module.name("a");
        let a = module.alloc_expr(Expr::IdentExpr { name: a_name });
        let b_name = module.name("b");
        let b = module.alloc_expr(Expr::IdentExpr { name: b_name });
        let c_name = module.name("c");
        let c = module.alloc_expr(Expr::IdentExpr { name: c_name });
        let int = module.name("int");
        let int = module.alloc_type_expr(TypeExpr::IdentTypeExpr { name: int });
        let params = vec![Param {
            name: b_name,
            typ: int,
        }]
        .into_boxed_slice();
        let int_to_int = module.alloc_type_expr(TypeExpr::TypeArrow { from: int, to: int });
        let inner_let = Expr::let_expr(c_name, vec![].into_boxed_slice(), int_to_int, a, c);
        let inner_let = module.alloc_expr(inner_let);
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
        let mut module = module();
        let a_name = module.name("a");
        let a = module.alloc_expr(Expr::IdentExpr { name: a_name });
        let b_name = module.name("b");
        let b = module.alloc_expr(Expr::IdentExpr { name: b_name });
        let c_name = module.name("c");
        let c = module.alloc_expr(Expr::IdentExpr { name: c_name });
        let int = module.name("int");
        let int = module.alloc_type_expr(TypeExpr::IdentTypeExpr { name: int });
        let params = vec![Param {
            name: b_name,
            typ: int,
        }]
        .into_boxed_slice();
        let inner_let = Expr::let_expr(c_name, vec![].into_boxed_slice(), int, b, c);
        let inner_let = module.alloc_expr(inner_let);
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
        let mut module = module();

        let a_name = module.name("a");
        let b_name = module.name("b");
        let a = module.alloc_expr(Expr::IdentExpr { name: a_name });
        let b = module.alloc_expr(Expr::IdentExpr { name: b_name });

        let int = module.name("int");
        let int = module.alloc_type_expr(TypeExpr::IdentTypeExpr { name: int });
        let missing_type = module.alloc_type_expr(TypeExpr::Missing);

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
        let mut module = module();

        let a_name = module.name("a");
        let a = module.alloc_expr(Expr::IdentExpr { name: a_name });
        let b_name = module.name("b");
        let b = module.alloc_expr(Expr::IdentExpr { name: b_name });

        let int = module.name("int");
        let int = module.alloc_type_expr(TypeExpr::IdentTypeExpr { name: int });
        let missing_type = module.alloc_type_expr(TypeExpr::Missing);
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
