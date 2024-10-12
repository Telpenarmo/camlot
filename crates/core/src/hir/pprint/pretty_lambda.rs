use crate::{Expr, Module, TypeExpr};

impl Module {
    pub(super) fn fmt_lambda(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        e: &crate::hir::LambdaExpr,
        indent: usize,
    ) -> std::fmt::Result {
        f.write_str("\\")?;
        self.fmt_param(f, &e.param)?;
        let mut body = e.body;
        while let Expr::LambdaExpr(inner) = self.get_expr(body) {
            if *self.get_type_expr(inner.return_type) != TypeExpr::Missing {
                break;
            }
            f.write_str(" ")?;
            self.fmt_param(f, &inner.param)?;
            body = inner.body;
        }
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
        self.fmt_expr(f, body, false, indent)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::{hir::pprint::InModule, LambdaExpr, Param, TypeExpr};

    use super::*;
    use expect_test::{self, expect};

    impl std::fmt::Debug for InModule<LambdaExpr> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            self.0.fmt_lambda(f, &self.1, 0)
        }
    }

    #[test]
    fn test_lambda_display() {
        let mut module = Module::new();
        let a = module.names.intern("a".into());
        let int = module.names.intern("int".into());
        let int = module
            .type_expressions
            .alloc(TypeExpr::IdentTypeExpr { name: int });
        let param = Param { name: a, typ: int };
        let a = module.expressions.alloc(Expr::IdentExpr { name: a });
        let lambda = LambdaExpr {
            param,
            body: a,
            return_type: int,
        };
        let expected = &expect![[r"\(a: int) : int -> a"]];
        expected.assert_eq(&format!("{:?}", InModule(module, lambda)));
    }

    #[test]
    fn test_lambda_no_param_type() {
        let mut module = Module::new();
        let a = module.names.intern("a".into());
        let int = module.names.intern("int".into());
        let int = module
            .type_expressions
            .alloc(TypeExpr::IdentTypeExpr { name: int });
        let missing_type = module.type_expressions.alloc(TypeExpr::Missing);

        let param = Param {
            name: a,
            typ: missing_type,
        };
        let a = module.expressions.alloc(Expr::IdentExpr { name: a });
        let lambda = LambdaExpr {
            param,
            body: a,
            return_type: int,
        };
        let expected = &expect![[r"\a : int -> a"]];
        expected.assert_eq(&format!("{:?}", InModule(module, lambda)));
    }

    #[test]
    fn test_lambda_no_return_type() {
        let mut module = Module::new();

        let a = module.names.intern("a".into());
        let int = module.names.intern("int".into());
        let int = module
            .type_expressions
            .alloc(TypeExpr::IdentTypeExpr { name: int });
        let missing_type = module.type_expressions.alloc(TypeExpr::Missing);

        let param_a = Param { name: a, typ: int };
        let a = module.expressions.alloc(Expr::IdentExpr { name: a });
        let lambda = LambdaExpr {
            param: param_a,
            body: a,
            return_type: missing_type,
        };
        let expected = &expect![[r"\(a: int) -> a"]];
        expected.assert_eq(&format!("{:?}", InModule(module, lambda)));
    }

    #[test]
    fn test_lambda_two_params() {
        let mut module = Module::new();
        let a = module.names.intern("a".into());
        let b = module.names.intern("b".into());
        let int = module.names.intern("int".into());
        let int = module
            .type_expressions
            .alloc(TypeExpr::IdentTypeExpr { name: int });
        let missing_type = module.type_expressions.alloc(TypeExpr::Missing);
        let param_a = Param { name: a, typ: int };
        let param_b = Param { name: b, typ: int };
        let b = module.expressions.alloc(Expr::IdentExpr { name: b });
        let inner = Expr::lambda_expr(param_b, missing_type, b);
        let inner = module.expressions.alloc(inner);
        let lambda = LambdaExpr {
            param: param_a,
            body: inner,
            return_type: int,
        };
        let expected = &expect![[r"\(a: int) (b: int) : int -> b"]];
        expected.assert_eq(&format!("{:?}", InModule(module, lambda)));
    }

    #[test]
    fn test_lambda_nested() {
        let mut module = Module::new();

        let a = module.names.intern("a".into());
        let b = module.names.intern("b".into());
        let c = module.names.intern("c".into());

        let int = module.names.intern("int".into());
        let int = module
            .type_expressions
            .alloc(TypeExpr::IdentTypeExpr { name: int });

        let missing_type = module.type_expressions.alloc(TypeExpr::Missing);

        let param_a = Param { name: a, typ: int };
        let param_b = Param { name: b, typ: int };
        let param_c = Param { name: c, typ: int };

        let c = module.expressions.alloc(Expr::IdentExpr { name: c });
        let most_inner = Expr::lambda_expr(param_c, missing_type, c);
        let most_inner = module.expressions.alloc(most_inner);

        let inner = Expr::lambda_expr(param_b, missing_type, most_inner);
        let inner = module.expressions.alloc(inner);

        let lambda = LambdaExpr {
            param: param_a,
            body: inner,
            return_type: int,
        };
        let expected = &expect![[r"\(a: int) (b: int) (c: int) : int -> c"]];
        expected.assert_eq(&format!("{:?}", InModule(module, lambda)));
    }

    #[test]
    fn test_lambda_nested_with_inner_return_types() {
        let mut module = Module::new();

        let a = module.names.intern("a".into());
        let b = module.names.intern("b".into());

        let int = module.names.intern("int".into());
        let int = module
            .type_expressions
            .alloc(TypeExpr::IdentTypeExpr { name: int });

        let int_to_int = module
            .type_expressions
            .alloc(TypeExpr::TypeArrow { from: int, to: int });

        let param_a = Param { name: a, typ: int };
        let param_b = Param { name: b, typ: int };

        let b = module.expressions.alloc(Expr::IdentExpr { name: b });
        let inner = Expr::lambda_expr(param_b, int, b);
        let inner = module.expressions.alloc(inner);

        let lambda = LambdaExpr {
            param: param_a,
            body: inner,
            return_type: int_to_int,
        };
        let expected = &expect![[r"\(a: int) : (int -> int) -> \(b: int) : int -> b"]];
        expected.assert_eq(&format!("{:?}", InModule(module, lambda)));
    }
}
