use parser::{nodes as ast, AstToken};

use crate::Name;

use super::{
    module::Module, Definition, Expr, ExprIdx, Literal, Param, Pattern, TypeDefinition, TypeExpr,
    TypeExprIdx,
};

impl Module {
    pub fn lower_module(&mut self, ast: &ast::Module) {
        ast.module_items().for_each(|ast| match ast {
            ast::ModuleItem::Definition(ast) => {
                let definition = self.lower_definition(&ast);
                self.alloc_definition(definition);
            }
            ast::ModuleItem::Open(_ast) => {}
            ast::ModuleItem::TypeDefinition(ast) => {
                let type_definition = self.lower_type_definition(&ast);
                self.alloc_type_definition(type_definition);
            }
        });
    }

    fn lower_definition(&mut self, ast: &ast::Definition) -> Definition {
        let params = self.lower_params(ast.params());
        let body = ast.def_body();
        let body = {
            if let Some(block) = body.as_ref().and_then(ast::DefBody::block_expr) {
                self.lower_block(&block)
            } else {
                let expr = body.as_ref().and_then(ast::DefBody::expr);
                self.lower_expr(expr)
            }
        };

        let return_type = self.lower_type_annotation(ast.type_annotation());
        let return_type = self.alloc_type_expr(return_type);

        let defn = self.alloc_expr(body);

        Definition {
            name: self.lower_ident(ast.ident_lit()),
            params,
            return_type,
            defn,
        }
    }

    fn lower_type_definition(&mut self, ast: &ast::TypeDefinition) -> TypeDefinition {
        let defn = self.lower_type_expr(ast.type_expr());
        let defn = self.alloc_type_expr(defn);

        TypeDefinition {
            name: self.lower_ident(ast.ident_lit()),
            defn,
        }
    }

    fn lower_params(&mut self, ast: Option<ast::Params>) -> Box<[Param]> {
        ast.map(|ast| ast.params().map(|ast| self.lower_param(&ast)).collect())
            .unwrap_or_default()
    }

    fn lower_param(&mut self, ast: &ast::Param) -> Param {
        let pattern = self.lower_pattern(&ast.pattern().expect("Empty param indicates parser bug"));

        let typ = self.lower_type_annotation(ast.type_annotation());
        Param {
            pattern,
            typ: self.alloc_type_expr(typ),
        }
    }

    fn lower_pattern(&mut self, ast: &ast::Pattern) -> Pattern {
        match ast {
            ast::Pattern::IdentPattern(ident_pattern) => {
                Pattern::Ident(self.lower_ident(ident_pattern.ident_lit()))
            }
            ast::Pattern::UnderscorePattern(_) => Pattern::Wildcard,
            ast::Pattern::UnitPattern(_) => Pattern::Unit,
        }
    }

    fn lower_type_annotation(&mut self, ast: Option<ast::TypeAnnotation>) -> TypeExpr {
        ast.map_or(TypeExpr::Missing, |ast| {
            self.lower_type_expr(ast.type_expr())
        })
    }

    fn lower_type_expr(&mut self, type_expr: Option<ast::TypeExpr>) -> TypeExpr {
        if type_expr.is_none() {
            return TypeExpr::Missing;
        }
        match type_expr.unwrap() {
            ast::TypeExpr::TypeIdent(ast) => ast.ident_lit().map_or(TypeExpr::Missing, |name| {
                let name = self.name(name.text());
                TypeExpr::IdentTypeExpr { name }
            }),
            ast::TypeExpr::TypeArrow(ast) => {
                let from = self.lower_type_expr(ast.from());
                let from = self.alloc_type_expr(from);

                let to = self.lower_type_expr(ast.to());
                let to = self.alloc_type_expr(to);
                TypeExpr::TypeArrow { from, to }
            }
            ast::TypeExpr::TypeParen(ast) => self.lower_type_expr(ast.type_expr()),
        }
    }

    fn curry<'a, T: Iterator<Item = &'a Param>>(&mut self, body: Expr, params: T) -> Expr {
        params.fold(body, |body, param| {
            let body = self.alloc_expr(body);

            Expr::lambda_expr(param.clone(), self.alloc_type_expr(TypeExpr::Missing), body)
        })
    }

    fn lower_expr_defaulting_to_unit(&mut self, expr: Option<ast::Expr>) -> Expr {
        if expr.is_some() {
            self.lower_expr(expr)
        } else {
            Expr::LiteralExpr(Literal::Unit)
        }
    }

    fn lower_expr(&mut self, expr: Option<ast::Expr>) -> Expr {
        if expr.is_none() {
            return Expr::Missing;
        }
        match expr.unwrap() {
            ast::Expr::IdentExpr(ast) => ast.ident_lit().map_or(Expr::Missing, |ident| {
                Expr::ident_expr(self.name(ident.text()))
            }),
            ast::Expr::ParenExpr(ast) => {
                if let Some(app) = ast.app_expr() {
                    self.lower_app(&app)
                } else {
                    self.lower_expr_defaulting_to_unit(ast.expr())
                }
            }
            ast::Expr::LiteralExpr(ast) => {
                ast.literal().map_or(Expr::Missing, |lit| match lit.kind() {
                    ast::LiteralKind::Int => {
                        Expr::int_expr(lit.syntax().text().parse().expect("Invalid int literal"))
                    }

                    ast::LiteralKind::DummyKw => unreachable!(),
                })
            }
            ast::Expr::LambdaExpr(ast) => {
                let params = self.lower_params(ast.params());
                let mut params_rev = params.iter().rev();

                let body = self.lower_expr(ast.body());
                let body = self.alloc_expr(body);

                let return_type = self.lower_type_annotation(ast.type_annotation());
                let return_type = self.alloc_type_expr(return_type);

                let innermost_param = params_rev.next().cloned().unwrap_or_else(|| Param {
                    pattern: Pattern::Wildcard,
                    typ: self.alloc_type_expr(TypeExpr::Missing),
                });
                let innermost = Expr::lambda_expr(innermost_param, return_type, body);

                self.curry(innermost, params_rev)
            }
            ast::Expr::BlockExpr(ast) => self.lower_block(&ast),
            ast::Expr::BinaryExpr(_) => todo!("Binary expressions are not yet supported"),
        }
    }

    fn lower_app(&mut self, app: &ast::AppExpr) -> Expr {
        let func = {
            if let Some(app) = app.app_func() {
                self.lower_app(&app)
            } else {
                self.lower_expr(app.func())
            }
        };
        let func = self.alloc_expr(func);

        let arg = self.lower_expr(app.arg());
        let arg = self.alloc_expr(arg);

        Expr::AppExpr { func, arg }
    }

    fn lower_ident(&mut self, ident: Option<parser::SyntaxToken>) -> Name {
        ident.map_or(self.empty_name(), |ident| self.name(ident.text()))
    }

    fn lower_stmt(&mut self, ast: ast::Stmt, cont: ExprIdx) -> Expr {
        match ast {
            ast::Stmt::ExprStmt(ast) => {
                let expr = self.lower_expr_defaulting_to_unit(ast.expr());
                let expr = self.alloc_expr(expr);
                Expr::let_expr(
                    Pattern::Unit,
                    self.alloc_type_expr(TypeExpr::Missing),
                    expr,
                    cont,
                )
            }
            ast::Stmt::LetStmt(ast) => {
                let wildcard_pattern = Pattern::Wildcard;
                let pattern = ast
                        .pattern()
                    .map_or(wildcard_pattern, |ast| self.lower_pattern(&ast));

                let params = self.lower_params(ast.params());

                let return_type = self.lower_type_annotation(ast.type_annotation());
                let return_type = self.alloc_type_expr(return_type);

                let defn = self.lower_expr(ast.def());

                let mut params_rev = params.iter().rev();

                let (defn, return_type) = match params_rev.next() {
                    Some(tail_param) => {
                        let defn = self.alloc_expr(defn);
                        let defn = Expr::lambda_expr(tail_param.clone(), return_type, defn);
                        (defn, self.alloc_type_expr(TypeExpr::Missing))
                    }
                    None => (defn, return_type),
                };
                let defn = self.curry(defn, params_rev);
                let defn = self.alloc_expr(defn);

                Expr::let_expr(pattern, return_type, defn, cont)
            }
        }
    }

    fn lower_block(&mut self, ast: &ast::BlockExpr) -> Expr {
        let tail_expr = self.lower_expr_defaulting_to_unit(ast.tail_expr());

        let stmts: Vec<_> = ast.statements().collect();
        stmts.iter().rev().fold(tail_expr, |body, stmt| {
            let body = self.alloc_expr(body);
            self.lower_stmt(stmt.clone(), body)
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::hir::ident_param;
    use crate::hir::module::{expr_deep_eq, type_expr_deep_eq};
    use crate::{Interner, Literal, Pattern};

    use super::{Definition, Expr, Module, Param, TypeExpr};

    fn unannotated_param(module: &mut Module, name: &str) -> Param {
        ident_param(module.name(name), module.alloc_type_expr(TypeExpr::Missing))
    }

    fn module() -> Module {
        Module::new(&mut Interner::new())
    }

    #[inline]
    #[track_caller]
    fn lower(text: &str) -> Module {
        let module_syntax = parser::parse(text).module();
        let mut module = module();

        module.lower_module(&module_syntax);
        module
    }

    #[track_caller]
    fn check_expr(text: &str, expected_module: &Module) {
        let module = lower(&format!("def x = {text};"));

        module
            .iter_expressions()
            .zip(expected_module.iter_expressions())
            .for_each(|((actual, _), (expected, _))| {
                assert!(
                    expr_deep_eq(&module, expected_module, actual, expected),
                    "Actual:\n{module}\n\nExpected:\n{expected_module}"
                );
            });

        module
            .iter_type_expressions()
            .zip(expected_module.iter_type_expressions())
            .for_each(|((actual, _), (expected, _))| {
                assert!(type_expr_deep_eq(
                    &module,
                    expected_module,
                    actual,
                    expected
                ));
            });
    }

    #[test]
    fn lower_def_func_as_expr() {
        let actual_module = lower("def f x y = 42;");

        let mut expected_module = module();
        let defn = expected_module.alloc_expr(Expr::int_expr(42));

        let x = unannotated_param(&mut expected_module, "x");
        let y = unannotated_param(&mut expected_module, "y");

        let return_type = expected_module.alloc_type_expr(TypeExpr::Missing);

        let definition = Definition {
            name: expected_module.name("f"),
            params: vec![x, y].into_boxed_slice(),
            return_type,
            defn,
        };

        expected_module.alloc_definition(definition);

        assert_eq!(actual_module, expected_module);
    }

    #[test]
    fn lower_def_with_annotated_param() {
        let actual_module = lower("def f (x: int) = x;");

        let mut expected_module = module();
        let x = expected_module.name("x");
        let int = expected_module.name("int");

        let int = expected_module.alloc_type_expr(TypeExpr::IdentTypeExpr { name: int });

        let defn = expected_module.alloc_expr(Expr::ident_expr(x));
        let return_type = expected_module.alloc_type_expr(TypeExpr::Missing);
        let param = ident_param(x, int);

        let definition = Definition {
            name: expected_module.name("f"),
            params: vec![param].into_boxed_slice(),
            return_type,
            defn,
        };
        expected_module.alloc_definition(definition);

        assert_eq!(actual_module, expected_module);
    }

    #[test]
    fn lower_def_func_block() {
        let actual_module = lower("def f x y { 42 };");

        let mut expected_module = module();
        let defn = expected_module.alloc_expr(Expr::int_expr(42));

        let x = unannotated_param(&mut expected_module, "x");
        let y = unannotated_param(&mut expected_module, "y");

        let return_type = expected_module.alloc_type_expr(TypeExpr::Missing);

        let definition = Definition {
            name: expected_module.name("f"),
            params: vec![x, y].into_boxed_slice(),
            return_type,
            defn,
        };
        expected_module.alloc_definition(definition);

        assert_eq!(actual_module, expected_module);
    }

    #[test]
    fn lower_ident() {
        let mut module = module();
        let x = module.name("x");
        module.alloc_expr(Expr::ident_expr(x));
        check_expr("x", &module);
    }

    #[test]
    fn lower_app() {
        let mut module = module();

        let f = module.name("f");
        let y = module.name("y");
        let func = module.alloc_expr(Expr::ident_expr(f));
        let arg = module.alloc_expr(Expr::ident_expr(y));
        module.alloc_expr(Expr::AppExpr { func, arg });

        check_expr("(f y)", &module);
    }

    #[test]
    fn lower_nested_app() {
        let mut module = module();

        let f = module.name("f");
        let y = module.name("y");
        let z = module.name("z");

        let func = module.alloc_expr(Expr::ident_expr(f));
        let arg = module.alloc_expr(Expr::ident_expr(y));

        let func = module.alloc_expr(Expr::AppExpr { func, arg });
        let arg = module.alloc_expr(Expr::ident_expr(z));
        module.alloc_expr(Expr::AppExpr { func, arg });

        check_expr("(f y z)", &module);
    }

    #[test]
    fn lower_lambda() {
        let mut module = module();

        let x = module.name("x");
        let body = module.alloc_expr(Expr::ident_expr(x));
        let param = unannotated_param(&mut module, "y");

        let typ = module.alloc_type_expr(TypeExpr::Missing);
        let inner = module.alloc_expr(Expr::lambda_expr(param, typ, body));
        let param = unannotated_param(&mut module, "x");
        let typ = module.alloc_type_expr(TypeExpr::Missing);
        let _outer = module.alloc_expr(Expr::lambda_expr(param, typ, inner));

        check_expr("\\x y -> x", &module);
    }

    #[test]
    fn lower_lambda_with_annotated_param() {
        let actual_module = lower("def f = \\(x: int) -> x;");

        let mut expected_module = module();

        let x = expected_module.name("x");
        let int = expected_module.name("int");
        expected_module.name("");

        let int = expected_module.alloc_type_expr(TypeExpr::IdentTypeExpr { name: int });
        let body = expected_module.alloc_expr(Expr::ident_expr(x));
        let param = ident_param(x, int);
        let ret_typ = expected_module.alloc_type_expr(TypeExpr::Missing);
        let defn = expected_module.alloc_expr(Expr::lambda_expr(param, ret_typ, body));

        let return_type = expected_module.alloc_type_expr(TypeExpr::Missing);

        let definition = Definition {
            name: expected_module.name("f"),
            params: vec![].into_boxed_slice(),
            return_type,
            defn,
        };
        expected_module.alloc_definition(definition);

        assert_eq!(actual_module, expected_module);
    }

    #[test]
    fn lower_let() {
        let mut module = module();

        let c = module.name("c");
        let d = module.name("d");

        let body = module.alloc_expr(Expr::ident_expr(d));
        let defn = module.alloc_expr(Expr::ident_expr(c));

        let name = module.name("a");
        let param = unannotated_param(&mut module, "b");

        let missing_type = module.alloc_type_expr(TypeExpr::Missing);
        let body = module.alloc_expr(Expr::lambda_expr(param, missing_type, body));

        let typ = module.alloc_type_expr(TypeExpr::Missing);

        module.alloc_expr(Expr::ident_let(name, typ, defn, body));

        let actual_module = lower("def x { let a b = c; d }");

        assert_eq!(module, actual_module);
    }

    #[test]
    fn lower_lambda_missing_type() {
        let mut module = module();

        let x = module.name("x");
        let body = module.alloc_expr(Expr::ident_expr(x));
        let param = unannotated_param(&mut module, "x");

        let typ = module.alloc_type_expr(TypeExpr::Missing);
        module.alloc_expr(Expr::lambda_expr(param, typ, body));

        check_expr("\\x -> x", &module);
    }

    #[test]
    fn lower_lambda_missing_body() {
        let mut module = module();

        let param = unannotated_param(&mut module, "x");
        let typ = module.alloc_type_expr(TypeExpr::Missing);
        let body = module.alloc_expr(Expr::Missing);
        module.alloc_expr(Expr::lambda_expr(param, typ, body));

        check_expr("\\x ->", &module);
    }

    #[test]
    fn lower_lambda_missing_params() {
        let mut module = module();

        let x = module.name("x");
        let body = module.alloc_expr(Expr::ident_expr(x));

        let typ = module.alloc_type_expr(TypeExpr::Missing);
        let param = Param {
            pattern: crate::Pattern::Wildcard,
            typ,
        };
        module.alloc_expr(Expr::lambda_expr(param, typ, body));

        check_expr("\\-> x", &module);
    }

    #[test]
    fn lower_let_missing_defn() {
        let mut module = module();

        let param = unannotated_param(&mut module, "b");

        let d = module.name("d");
        let body = module.alloc_expr(Expr::ident_expr(d));

        let name = module.name("a");
        let typ = module.alloc_type_expr(TypeExpr::Missing);
        let defn = module.alloc_expr(Expr::Missing);

        let defn = module.alloc_expr(Expr::lambda_expr(param, typ, defn));

        module.alloc_expr(Expr::ident_let(name, typ, defn, body));

        check_expr("{ let a b; d }", &module);
    }

    #[test]
    fn lower_def_func_with_return_type() {
        let actual_module = lower("def f x y : Int = 42;");

        let mut expected_module = module();
        let defn = expected_module.alloc_expr(Expr::int_expr(42));

        let x = unannotated_param(&mut expected_module, "x");
        let y = unannotated_param(&mut expected_module, "y");

        let int = expected_module.name("Int");
        let return_type = expected_module.alloc_type_expr(TypeExpr::IdentTypeExpr { name: int });
        let definition = Definition {
            name: expected_module.name("f"),
            params: vec![x, y].into_boxed_slice(),
            return_type,
            defn,
        };
        expected_module.alloc_definition(definition);

        assert_eq!(actual_module, expected_module);
    }

    #[test]
    fn lower_annotated_def() {
        let actual_module = lower("def x : Int = 42;");

        let mut expected_module = module();

        let int = expected_module.name("Int");

        let int = expected_module.alloc_type_expr(TypeExpr::IdentTypeExpr { name: int });
        let defn = expected_module.alloc_expr(Expr::int_expr(42));

        let definition = Definition {
            name: expected_module.name("x"),
            params: vec![].into_boxed_slice(),
            return_type: int,
            defn,
        };
        expected_module.alloc_definition(definition);

        assert_eq!(actual_module, expected_module);
    }

    #[test]
    fn lower_let_with_return_type() {
        let actual_module = lower("def f { let x : Int = 42; x }");

        let mut expected_module = module();

        let x = expected_module.name("x");
        let int = expected_module.name("Int");
        let f = expected_module.name("f");

        let return_type = expected_module.alloc_type_expr(TypeExpr::IdentTypeExpr { name: int });

        let tail_expr = expected_module.alloc_expr(Expr::ident_expr(x));
        let let_body = expected_module.alloc_expr(Expr::int_expr(42));

        let defn = expected_module.alloc_expr(Expr::ident_let(x, return_type, let_body, tail_expr));

        let definition = Definition {
            name: f,
            params: vec![].into_boxed_slice(),
            return_type,
            defn,
        };
        expected_module.alloc_definition(definition);

        assert_eq!(actual_module, expected_module);
    }

    #[test]
    fn lower_empty_parentheses() {
        let mut module = module();
        module.alloc_expr(Expr::LiteralExpr(Literal::Unit));

        check_expr("()", &module);
        check_expr("( )", &module);
    }

    #[test]
    fn lower_empty_braces() {
        let mut module = module();
        module.alloc_expr(Expr::LiteralExpr(Literal::Unit));

        check_expr("{}", &module);
        check_expr("{ }", &module);
    }

    #[test]
    fn lower_block_with_only_expr_stmt() {
        let mut module = module();

        let body = module.alloc_expr(Expr::LiteralExpr(Literal::Unit));
        let defn = module.alloc_expr(Expr::int_expr(42));

        let return_type = module.alloc_type_expr(TypeExpr::Missing);

        module.alloc_expr(Expr::let_expr(Pattern::Unit, return_type, defn, body));

        check_expr("{ 42; }", &module);
    }

    #[test]
    fn lower_block_with_only_let_stmt() {
        let mut module = module();

        let body = module.alloc_expr(Expr::LiteralExpr(Literal::Unit));
        let defn = module.alloc_expr(Expr::int_expr(42));
        let x = module.name("x");

        let return_type = module.alloc_type_expr(TypeExpr::Missing);

        module.alloc_expr(Expr::ident_let(x, return_type, defn, body));

        check_expr("{ let x = 42; }", &module);
    }
}
