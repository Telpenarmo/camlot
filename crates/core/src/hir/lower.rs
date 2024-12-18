use parser::{nodes as ast, AstToken};

use crate::Name;

use super::{
    module::{Module, StoredInArena},
    Definition, DefinitionIdx, Expr, ExprIdx, Literal, Param, ParamIdx, Pattern, PatternIdx,
    TypeDefinition, TypeExpr, TypeExprIdx,
};

impl Module {
    pub fn lower_module(&mut self, ast: &ast::Module) {
        ast.module_items().for_each(|ast| match ast {
            ast::ModuleItem::Definition(ast) => {
                self.lower_definition(&ast);
            }
            ast::ModuleItem::Open(_ast) => {}
            ast::ModuleItem::TypeDefinition(ast) => {
                self.lower_type_definition(&ast).alloc(self, &ast);
            }
        });
    }

    fn lower_definition(&mut self, ast: &ast::Definition) -> DefinitionIdx {
        let params = self.lower_params(ast.params());
        let body = ast.def_body();
        let defn = {
            if let Some(block) = body.as_ref().and_then(ast::DefBody::block_expr) {
                self.lower_block(&block)
            } else {
                let expr = body.as_ref().and_then(ast::DefBody::expr);
                self.lower_expr(expr)
            }
        };

        let return_type = self.lower_type_annotation(ast.type_annotation());

        Definition {
            name: self.lower_ident(ast.ident_lit()),
            params,
            return_type,
            defn,
        }
        .alloc(self, ast)
    }

    fn lower_type_definition(&mut self, ast: &ast::TypeDefinition) -> TypeDefinition {
        let defn = self.lower_type_expr(ast.type_expr());

        TypeDefinition {
            name: self.lower_ident(ast.ident_lit()),
            defn,
        }
    }

    fn lower_params(&mut self, ast: Option<ast::Params>) -> Box<[ParamIdx]> {
        ast.map(|ast| ast.params().map(|ast| self.lower_param(&ast)).collect())
            .unwrap_or_default()
    }

    fn lower_param(&mut self, ast: &ast::Param) -> ParamIdx {
        let pattern = self.lower_pattern(&ast.pattern().expect("Empty param indicates parser bug"));

        let typ = self.lower_type_annotation(ast.type_annotation());

        Param { pattern, typ }.alloc(self, ast)
    }

    fn lower_pattern(&mut self, ast: &ast::Pattern) -> PatternIdx {
        match ast {
            ast::Pattern::IdentPattern(ident_pattern) => {
                Pattern::Ident(self.lower_ident(ident_pattern.ident_lit()))
            }
            ast::Pattern::UnderscorePattern(_) => Pattern::Wildcard,
            ast::Pattern::UnitPattern(_) => Pattern::Unit,
        }
        .alloc(self, ast)
    }

    fn lower_type_annotation(&mut self, ast: Option<ast::TypeAnnotation>) -> TypeExprIdx {
        match ast {
            Some(ast) => self.lower_type_expr(ast.type_expr()),
            None => self.alloc_missing(),
        }
    }

    fn lower_type_expr(&mut self, type_expr: Option<ast::TypeExpr>) -> TypeExprIdx {
        match type_expr {
            None => self.alloc_missing(),
            Some(type_expr) => match type_expr.clone() {
                ast::TypeExpr::TypeIdent(ast) => match ast.ident_lit() {
                    Some(name) => {
                        let name = self.name(name.text());
                        TypeExpr::IdentTypeExpr { name }.alloc(self, &type_expr)
                    }
                    None => self.alloc_missing(),
                },
                ast::TypeExpr::TypeArrow(ast) => {
                    let from = self.lower_type_expr(ast.from());
                    let to = self.lower_type_expr(ast.to());

                    TypeExpr::TypeArrow { from, to }.alloc(self, &type_expr)
                }
                ast::TypeExpr::TypeParen(ast) => self.lower_type_expr(ast.type_expr()),
            },
        }
    }

    fn curry<'a, T: Iterator<Item = &'a ParamIdx>, N: parser::AstNode>(
        &mut self,
        body: ExprIdx,
        params: T,
        return_type: &mut Option<TypeExprIdx>,
        whole_expr_ast: &N,
    ) -> ExprIdx {
        params.fold(body, |body, param| {
            let return_type = return_type.take().unwrap_or_else(|| self.alloc_missing());
            Expr::lambda_expr(*param, return_type, body).alloc(self, whole_expr_ast)
        })
    }

    fn lower_expr_defaulting_to_unit(&mut self, expr: Option<ast::Expr>) -> ExprIdx {
        if expr.is_some() {
            self.lower_expr(expr)
        } else {
            Expr::LiteralExpr(Literal::Unit).alloc_no_syntax(self)
        }
    }

    fn lower_expr(&mut self, expr: Option<ast::Expr>) -> ExprIdx {
        if expr.is_none() {
            return self.alloc_missing();
        }
        match expr.unwrap() {
            ast::Expr::IdentExpr(ast) => match ast.ident_lit() {
                Some(lit) => Expr::ident_expr(self.name(lit.text())).alloc(self, &ast),
                None => self.alloc_missing(),
            },
            ast::Expr::ParenExpr(ast) => {
                if let Some(app) = ast.app_expr() {
                    self.lower_app(&app)
                } else {
                    self.lower_expr_defaulting_to_unit(ast.expr())
                }
            }
            ast::Expr::LiteralExpr(ast) => match ast.literal() {
                Some(lit) => match lit.kind() {
                    ast::LiteralKind::Int => {
                        Expr::int_expr(lit.syntax().text().parse().expect("Invalid int literal"))
                            .alloc(self, &ast)
                    }

                    ast::LiteralKind::DummyKw => unreachable!(),
                },
                None => self.alloc_missing(),
            },
            ast::Expr::LambdaExpr(ast) => {
                let params = {
                    let params = self.lower_params(ast.params());
                    if params.is_empty() {
                        let typ = self.alloc_missing();
                        let pattern = Pattern::Wildcard.alloc_no_syntax(self);
                        Box::new([Param { pattern, typ }.alloc_no_syntax(self)])
                    } else {
                        params
                    }
                };
                let params_rev = params.iter().rev();

                let body = self.lower_expr(ast.body());

                let return_type = self.lower_type_annotation(ast.type_annotation());

                let mut return_type = Some(return_type);
                let e = self.curry(body, params_rev, &mut return_type, &ast);
                debug_assert!(
                    return_type.is_none(),
                    "Return type definition should be used"
                );
                e
            }
            ast::Expr::BlockExpr(ast) => self.lower_block(&ast),
            ast::Expr::BinaryExpr(_) => todo!("Binary expressions are not yet supported"),
        }
    }

    fn lower_app(&mut self, app: &ast::AppExpr) -> ExprIdx {
        let func = {
            if let Some(app) = app.app_func() {
                self.lower_app(&app)
            } else {
                self.lower_expr(app.func())
            }
        };

        let arg = self.lower_expr(app.arg());

        Expr::AppExpr { func, arg }.alloc(self, app)
    }

    fn lower_ident(&mut self, ident: Option<parser::SyntaxToken>) -> Name {
        ident.map_or(self.empty_name(), |ident| self.name(ident.text()))
    }

    fn lower_stmt(&mut self, ast: ast::Stmt, cont: ExprIdx) -> ExprIdx {
        match ast {
            ast::Stmt::ExprStmt(ast) => {
                let expr = self.lower_expr_defaulting_to_unit(ast.expr());
                let pat = Pattern::Unit.alloc(self, &ast);
                Expr::let_expr(pat, self.alloc_missing(), expr, cont).alloc(self, &ast)
            }
            ast::Stmt::LetStmt(ast) => {
                let pattern = match ast.pattern() {
                    Some(pat) => self.lower_pattern(&pat),
                    None => Pattern::Wildcard.alloc_no_syntax(self),
                };

                let params = self.lower_params(ast.params());
                let params_rev = params.iter().rev();

                let return_type = self.lower_type_annotation(ast.type_annotation());
                let mut return_type = Some(return_type);

                let defn = self.lower_expr(ast.def());
                let defn = self.curry(defn, params_rev, &mut return_type, &ast);

                let return_type = return_type.unwrap_or_else(|| self.alloc_missing());

                Expr::let_expr(pattern, return_type, defn, cont).alloc(self, &ast)
            }
        }
    }

    fn lower_block(&mut self, ast: &ast::BlockExpr) -> ExprIdx {
        let tail_expr = self.lower_expr_defaulting_to_unit(ast.tail_expr());

        let stmts: Vec<_> = ast.statements().collect();
        stmts
            .iter()
            .rev()
            .fold(tail_expr, |body, stmt| self.lower_stmt(stmt.clone(), body))
    }
}

#[cfg(test)]
mod tests {
    use crate::hir::module::{expr_deep_eq, type_expr_deep_eq};
    use crate::{Interner, ParamIdx, Pattern};

    use super::{Expr, Module, Param, StoredInArena};

    fn unannotated_param(module: &mut Module, name: &str) -> ParamIdx {
        let name = module.name(name);
        let typ = module.alloc_missing();
        let pattern = Pattern::Ident(name).alloc_no_syntax(module);
        Param { pattern, typ }.alloc_no_syntax(module)
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
    fn lower_lambda_missing_body() {
        let mut module = module();

        let param = unannotated_param(&mut module, "x");
        let typ = module.alloc_missing();
        let body = module.alloc_missing();
        Expr::lambda_expr(param, typ, body).alloc_no_syntax(&mut module);

        check_expr("\\x ->", &module);
    }

    #[test]
    fn lower_lambda_missing_params() {
        let mut module = module();

        let x = module.name("x");
        let body = Expr::ident_expr(x).alloc_no_syntax(&mut module);

        let typ = module.alloc_missing();
        let pattern = Pattern::Wildcard.alloc_no_syntax(&mut module);
        let param = Param { pattern, typ }.alloc_no_syntax(&mut module);

        Expr::lambda_expr(param, typ, body).alloc_no_syntax(&mut module);

        check_expr("\\-> x", &module);
    }

    #[test]
    fn lower_let_missing_defn() {
        let mut module = module();

        let param = unannotated_param(&mut module, "b");

        let d = module.name("d");
        let body = Expr::ident_expr(d).alloc_no_syntax(&mut module);

        let name = module.name("a");
        let typ = module.alloc_missing();
        let defn = module.alloc_missing();
        let defn = Expr::lambda_expr(param, typ, defn).alloc_no_syntax(&mut module);

        let pat = Pattern::Ident(name).alloc_no_syntax(&mut module);

        Expr::let_expr(pat, typ, defn, body).alloc_no_syntax(&mut module);

        check_expr("{ let a b; d }", &module);
    }
}
