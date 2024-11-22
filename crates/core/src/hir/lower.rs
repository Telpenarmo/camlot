use parser::{nodes as ast, AstNode, AstToken};

use crate::Name;

use super::{
    module::{Missable, Module, StoredInArena},
    Definition, Expr, ExprIdx, Literal, Param, ParamIdx, Pattern, PatternIdx, TypeDefinition,
    TypeExpr, TypeExprIdx,
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

        self.alloc_param(Param { pattern, typ })
    }

    fn lower_pattern(&mut self, ast: &ast::Pattern) -> PatternIdx {
        let pat = match ast {
            ast::Pattern::IdentPattern(ident_pattern) => {
                Pattern::Ident(self.lower_ident(ident_pattern.ident_lit()))
            }
            ast::Pattern::UnderscorePattern(_) => Pattern::Wildcard,
            ast::Pattern::UnitPattern(_) => Pattern::Unit,
        };

        self.alloc_pattern(pat)
    }

    fn lower_type_annotation(&mut self, ast: Option<ast::TypeAnnotation>) -> TypeExprIdx {
        match ast {
            Some(ast) => self.lower_type_expr(ast.type_expr()),
            None => TypeExpr::alloc_missing(self),
        }
    }

    fn lower_type_expr(&mut self, type_expr: Option<ast::TypeExpr>) -> TypeExprIdx {
        match type_expr {
            None => TypeExpr::alloc_missing(self),
            Some(type_expr) => match type_expr.clone() {
                ast::TypeExpr::TypeIdent(ast) => match ast.ident_lit() {
                    Some(name) => {
                        let name = self.name(name.text());
                        TypeExpr::IdentTypeExpr { name }.alloc(self, &type_expr)
                    }
                    None => TypeExpr::alloc_missing(self),
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

    fn curry<'a, T: Iterator<Item = &'a ParamIdx>>(
        &mut self,
        body: ExprIdx,
        params: T,
        return_type: &mut Option<TypeExprIdx>,
        whole_expr_ast: &parser::SyntaxNode,
    ) -> ExprIdx {
        params.fold(body, |body, param| {
            let return_type = return_type
                .take()
                .unwrap_or_else(|| TypeExpr::alloc_missing(self));
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
            return Expr::alloc_missing(self);
        }
        match expr.unwrap() {
            ast::Expr::IdentExpr(ast) => match ast.ident_lit() {
                Some(lit) => Expr::ident_expr(self.name(lit.text())).alloc(self, ast.syntax()),
                None => Expr::alloc_missing(self),
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
                            .alloc(self, ast.syntax())
                    }

                    ast::LiteralKind::DummyKw => unreachable!(),
                },
                None => Expr::alloc_missing(self),
            },
            ast::Expr::LambdaExpr(ast) => {
                let params = {
                    let params = self.lower_params(ast.params());
                    if params.is_empty() {
                        let typ = TypeExpr::alloc_missing(self);
                        let pattern = self.alloc_pattern(Pattern::Wildcard);
                        Box::new([self.alloc_param(Param { pattern, typ })])
                    } else {
                        params
                    }
                };
                let params_rev = params.iter().rev();

                let body = self.lower_expr(ast.body());

                let return_type = self.lower_type_annotation(ast.type_annotation());

                let mut return_type = Some(return_type);
                let e = self.curry(body, params_rev, &mut return_type, ast.syntax());
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

        Expr::AppExpr { func, arg }.alloc(self, app.syntax())
    }

    fn lower_ident(&mut self, ident: Option<parser::SyntaxToken>) -> Name {
        ident.map_or(self.empty_name(), |ident| self.name(ident.text()))
    }

    fn lower_stmt(&mut self, ast: ast::Stmt, cont: ExprIdx) -> ExprIdx {
        match ast {
            ast::Stmt::ExprStmt(ast) => {
                let expr = self.lower_expr_defaulting_to_unit(ast.expr());
                let pat = self.alloc_pattern(Pattern::Unit);
                Expr::let_expr(pat, TypeExpr::alloc_missing(self), expr, cont)
                    .alloc(self, ast.syntax())
            }
            ast::Stmt::LetStmt(ast) => {
                let pattern = match ast.pattern() {
                    Some(pat) => self.lower_pattern(&pat),
                    None => self.alloc_pattern(Pattern::Wildcard),
                };

                let params = self.lower_params(ast.params());
                let params_rev = params.iter().rev();

                let return_type = self.lower_type_annotation(ast.type_annotation());
                let mut return_type = Some(return_type);

                let defn = self.lower_expr(ast.def());
                let defn = self.curry(defn, params_rev, &mut return_type, ast.syntax());

                let return_type = return_type.unwrap_or_else(|| TypeExpr::alloc_missing(self));

                Expr::let_expr(pattern, return_type, defn, cont).alloc(self, ast.syntax())
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

    use super::{Expr, Missable, Module, Param, StoredInArena, TypeExpr};

    fn unannotated_param(module: &mut Module, name: &str) -> ParamIdx {
        let param = {
            let name = module.name(name);
            let typ = TypeExpr::alloc_missing(module);
            let pattern = module.alloc_pattern(Pattern::Ident(name));
            Param { pattern, typ }
        };
        module.alloc_param(param)
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
        let typ = TypeExpr::alloc_missing(&mut module);
        let body = Expr::alloc_missing(&mut module);
        Expr::lambda_expr(param, typ, body).alloc_no_syntax(&mut module);

        check_expr("\\x ->", &module);
    }

    #[test]
    fn lower_lambda_missing_params() {
        let mut module = module();

        let x = module.name("x");
        let body = Expr::ident_expr(x).alloc_no_syntax(&mut module);

        let typ = TypeExpr::alloc_missing(&mut module);
        let pattern = module.alloc_pattern(Pattern::Wildcard);
        let param = Param { pattern, typ };

        let param = module.alloc_param(param);
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
        let typ = TypeExpr::alloc_missing(&mut module);
        let defn = Expr::alloc_missing(&mut module);
        let defn = Expr::lambda_expr(param, typ, defn).alloc_no_syntax(&mut module);

        let pat = module.alloc_pattern(Pattern::Ident(name));

        Expr::let_expr(pat, typ, defn, body).alloc_no_syntax(&mut module);

        check_expr("{ let a b; d }", &module);
    }
}
