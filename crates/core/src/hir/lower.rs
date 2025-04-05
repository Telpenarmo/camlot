use std::{iter, slice};

use parser::{nodes as ast, AstToken};

use crate::{Interner, Name};

use super::{
    module::{HirNode, Module},
    Definition, DefinitionIdx, Expr, ExprIdx, Literal, Param, ParamIdx, Pattern, PatternIdx,
    TypeDefinition, TypeExpr, TypeExprIdx,
};

struct LoweringContext<'a> {
    module: &'a mut Module,
    names: &'a mut Interner<String>,
}

impl Module {
    pub fn lower_module(&mut self, names: &mut Interner<String>, ast: &ast::Module) {
        let mut ctx = LoweringContext {
            module: self,
            names,
        };
        ast.module_items().for_each(|ast| match ast {
            ast::ModuleItem::Definition(ast) => {
                ctx.lower_definition(&ast);
            }
            ast::ModuleItem::Open(_ast) => {}
            ast::ModuleItem::TypeDefinition(ast) => {
                ctx.lower_type_definition(&ast).alloc(ctx.module, &ast);
            }
        });
    }
}

impl LoweringContext<'_> {
    fn lower_definition(&mut self, ast: &ast::Definition) -> DefinitionIdx {
        let type_params = self.lower_type_params(ast.type_params());
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
            type_params,
        }
        .alloc(self.module, ast)
    }

    fn lower_type_definition(&mut self, ast: &ast::TypeDefinition) -> TypeDefinition {
        let defn = self.lower_type_expr(ast.type_expr());

        TypeDefinition {
            name: self.lower_ident(ast.ident_lit()),
            defn,
        }
    }

    fn lower_type_params(&mut self, ast: Option<ast::TypeParams>) -> Box<[Name]> {
        ast.map(|ast| {
            ast.type_params()
                .map(|ast| self.lower_ident(ast.ident_lit()))
                .collect()
        })
        .unwrap_or_default()
    }

    fn lower_params(&mut self, ast: Option<ast::Params>) -> Box<[ParamIdx]> {
        ast.map(|ast| ast.params().map(|ast| self.lower_param(&ast)).collect())
            .unwrap_or_default()
    }

    fn lower_param(&mut self, ast: &ast::Param) -> ParamIdx {
        let pattern = match ast.pattern() {
            Some(pattern) => self.lower_pattern(&pattern),
            None => Pattern::Wildcard.alloc_no_syntax(self.module),
        };

        let typ = self.lower_type_annotation(ast.type_annotation());

        Param { pattern, typ }.alloc(self.module, ast)
    }

    fn lower_pattern(&mut self, ast: &ast::Pattern) -> PatternIdx {
        match ast {
            ast::Pattern::IdentPattern(ident_pattern) => {
                Pattern::Ident(self.lower_ident(ident_pattern.ident_lit()))
            }
            ast::Pattern::UnderscorePattern(_) => Pattern::Wildcard,
            ast::Pattern::UnitPattern(_) => Pattern::Unit,
        }
        .alloc(self.module, ast)
    }

    fn lower_type_annotation(&mut self, ast: Option<ast::TypeAnnotation>) -> TypeExprIdx {
        match ast {
            Some(ast) => self.lower_type_expr(ast.type_expr()),
            None => self.module.alloc_missing(),
        }
    }

    fn lower_type_expr(&mut self, type_expr: Option<ast::TypeExpr>) -> TypeExprIdx {
        match type_expr {
            None => self.module.alloc_missing(),
            Some(type_expr) => match type_expr.clone() {
                ast::TypeExpr::TypeIdent(ast) => match ast.ident_lit() {
                    Some(name) => {
                        let name = self.names.name(name.text());
                        TypeExpr::IdentTypeExpr { name }.alloc(self.module, &type_expr)
                    }
                    None => self.module.alloc_missing(),
                },
                ast::TypeExpr::TypeArrow(ast) => {
                    let from = self.lower_type_expr(ast.from());
                    let to = self.lower_type_expr(ast.to());

                    TypeExpr::TypeArrow { from, to }.alloc(self.module, &type_expr)
                }
                ast::TypeExpr::TypeParen(ast) => self.lower_type_expr(ast.type_expr()),
            },
        }
    }

    fn curry_impl(
        &mut self,
        body: ExprIdx,
        innermost_param: ParamIdx,
        other_params: iter::Rev<slice::Iter<ParamIdx>>,
        return_type: TypeExprIdx,
    ) -> Expr {
        let inner = Expr::lambda_expr(innermost_param, return_type, body);
        other_params.fold(inner, |body, &param| {
            let body = body.alloc_no_syntax(self.module);
            Expr::lambda_expr(param, self.module.alloc_missing(), body)
        })
    }

    fn curry<N: parser::AstNode>(
        &mut self,
        body: ExprIdx,
        params: &[ParamIdx],
        return_type: &mut Option<TypeExprIdx>,
        whole_expr_ast: &N,
    ) -> ExprIdx {
        let mut params = params.iter().rev();
        if let Some(innermost_param) = params.next().copied() {
            let return_type = return_type
                .take()
                .unwrap_or_else(|| self.module.alloc_missing());
            self.curry_impl(body, innermost_param, params, return_type)
                .alloc(self.module, whole_expr_ast)
        } else {
            body
        }
    }

    fn lower_expr_defaulting_to_unit<N: parser::AstNode>(
        &mut self,
        expr: Option<ast::Expr>,
        parent: &N,
    ) -> ExprIdx {
        if expr.is_some() {
            self.lower_expr(expr)
        } else {
            Expr::LiteralExpr(Literal::Unit).alloc(self.module, parent)
        }
    }

    fn lower_expr(&mut self, expr: Option<ast::Expr>) -> ExprIdx {
        if expr.is_none() {
            return self.module.alloc_missing();
        }
        match expr.unwrap() {
            ast::Expr::IdentExpr(ast) => match ast.ident_lit() {
                Some(lit) => Expr::ident_expr(self.names.name(lit.text())).alloc(self.module, &ast),
                None => self.module.alloc_missing(),
            },
            ast::Expr::ParenExpr(ast) => {
                if let Some(app) = ast.app_expr() {
                    self.lower_app(&app)
                } else {
                    self.lower_expr_defaulting_to_unit(ast.expr(), &ast)
                }
            }
            ast::Expr::LiteralExpr(ast) => match ast.literal() {
                Some(lit) => match lit.kind() {
                    ast::LiteralKind::Int => {
                        Expr::int_expr(lit.syntax().text().parse().expect("Invalid int literal"))
                            .alloc(self.module, &ast)
                    }

                    ast::LiteralKind::DummyKw => unreachable!(),
                },
                None => self.module.alloc_missing(),
            },
            ast::Expr::LambdaExpr(ast) => {
                let params = {
                    let params = self.lower_params(ast.params());
                    if params.is_empty() {
                        let typ = self.module.alloc_missing();
                        let pattern = Pattern::Wildcard.alloc_no_syntax(self.module);
                        Box::new([Param { pattern, typ }.alloc_no_syntax(self.module)])
                    } else {
                        params
                    }
                };

                let body = self.lower_expr(ast.body());

                let return_type = self.lower_type_annotation(ast.type_annotation());

                let mut return_type = Some(return_type);
                let expr = self.curry(body, &params, &mut return_type, &ast);
                debug_assert!(
                    return_type.is_none(),
                    "Return type definition should be used"
                );
                expr
            }
            ast::Expr::BlockExpr(ast) => self.lower_block(&ast),
            ast::Expr::BinaryExpr(_) => panic!("Binary expressions are not yet supported"),
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

        Expr::AppExpr { func, arg }.alloc(self.module, app)
    }

    fn lower_ident(&mut self, ident: Option<parser::SyntaxToken>) -> Name {
        ident.map_or(self.names.empty_name(), |ident| {
            self.names.name(ident.text())
        })
    }

    fn lower_stmt(&mut self, ast: ast::Stmt, cont: ExprIdx) -> ExprIdx {
        match ast {
            ast::Stmt::ExprStmt(ast) => {
                let expr = self.lower_expr_defaulting_to_unit(ast.expr(), &ast);
                let pat = Pattern::Unit.alloc(self.module, &ast);
                let ret_type = self.module.alloc_missing();
                Expr::let_expr(pat, false, [].into(), ret_type, expr, cont).alloc(self.module, &ast)
            }
            ast::Stmt::LetStmt(ast) => {
                let pattern = match ast.pattern() {
                    Some(pat) => self.lower_pattern(&pat),
                    None => Pattern::Wildcard.alloc_no_syntax(self.module),
                };

                let rec = ast.rec_kw_token().is_some();

                let type_params = self.lower_type_params(ast.type_params());

                let params = self.lower_params(ast.params());

                let return_type = self.lower_type_annotation(ast.type_annotation());
                let mut return_type = Some(return_type);

                let defn = self.lower_expr(ast.def());
                let defn = self.curry(defn, &params, &mut return_type, &ast);

                let return_type = return_type.unwrap_or_else(|| self.module.alloc_missing());

                Expr::let_expr(pattern, rec, type_params, return_type, defn, cont)
                    .alloc(self.module, &ast) // possibly overriding HIR for `&ast`
            }
        }
    }

    fn lower_block(&mut self, ast: &ast::BlockExpr) -> ExprIdx {
        let tail_expr = self.lower_expr_defaulting_to_unit(ast.tail_expr(), ast);

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
    use crate::hir::pprint::ModuleAndNames;
    use crate::{Interner, ParamIdx, Pattern};

    use super::{Expr, HirNode, Module, Param};

    fn unannotated_param(
        module: &mut Module,
        names: &mut Interner<String>,
        name: &str,
    ) -> ParamIdx {
        let name = names.name(name);
        let typ = module.alloc_missing();
        let pattern = Pattern::Ident(name).alloc_no_syntax(module);
        Param { pattern, typ }.alloc_no_syntax(module)
    }

    fn module(names: &mut Interner<String>) -> Module {
        Module::new(names, &mut Interner::new())
    }

    #[inline]
    #[track_caller]
    fn lower(names: &mut Interner<String>, text: &str) -> Module {
        let module_syntax = parser::parse(text).module();
        let mut module = module(names);

        module.lower_module(names, &module_syntax);
        module
    }

    #[track_caller]
    fn check_expr(text: &str, names: &mut Interner<String>, expected_module: &Module) {
        let module = lower(names, &format!("def x = {text};"));

        module
            .iter_expressions()
            .zip(expected_module.iter_expressions())
            .for_each(|((actual, _), (expected, _))| {
                assert!(
                    expr_deep_eq(&module, expected_module, actual, expected),
                    "Actual:\n{}\n\nExpected:\n{}",
                    ModuleAndNames {
                        module: &module,
                        names
                    },
                    ModuleAndNames {
                        module: expected_module,
                        names
                    }
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
        let mut names = Interner::new();
        let mut module = module(&mut names);

        let param = unannotated_param(&mut module, &mut names, "x");
        let typ = module.alloc_missing();
        let body = module.alloc_missing();
        Expr::lambda_expr(param, typ, body).alloc_no_syntax(&mut module);

        check_expr("\\x ->", &mut names, &module);
    }

    #[test]
    fn lower_lambda_missing_params() {
        let mut names = Interner::new();
        let mut module = module(&mut names);

        let x = names.name("x");
        let body = Expr::ident_expr(x).alloc_no_syntax(&mut module);

        let typ = module.alloc_missing();
        let pattern = Pattern::Wildcard.alloc_no_syntax(&mut module);
        let param = Param { pattern, typ }.alloc_no_syntax(&mut module);

        Expr::lambda_expr(param, typ, body).alloc_no_syntax(&mut module);

        check_expr("\\-> x", &mut names, &module);
    }

    #[test]
    fn lower_let_missing_defn() {
        let mut names = Interner::new();
        let mut module = module(&mut names);

        let param = unannotated_param(&mut module, &mut names, "b");

        let d = names.name("d");
        let body = Expr::ident_expr(d).alloc_no_syntax(&mut module);

        let name = names.name("a");
        let typ = module.alloc_missing();
        let defn = module.alloc_missing();
        let defn = Expr::lambda_expr(param, typ, defn).alloc_no_syntax(&mut module);

        let pat = Pattern::Ident(name).alloc_no_syntax(&mut module);

        Expr::let_expr(pat, false, [].into(), typ, defn, body).alloc_no_syntax(&mut module);

        check_expr("{ let a b; d }", &mut names, &module);
    }
}
