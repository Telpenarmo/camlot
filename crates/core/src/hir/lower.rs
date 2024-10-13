use crate::hir::{
    Definition, DefinitionIdx, Expr, ExprIdx, Open, OpenIdx, Param, TypeDefinition,
    TypeDefinitionIdx, TypeExpr, TypeExprIdx,
};
use crate::Name;
use parser::{nodes as ast, AstToken};

use super::module::Module;

impl Module {
    pub fn lower_module(&mut self, ast: &ast::Module) {
        ast.module_items().for_each(|ast| match ast {
            ast::ModuleItem::Definition(ast) => {
                let definition = self.lower_definition(&ast);
                self.definitions.alloc(definition);
            }
            ast::ModuleItem::Open(ast) => {
                let open = self.lower_open(&ast);
                self.opens.alloc(open);
            }
            ast::ModuleItem::TypeDefinition(ast) => {
                let type_definition = self.lower_type_definition(&ast);
                self.type_definitions.alloc(type_definition);
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

        let defn = if params.is_empty() {
            body
        } else {
            let body = self.alloc_expr(body);
            let return_type = self.lower_type_annotation(ast.type_annotation());
            let return_type = self.alloc_type_expr(return_type);
            self.curry(body, &params, return_type)
        };
        let defn = self.alloc_expr(defn);

        Definition {
            name: self.lower_ident(ast.ident_lit()),
            defn,
        }
    }

    fn lower_open(&mut self, ast: &ast::Open) -> Open {
        Open {
            path: self.lower_ident(ast.ident_lit()),
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
        let name = self.name(
            ast.ident_lit()
                .expect("Empty param indicates parser bug")
                .text(),
        );
        let typ = self.lower_type_annotation(ast.type_annotation());
        Param {
            name,
            typ: self.alloc_type_expr(typ),
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

    fn curry(&mut self, body: ExprIdx, params: &[Param], return_type: TypeExprIdx) -> Expr {
        let empty_param = Param {
            name: self.empty_name(),
            typ: self.alloc_type_expr(TypeExpr::Missing),
        };
        let tail_param = params.last().cloned().unwrap_or(empty_param);

        let body = Expr::lambda_expr(tail_param, return_type, body);

        params.iter().rev().skip(1).fold(body, |body, param| {
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

                let body = self.lower_expr(ast.body());
                let body = self.alloc_expr(body);

                let return_type = self.lower_type_annotation(ast.type_annotation());
                let return_type = self.alloc_type_expr(return_type);

                self.curry(body, &params, return_type)
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
        let name = ident.map(|ident| ident.text().into()).unwrap_or_default();
        self.names.intern(name)
    }

    fn lower_stmt(&mut self, ast: ast::Stmt, cont: ExprIdx) -> Expr {
        match ast {
            ast::Stmt::ExprStmt(ast) => {
                let expr = self.lower_expr_defaulting_to_unit(ast.expr());
                let expr = self.alloc_expr(expr);
                Expr::let_expr(
                    self.empty_name(),
                    vec![].into(),
                    self.alloc_type_expr(TypeExpr::Missing),
                    expr,
                    cont,
                )
            }
            ast::Stmt::LetStmt(ast) => {
                let name = self.lower_ident(ast.ident_lit());

                let params = self.lower_params(ast.params());

                let return_type = self.lower_type_annotation(ast.type_annotation());
                let return_type = self.alloc_type_expr(return_type);

                let defn = self.lower_expr(ast.def());
                let defn = self.alloc_expr(defn);

                Expr::let_expr(name, params, return_type, defn, cont)
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

    fn alloc_expr(&mut self, expr: Expr) -> ExprIdx {
        self.expressions.alloc(expr)
    }

    fn alloc_type_expr(&mut self, type_expr: TypeExpr) -> TypeExprIdx {
        self.type_expressions.alloc(type_expr)
    }

    fn empty_name(&mut self) -> Name {
        self.names.intern(String::new())
    }

    fn name<S: Into<String>>(&mut self, name: S) -> Name {
        self.names.intern(name.into())
    }

    pub(crate) fn get_expr(&self, idx: ExprIdx) -> &Expr {
        &self.expressions[idx]
    }

    pub(crate) fn get_type_expr(&self, idx: TypeExprIdx) -> &TypeExpr {
        &self.type_expressions[idx]
    }

    pub(crate) fn get_definition(&self, idx: DefinitionIdx) -> &Definition {
        &self.definitions[idx]
    }

    pub(crate) fn get_open(&self, idx: OpenIdx) -> &Open {
        &self.opens[idx]
    }

    pub(crate) fn get_type_definition(&self, idx: TypeDefinitionIdx) -> &TypeDefinition {
        &self.type_definitions[idx]
    }
}

#[cfg(test)]
mod tests {
    use crate::hir::module::{expr_deep_eq, type_expr_deep_eq};
    use crate::Literal;

    use super::{Definition, Expr, Module, Param, TypeExpr};

    fn unannotated_param(module: &mut Module, name: &str) -> Param {
        Param {
            name: module.name(name),
            typ: module.alloc_type_expr(TypeExpr::Missing),
        }
    }

    fn check_expr(text: &str, expected_module: &Module) {
        let module_syntax = parser::parse(&format!("def x = {text};")).module();
        let mut module = Module::default();

        module.lower_module(&module_syntax);

        module
            .expressions
            .iter()
            .zip(expected_module.expressions.iter())
            .for_each(|((actual, _), (expected, _))| {
                assert!(expr_deep_eq(&module, expected_module, actual, expected));
            });

        module
            .type_expressions
            .iter()
            .zip(expected_module.type_expressions.iter())
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
        let module = parser::parse("def f x y = 42;").module();
        let mut actual_module = Module::default();

        actual_module.lower_module(&module);

        let mut expected_module = Module::default();
        let body = expected_module.alloc_expr(Expr::int_expr(42));

        let x = unannotated_param(&mut expected_module, "x");
        let y = unannotated_param(&mut expected_module, "y");

        let return_type = expected_module.alloc_type_expr(TypeExpr::Missing);
        let body = expected_module.alloc_expr(Expr::lambda_expr(y, return_type, body));
        let return_type = expected_module.alloc_type_expr(TypeExpr::Missing);
        let defn = expected_module.alloc_expr(Expr::lambda_expr(x, return_type, body));

        let definition = Definition {
            name: expected_module.name("f"),
            defn,
        };

        expected_module.definitions.alloc(definition);

        assert_eq!(actual_module, expected_module);
    }

    #[test]
    fn lower_def_with_annotated_param() {
        let module = parser::parse("def f (x: int) = x;").module();
        let mut actual_module = Module::default();
        actual_module.lower_module(&module);

        let mut expected_module = Module::default();

        let x = expected_module.name("x");
        let int = expected_module.name("int");

        let int = expected_module.alloc_type_expr(TypeExpr::IdentTypeExpr { name: int });

        let param = Param { name: x, typ: int };
        let body = expected_module.alloc_expr(Expr::ident_expr(x));
        let ret_typ = expected_module.alloc_type_expr(TypeExpr::Missing);

        let defn = expected_module.alloc_expr(Expr::lambda_expr(param, ret_typ, body));

        let definition = Definition {
            name: expected_module.name("f"),
            defn,
        };
        expected_module.definitions.alloc(definition);

        assert_eq!(actual_module, expected_module);
    }

    #[test]
    fn lower_def_func_block() {
        let module = parser::parse("def f x y { 42 };").module();
        let mut actual_module = Module::default();
        actual_module.lower_module(&module);

        let mut expected_module = Module::default();
        let body = expected_module.alloc_expr(Expr::int_expr(42));

        let x = unannotated_param(&mut expected_module, "x");
        let y = unannotated_param(&mut expected_module, "y");

        let typ = expected_module.alloc_type_expr(TypeExpr::Missing);
        let body = expected_module.alloc_expr(Expr::lambda_expr(y, typ, body));

        let ret_typ = expected_module.alloc_type_expr(TypeExpr::Missing);

        let defn = expected_module.alloc_expr(Expr::lambda_expr(x, ret_typ, body));

        let definition = Definition {
            name: expected_module.name("f"),
            defn,
        };
        expected_module.definitions.alloc(definition);

        assert_eq!(actual_module, expected_module);
    }

    #[test]
    fn lower_ident() {
        let mut module = Module::default();
        let x = module.name("x");
        module.alloc_expr(Expr::ident_expr(x));
        check_expr("x", &module);
    }

    #[test]
    fn lower_app() {
        let mut module = Module::default();

        let f = module.name("f");
        let y = module.name("y");
        let func = module.alloc_expr(Expr::ident_expr(f));
        let arg = module.alloc_expr(Expr::ident_expr(y));
        module.alloc_expr(Expr::AppExpr { func, arg });

        check_expr("(f y)", &module);
    }

    #[test]
    fn lower_nested_app() {
        let mut module = Module::default();

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
        let mut module = Module::default();

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
        let mut expected_module = Module::default();

        let x = expected_module.name("x");
        let int = expected_module.name("int");
        expected_module.name("");

        let int = expected_module.alloc_type_expr(TypeExpr::IdentTypeExpr { name: int });
        let body = expected_module.alloc_expr(Expr::ident_expr(x));
        let param = Param { name: x, typ: int };
        let ret_typ = expected_module.alloc_type_expr(TypeExpr::Missing);
        let lambda = expected_module.alloc_expr(Expr::lambda_expr(param, ret_typ, body));

        let definition = Definition {
            name: expected_module.name("f"),
            defn: lambda,
        };
        expected_module.definitions.alloc(definition);

        let module = parser::parse("def f = \\(x: int) -> x;").module();
        let mut actual_module = Module::new();

        actual_module.lower_module(&module);

        assert_eq!(actual_module, expected_module);
    }

    #[test]
    fn lower_let() {
        let mut module = Module::default();

        let c = module.name("c");
        let d = module.name("d");

        let body = module.alloc_expr(Expr::ident_expr(d));
        let defn = module.alloc_expr(Expr::ident_expr(c));

        let name = module.name("a");
        let param = unannotated_param(&mut module, "b");

        let typ = module.alloc_type_expr(TypeExpr::Missing);

        module.alloc_expr(Expr::let_expr(
            name,
            vec![param].into_boxed_slice(),
            typ,
            defn,
            body,
        ));

        let module_syntax = parser::parse("def x { let a b = c; d }").module();
        let mut module = Module::default();

        module.lower_module(&module_syntax);

        assert_eq!(module.expressions, module.expressions);
        assert_eq!(module.type_expressions, module.type_expressions);
    }

    #[test]
    fn lower_lambda_missing_type() {
        let mut module = Module::default();

        let x = module.name("x");
        let body = module.alloc_expr(Expr::ident_expr(x));
        let param = unannotated_param(&mut module, "x");

        let typ = module.alloc_type_expr(TypeExpr::Missing);
        module.alloc_expr(Expr::lambda_expr(param, typ, body));

        check_expr("\\x -> x", &module);
    }

    #[test]
    fn lower_lambda_missing_body() {
        let mut module = Module::default();

        let param = unannotated_param(&mut module, "x");
        let typ = module.alloc_type_expr(TypeExpr::Missing);
        let body = module.alloc_expr(Expr::Missing);
        module.alloc_expr(Expr::lambda_expr(param, typ, body));

        check_expr("\\x ->", &module);
    }

    #[test]
    fn lower_lambda_missing_params() {
        let mut module = Module::default();

        let x = module.name("x");
        let body = module.alloc_expr(Expr::ident_expr(x));
        let param = unannotated_param(&mut module, "");
        let typ = module.alloc_type_expr(TypeExpr::Missing);
        module.alloc_expr(Expr::lambda_expr(param, typ, body));

        check_expr("\\-> x", &module);
    }

    #[test]
    fn lower_let_missing_defn() {
        let mut module = Module::default();

        let param = unannotated_param(&mut module, "b");
        let params = vec![param].into_boxed_slice();

        let d = module.name("d");
        let body = module.alloc_expr(Expr::ident_expr(d));

        let name = module.name("a");
        let typ = module.alloc_type_expr(TypeExpr::Missing);
        let defn = module.alloc_expr(Expr::Missing);
        module.alloc_expr(Expr::let_expr(name, params, typ, defn, body));

        check_expr("{ let a b; d }", &module);
    }

    #[test]
    fn lower_def_func_with_return_type() {
        let module = parser::parse("def f x y : Int = 42;").module();
        let mut actual_module = Module::new();

        actual_module.lower_module(&module);

        let mut expected_module = Module::default();
        let body = expected_module.alloc_expr(Expr::int_expr(42));

        let x = unannotated_param(&mut expected_module, "x");
        let y = unannotated_param(&mut expected_module, "y");

        let int = expected_module.name("Int");
        let return_type = expected_module.alloc_type_expr(TypeExpr::IdentTypeExpr { name: int });
        let body = expected_module.alloc_expr(Expr::lambda_expr(y, return_type, body));
        let typ = expected_module.alloc_type_expr(TypeExpr::Missing);
        let defn = expected_module.alloc_expr(Expr::lambda_expr(x, typ, body));
        let definition = Definition {
            name: expected_module.name("f"),
            defn,
        };
        expected_module.definitions.alloc(definition);

        assert_eq!(actual_module, expected_module);
    }

    #[test]
    fn lower_let_with_return_type() {
        let module = parser::parse("def f { let x : Int = 42; x }").module();
        let mut actual_module = Module::default();

        actual_module.lower_module(&module);

        let mut expected_module = Module::default();

        let x = expected_module.name("x");
        let int = expected_module.name("Int");
        let f = expected_module.name("f");

        let return_type = expected_module.alloc_type_expr(TypeExpr::IdentTypeExpr { name: int });

        let tail_expr = expected_module.alloc_expr(Expr::ident_expr(x));
        let let_body = expected_module.alloc_expr(Expr::int_expr(42));

        let defn = expected_module.alloc_expr(Expr::let_expr(
            x,
            vec![].into_boxed_slice(),
            return_type,
            let_body,
            tail_expr,
        ));

        let definition = Definition { name: f, defn };
        expected_module.definitions.alloc(definition);

        assert_eq!(actual_module, expected_module);
    }

    #[test]
    fn lower_empty_parentheses() {
        let mut module = Module::default();
        module.alloc_expr(Expr::LiteralExpr(Literal::Unit));

        check_expr("()", &module);
        check_expr("( )", &module);
    }

    #[test]
    fn lower_empty_braces() {
        let mut module = Module::default();
        module.alloc_expr(Expr::LiteralExpr(Literal::Unit));

        check_expr("{}", &module);
        check_expr("{ }", &module);
    }

    #[test]
    fn lower_block_with_only_expr_stmt() {
        let mut module = Module::default();

        let body = module.alloc_expr(Expr::LiteralExpr(Literal::Unit));
        let defn = module.alloc_expr(Expr::int_expr(42));

        let name = module.empty_name();

        let typ = module.alloc_type_expr(TypeExpr::Missing);
        module.alloc_expr(Expr::let_expr(name, Box::new([]), typ, defn, body));

        check_expr("{ 42; }", &module);
    }

    #[test]
    fn lower_block_with_only_let_stmt() {
        let mut module = Module::default();

        let body = module.alloc_expr(Expr::LiteralExpr(Literal::Unit));
        let defn = module.alloc_expr(Expr::int_expr(42));
        let x = module.name("x");

        let typ = module.alloc_type_expr(TypeExpr::Missing);
        module.alloc_expr(Expr::let_expr(x, Box::new([]), typ, defn, body));

        check_expr("{ let x = 42; }", &module);
    }
}
