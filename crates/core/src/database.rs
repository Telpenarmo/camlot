use crate::{
    hir::{
        Declaration, DeclarationIdx, Expr, ExprIdx, Literal, Module, Param, TypeExpr, TypeExprIdx,
    },
    DefDecl, LetExpr,
};
use la_arena::Arena;
use parser::{nodes as ast, AstToken};

#[allow(unused)]
pub struct Database {
    declarations: Arena<Declaration>,
    expressions: Arena<Expr>,
    type_expressions: Arena<TypeExpr>,
}

#[allow(unreachable_code, unused)]
impl Default for Database {
    fn default() -> Self {
        Self::new()
    }
}

#[allow(unused)]
impl Database {
    #[must_use]
    pub fn new() -> Self {
        let declarations = Arena::new();
        let mut expressions = Arena::new();
        let mut type_expressions = Arena::new();

        expressions.alloc(Expr::Missing);
        type_expressions.alloc(TypeExpr::Missing);

        Self {
            declarations,
            expressions,
            type_expressions,
        }
    }

    pub(crate) fn lower_module(&mut self, ast: &ast::Module) -> Module {
        let declarations: Vec<DeclarationIdx> = ast
            .decls()
            .filter_map(|ast| {
                self.lower_decl(ast)
                    .map(|decl| self.declarations.alloc(decl))
            })
            .collect();
        let declarations = declarations.into_boxed_slice();
        Module { declarations }
    }

    fn lower_decl(&mut self, ast: ast::Decl) -> Option<Declaration> {
        Some(match ast {
            ast::Decl::DefDecl(ast) => {
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
                    self.curry(body, &params)
                };
                let defn = self.alloc_expr(defn);

                Declaration::DefDecl(Box::new(DefDecl {
                    name: Self::lower_ident(ast.ident_lit())?,
                    defn,
                }))
            }
            ast::Decl::OpenDecl(ast) => Declaration::OpenDecl {
                path: Self::lower_ident(ast.ident_lit())?,
            },
            ast::Decl::TypeDecl(ast) => {
                let defn = self.lower_type_expr(ast.type_expr());
                let defn = self.alloc_type_expr(defn);
                Declaration::TypeDecl {
                    name: Self::lower_ident(ast.ident_lit())?,
                    defn,
                }
            }
        })
    }

    fn lower_params(&mut self, ast: Option<ast::Params>) -> Box<[Param]> {
        ast.map(|ast| ast.params().map(|ast| self.lower_param(&ast)).collect())
            .unwrap_or_default()
    }

    fn lower_param(&mut self, ast: &ast::Param) -> Param {
        let typ = ast.type_expr().map(|typ| self.lower_type_expr(Some(typ)));
        let typ = typ.map(|typ| self.alloc_type_expr(typ));
        Param {
            name: ast.ident_lit().expect("Empty param. How?").text().into(),
            typ,
        }
    }

    fn lower_type_expr(&mut self, type_expr: Option<ast::TypeExpr>) -> TypeExpr {
        if type_expr.is_none() {
            return TypeExpr::Missing;
        }
        match type_expr.unwrap() {
            ast::TypeExpr::TypeIdent(ast) => ast.ident_lit().map_or(TypeExpr::Missing, |name| {
                let name = name.text().into();
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

    fn curry(&mut self, body: ExprIdx, params: &[Param]) -> Expr {
        let tail_param = params.last().cloned().unwrap_or(Param::empty());
        let param = Box::new(tail_param);

        let body = Expr::LambdaExpr { param, body };
        params.iter().rev().skip(1).fold(body, |body, param| {
            let body = self.alloc_expr(body);
            let param = Box::new(param.clone());
            Expr::LambdaExpr { param, body }
        })
    }

    fn lower_expr(&mut self, expr: Option<ast::Expr>) -> Expr {
        if expr.is_none() {
            return Expr::Missing;
        }
        match expr.unwrap() {
            ast::Expr::IdentExpr(ast) => {
                ast.ident_lit()
                    .map_or(Expr::Missing, |ident| Expr::IdentExpr {
                        name: ident.text().into(),
                    })
            }
            ast::Expr::ParenExpr(ast) => self.lower_expr(ast.expr()),
            ast::Expr::AppExpr(ast) => {
                let func = self.lower_expr(ast.func());
                let func = self.alloc_expr(func);

                let arg = self.lower_expr(ast.arg());
                let arg = self.alloc_expr(arg);

                Expr::AppExpr { func, arg }
            }
            ast::Expr::LiteralExpr(ast) => ast.literal().map_or(Expr::Missing, |lit| {
                Expr::LiteralExpr(match lit.kind() {
                    ast::LiteralKind::Int => Literal::IntLiteral(
                        lit.syntax().text().parse().expect("Invalid int literal"),
                    ),

                    ast::LiteralKind::DummyKw => unreachable!(),
                })
            }),
            ast::Expr::LambdaExpr(ast) => {
                let params = self.lower_params(ast.params());
                let body = self.lower_expr(ast.body());
                let body = self.alloc_expr(body);

                self.curry(body, &params)
            }
            ast::Expr::BlockExpr(ast) => self.lower_block(&ast),
            ast::Expr::BinaryExpr(_) => todo!("Binary expressions are not yet supported"),
        }
    }

    fn lower_ident(ident: Option<parser::SyntaxToken>) -> Option<String> {
        ident.map(|ident| ident.text().into())
    }

    fn lower_stmt(&mut self, ast: ast::Stmt, cont: ExprIdx) -> Expr {
        match ast {
            ast::Stmt::ExprStmt(ast) => {
                let expr = self.lower_expr(ast.expr());
                let expr = self.alloc_expr(expr);
                Expr::LetExpr(Box::new(LetExpr {
                    name: String::new(),
                    params: vec![].into(),
                    defn: expr,
                    body: cont,
                }))
            }
            ast::Stmt::LetStmt(ast) => {
                let params = self.lower_params(ast.params());
                let defn = self.lower_expr(ast.def());
                let defn = self.alloc_expr(defn);
                let name = Self::lower_ident(ast.ident_lit()).unwrap_or(String::new());
                Expr::LetExpr(Box::new(LetExpr {
                    name,
                    params,
                    defn,
                    body: cont,
                }))
            }
        }
    }

    fn lower_block(&mut self, ast: &ast::BlockExpr) -> Expr {
        let tail_expr = self.lower_expr(ast.tail_expr());

        let stmts: Vec<_> = ast.statements().collect();
        stmts.iter().rev().fold(tail_expr, |body, stmt| {
            let body = self.alloc_expr(body);
            self.lower_stmt(stmt.clone(), body)
        })
    }

    fn alloc_expr(&mut self, expr: Expr) -> ExprIdx {
        if let Expr::Missing = expr {
            missing_expr_id()
        } else {
            self.expressions.alloc(expr)
        }
    }

    fn alloc_type_expr(&mut self, type_expr: TypeExpr) -> TypeExprIdx {
        if let TypeExpr::Missing = type_expr {
            missing_type_expr_id()
        } else {
            self.type_expressions.alloc(type_expr)
        }
    }
}

pub(crate) fn missing_expr_id() -> ExprIdx {
    let raw = la_arena::RawIdx::from_u32(0);
    ExprIdx::from_raw(raw)
}

fn missing_type_expr_id() -> TypeExprIdx {
    let raw = la_arena::RawIdx::from_u32(0);
    TypeExprIdx::from_raw(raw)
}

#[cfg(test)]
mod tests {
    use crate::{missing_expr_id, Declaration, Expr, Param};

    fn check_expr(text: &str, expected_database: &super::Database) {
        let module_syntax = parser::parse(&format!("def x = {text};")).module();
        let mut db = super::Database::default();

        db.lower_module(&module_syntax);

        assert_eq!(db.expressions, expected_database.expressions);
        assert_eq!(db.type_expressions, expected_database.type_expressions);
    }

    #[test]
    fn lower_def_func_as_expr() {
        let module = parser::parse("def f x y = 42;").module();
        let mut actual_db = super::Database::default();

        actual_db.lower_module(&module);

        let mut expected_db = super::Database::default();
        let body = expected_db.alloc_expr(Expr::LiteralExpr(super::Literal::IntLiteral(42)));

        let param = Box::new(Param {
            name: "x".into(),
            typ: None,
        });

        let param2 = Box::new(Param {
            name: "y".into(),
            typ: None,
        });

        let body = expected_db.alloc_expr(Expr::LambdaExpr {
            param: param2,
            body,
        });

        let defn = expected_db.alloc_expr(Expr::LambdaExpr { param, body });

        let def_decl = Declaration::DefDecl(Box::new(super::DefDecl {
            name: String::from("f"),
            defn,
        }));

        expected_db.declarations.alloc(def_decl);

        assert_eq!(actual_db.expressions, expected_db.expressions);
        assert_eq!(actual_db.type_expressions, expected_db.type_expressions);
        assert_eq!(actual_db.declarations, expected_db.declarations);
    }

    #[test]
    fn lower_def_func_block() {
        let module = parser::parse("def f x y { 42 };").module();
        let mut actual_db = super::Database::default();
        actual_db.lower_module(&module);

        let mut expected_db = super::Database::default();
        let body = expected_db.alloc_expr(Expr::LiteralExpr(super::Literal::IntLiteral(42)));

        let param = Box::new(Param {
            name: "x".into(),
            typ: None,
        });

        let param2 = Box::new(Param {
            name: "y".into(),
            typ: None,
        });

        let body = expected_db.alloc_expr(Expr::LambdaExpr {
            param: param2,
            body,
        });

        let defn = expected_db.alloc_expr(Expr::LambdaExpr { param, body });
        let def_decl = Declaration::DefDecl(Box::new(super::DefDecl {
            name: String::from("f"),
            defn,
        }));
        expected_db.declarations.alloc(def_decl);
        assert_eq!(actual_db.expressions, expected_db.expressions);
        assert_eq!(actual_db.type_expressions, expected_db.type_expressions);
        assert_eq!(actual_db.declarations, expected_db.declarations);
    }

    #[test]
    fn lower_ident() {
        let mut database = super::Database::default();
        database.alloc_expr(Expr::IdentExpr { name: "x".into() });
        check_expr("x", &database);
    }

    #[test]
    fn lower_app() {
        let mut database = super::Database::default();
        let func = database.alloc_expr(Expr::IdentExpr { name: "f".into() });
        let arg = database.alloc_expr(Expr::IdentExpr { name: "y".into() });
        database.alloc_expr(Expr::AppExpr { func, arg });
        check_expr("f y", &database);
    }

    #[test]
    fn lower_lambda() {
        let mut database = super::Database::default();
        let param = Param {
            name: "x".into(),
            typ: None,
        };
        let param2 = Param {
            name: "y".into(),
            typ: None,
        };
        let body = database.alloc_expr(Expr::IdentExpr { name: "x".into() });
        let inner = database.alloc_expr(Expr::LambdaExpr {
            param: Box::new(param2),
            body,
        });
        let _outer = database.alloc_expr(Expr::LambdaExpr {
            param: Box::new(param),
            body: inner,
        });

        check_expr("\\x y -> x", &database);
    }

    #[test]
    fn lower_let() {
        let mut database = super::Database::default();
        let param = Param {
            name: "b".into(),
            typ: None,
        };
        let params = vec![param].into_boxed_slice();
        let body = database.alloc_expr(Expr::IdentExpr { name: "d".into() });
        let defn = database.alloc_expr(Expr::IdentExpr { name: "c".into() });

        database.alloc_expr(Expr::LetExpr(Box::new(crate::LetExpr {
            name: "a".into(),
            params,
            defn,
            body,
        })));

        let module_syntax = parser::parse("def x { let a b = c; d }").module();
        let mut db = super::Database::default();

        db.lower_module(&module_syntax);

        assert_eq!(db.expressions, database.expressions);
        assert_eq!(db.type_expressions, database.type_expressions);
    }

    #[test]
    fn lower_lambda_missing_type() {
        let mut database = super::Database::default();
        let param = Param {
            name: "x".into(),
            typ: None,
        };
        let body = database.alloc_expr(Expr::IdentExpr { name: "x".into() });
        database.alloc_expr(Expr::LambdaExpr {
            param: Box::new(param),
            body,
        });

        check_expr("\\x -> x", &database);
    }

    #[test]
    fn lower_lambda_missing_body() {
        let mut database = super::Database::default();
        let param = Param {
            name: "x".into(),
            typ: None,
        };

        database.alloc_expr(Expr::LambdaExpr {
            param: Box::new(param),
            body: missing_expr_id(),
        });

        check_expr("\\x ->", &database);
    }

    #[test]
    fn lower_lambda_missing_params() {
        let mut database = super::Database::default();
        let body = database.alloc_expr(Expr::IdentExpr { name: "x".into() });
        let param = Box::new(Param::empty());
        database.alloc_expr(Expr::LambdaExpr { param, body });

        check_expr("\\-> x", &database);
    }

    #[test]
    fn lower_let_missing_defn() {
        let mut database = super::Database::default();
        let param = Param {
            name: "b".into(),
            typ: None,
        };
        let params = vec![param].into_boxed_slice();
        let body = database.alloc_expr(Expr::IdentExpr { name: "d".into() });
        database.alloc_expr(Expr::LetExpr(Box::new(crate::LetExpr {
            name: "a".into(),
            params,
            defn: missing_expr_id(),
            body,
        })));

        check_expr("{ let a b; d }", &database);
    }
}
