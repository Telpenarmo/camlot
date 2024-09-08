use crate::hir::{
        Definition, DefinitionIdx, Expr, ExprIdx, Module, Open, OpenIdx, Param, TypeDefinition,
        TypeDefinitionIdx, TypeExpr, TypeExprIdx,
    };
use la_arena::Arena;
use parser::{nodes as ast, AstToken};

#[derive(Debug)]
#[allow(unused)]
pub struct Database {
    definitions: Arena<Definition>,
    opens: Arena<Open>,
    type_definitions: Arena<TypeDefinition>,
    expressions: Arena<Expr>,
    type_expressions: Arena<TypeExpr>,
}

fn param_deep_eq(a_db: &Database, b_db: &Database, a: &Param, b: &Param) -> bool {
    a.name == b.name && type_expr_deep_eq(a_db, b_db, a.typ, b.typ)
}
fn type_expr_deep_eq(a_db: &Database, b_db: &Database, a: TypeExprIdx, b: TypeExprIdx) -> bool {
    let a = a_db.get_type_expr(a);
    let b = b_db.get_type_expr(b);
    match (a, b) {
        (TypeExpr::Missing, TypeExpr::Missing) => true,
        (TypeExpr::IdentTypeExpr { name: a }, TypeExpr::IdentTypeExpr { name: b }) => a == b,
        (
            TypeExpr::TypeArrow { from, to },
            TypeExpr::TypeArrow {
                from: b_from,
                to: b_to,
            },
        ) => {
            type_expr_deep_eq(a_db, b_db, *from, *b_from)
                && type_expr_deep_eq(a_db, b_db, *to, *b_to)
        }
        _ => false,
    }
}
fn expr_deep_eq(a_db: &Database, b_db: &Database, a: ExprIdx, b: ExprIdx) -> bool {
    let a = a_db.get_expr(a);
    let b = b_db.get_expr(b);
    match (a, b) {
        (Expr::Missing, Expr::Missing) => true,
        (Expr::LiteralExpr(a), Expr::LiteralExpr(b)) => a == b,
        (Expr::IdentExpr { name: a }, Expr::IdentExpr { name: b }) => a == b,
        (
            Expr::AppExpr { func, arg },
            Expr::AppExpr {
                func: b_func,
                arg: b_arg,
            },
        ) => expr_deep_eq(a_db, b_db, *func, *b_func) && expr_deep_eq(a_db, b_db, *arg, *b_arg),
        (Expr::LambdaExpr(l_lambda), Expr::LambdaExpr(b_lambda)) => {
            param_deep_eq(a_db, b_db, &l_lambda.param, &b_lambda.param)
                && expr_deep_eq(a_db, b_db, l_lambda.body, b_lambda.body)
                && type_expr_deep_eq(a_db, b_db, l_lambda.return_type, b_lambda.return_type)
        }
        (Expr::LetExpr(l_let), Expr::LetExpr(b_let)) => {
            l_let.name == b_let.name
                && type_expr_deep_eq(a_db, b_db, l_let.return_type, b_let.return_type)
                && expr_deep_eq(a_db, b_db, l_let.body, b_let.body)
                && expr_deep_eq(a_db, b_db, l_let.defn, b_let.defn)
                && l_let
                    .params
                    .iter()
                    .zip(b_let.params.iter())
                    .all(|(a, b)| param_deep_eq(a_db, b_db, a, b))
        }
        _ => false,
    }
}

impl PartialEq for Database {
    fn eq(&self, other: &Self) -> bool {
        self.definitions
            .values()
            .zip(other.definitions.values())
            .all(|(a, b)| expr_deep_eq(self, other, a.defn, b.defn) && a.name == b.name)
            && self
                .opens
                .values()
                .zip(other.opens.values())
                .all(|(a, b)| a.path == b.path)
            && self
                .type_definitions
                .values()
                .zip(other.type_definitions.values())
                .all(|(a, b)| a.name == b.name && type_expr_deep_eq(self, other, a.defn, b.defn))
    }
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
        let mut expressions = Arena::new();
        let mut type_expressions = Arena::new();

        expressions.alloc(Expr::Missing);
        type_expressions.alloc(TypeExpr::Missing);

        Self {
            expressions,
            type_expressions,
            definitions: Arena::new(),
            opens: Arena::new(),
            type_definitions: Arena::new(),
        }
    }

    pub fn lower_module(&mut self, ast: &ast::Module) -> Module {
        let mut definitions: Vec<DefinitionIdx> = Vec::new();
        let mut opens: Vec<OpenIdx> = Vec::new();
        let mut types: Vec<TypeDefinitionIdx> = Vec::new();

        ast.module_items().for_each(|ast| match ast {
            ast::ModuleItem::Definition(ast) => {
                let definition = self.lower_definition(&ast);
                definitions.push(self.definitions.alloc(definition));
            }
            ast::ModuleItem::Open(ast) => {
                let open = Self::lower_open(&ast);
                opens.push(self.opens.alloc(open));
            }
            ast::ModuleItem::TypeDefinition(ast) => {
                let type_definition = self.lower_type_definition(&ast);
                types.push(self.type_definitions.alloc(type_definition));
            }
        });

        let definitions = definitions.into_boxed_slice();
        let opens = opens.into_boxed_slice();
        let type_definitions = types.into_boxed_slice();

        Module {
            definitions,
            opens,
            type_definitions,
        }
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
            name: Self::lower_ident(ast.ident_lit()),
            defn,
        }
    }

    fn lower_open(ast: &ast::Open) -> Open {
        Open {
            path: Self::lower_ident(ast.ident_lit()),
        }
    }

    fn lower_type_definition(&mut self, ast: &ast::TypeDefinition) -> TypeDefinition {
        let defn = self.lower_type_expr(ast.type_expr());
        let defn = self.alloc_type_expr(defn);
        TypeDefinition {
            name: Self::lower_ident(ast.ident_lit()),
            defn,
        }
    }

    fn lower_params(&mut self, ast: Option<ast::Params>) -> Box<[Param]> {
        ast.map(|ast| ast.params().map(|ast| self.lower_param(&ast)).collect())
            .unwrap_or_default()
    }

    fn lower_param(&mut self, ast: &ast::Param) -> Param {
        let typ = self.lower_type_annotation(ast.type_annotation());
        Param {
            name: ast.ident_lit().expect("Empty param. How?").text().into(),
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

    fn curry(&mut self, body: ExprIdx, params: &[Param], return_type: TypeExprIdx) -> Expr {
        let empty_param = Param {
            name: String::new(),
            typ: MISSING_TYPE_EXPR_ID,
        };
        let tail_param = params.last().cloned().unwrap_or(empty_param);

        let body = Expr::lambda_expr(tail_param, return_type, body);

        params.iter().rev().skip(1).fold(body, |body, param| {
            let body = self.alloc_expr(body);

            Expr::lambda_expr(param.clone(), MISSING_TYPE_EXPR_ID, body)
        })
    }

    fn lower_expr(&mut self, expr: Option<ast::Expr>) -> Expr {
        if expr.is_none() {
            return Expr::Missing;
        }
        match expr.unwrap() {
            ast::Expr::IdentExpr(ast) => ast
                .ident_lit()
                .map_or(Expr::Missing, |ident| Expr::ident_expr(ident.text())),
            ast::Expr::ParenExpr(ast) => {
                if let Some(app) = ast.app_expr() {
                    self.lower_app(&app)
                } else {
                    self.lower_expr(ast.expr())
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

    fn lower_ident(ident: Option<parser::SyntaxToken>) -> String {
        ident.map(|ident| ident.text().into()).unwrap_or_default()
    }

    fn lower_stmt(&mut self, ast: ast::Stmt, cont: ExprIdx) -> Expr {
        match ast {
            ast::Stmt::ExprStmt(ast) => {
                let expr = self.lower_expr(ast.expr());
                let expr = self.alloc_expr(expr);
                Expr::let_expr(
                    String::new(),
                    vec![].into(),
                    self.alloc_type_expr(TypeExpr::Missing),
                    expr,
                    cont,
                )
            }
            ast::Stmt::LetStmt(ast) => {
                let name = Self::lower_ident(ast.ident_lit());

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
        let tail_expr = self.lower_expr(ast.tail_expr());

        let stmts: Vec<_> = ast.statements().collect();
        stmts.iter().rev().fold(tail_expr, |body, stmt| {
            let body = self.alloc_expr(body);
            self.lower_stmt(stmt.clone(), body)
        })
    }

    fn alloc_expr(&mut self, expr: Expr) -> ExprIdx {
        if let Expr::Missing = expr {
            MISSING_EXPR_ID
        } else {
            self.expressions.alloc(expr)
        }
    }

    fn alloc_type_expr(&mut self, type_expr: TypeExpr) -> TypeExprIdx {
        if let TypeExpr::Missing = type_expr {
            MISSING_TYPE_EXPR_ID
        } else {
            self.type_expressions.alloc(type_expr)
        }
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

const MISSING_EXPR_ID: ExprIdx = {
    let raw = la_arena::RawIdx::from_u32(0);
    ExprIdx::from_raw(raw)
};

const MISSING_TYPE_EXPR_ID: TypeExprIdx = {
    let raw = la_arena::RawIdx::from_u32(0);
    TypeExprIdx::from_raw(raw)
};

#[cfg(test)]
mod tests {
    use super::{MISSING_EXPR_ID as MISSING_EXPR, MISSING_TYPE_EXPR_ID as MISSING_TYPE};
    use crate::hir::{Definition, Expr, Param, TypeExpr};

    fn unannotated_param(name: &str) -> Param {
        Param {
            name: name.into(),
            typ: MISSING_TYPE,
        }
    }

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
        let body = expected_db.alloc_expr(Expr::int_expr(42));

        let body = expected_db.alloc_expr(Expr::lambda_expr(
            unannotated_param("y"),
            MISSING_TYPE,
            body,
        ));
        let defn = expected_db.alloc_expr(Expr::lambda_expr(
            unannotated_param("x"),
            MISSING_TYPE,
            body,
        ));
        let definition = Definition {
            name: String::from("f"),
            defn,
        };

        expected_db.definitions.alloc(definition);

        assert_eq!(actual_db, expected_db);
    }

    #[test]
    fn lower_def_func_block() {
        let module = parser::parse("def f x y { 42 };").module();
        let mut actual_db = super::Database::default();
        actual_db.lower_module(&module);

        let mut expected_db = super::Database::default();
        let body = expected_db.alloc_expr(Expr::int_expr(42));

        let body = expected_db.alloc_expr(Expr::lambda_expr(
            unannotated_param("y"),
            MISSING_TYPE,
            body,
        ));

        let defn = expected_db.alloc_expr(Expr::lambda_expr(
            unannotated_param("x"),
            MISSING_TYPE,
            body,
        ));
        let definition = Definition {
            name: String::from("f"),
            defn,
        };
        expected_db.definitions.alloc(definition);

        assert_eq!(actual_db, expected_db);
    }

    #[test]
    fn lower_ident() {
        let mut database = super::Database::default();
        database.alloc_expr(Expr::ident_expr("x"));
        check_expr("x", &database);
    }

    #[test]
    fn lower_app() {
        let mut database = super::Database::default();

        let func = database.alloc_expr(Expr::ident_expr("f"));
        let arg = database.alloc_expr(Expr::ident_expr("y"));
        database.alloc_expr(Expr::AppExpr { func, arg });

        check_expr("(f y)", &database);
    }

    #[test]
    fn lower_nested_app() {
        let mut database = super::Database::default();

        let func = database.alloc_expr(Expr::ident_expr("f"));
        let arg = database.alloc_expr(Expr::ident_expr("y"));

        let func = database.alloc_expr(Expr::AppExpr { func, arg });
        let arg = database.alloc_expr(Expr::ident_expr("z"));
        database.alloc_expr(Expr::AppExpr { func, arg });

        check_expr("(f y z)", &database);
    }

    #[test]
    fn lower_lambda() {
        let mut database = super::Database::default();

        let body = database.alloc_expr(Expr::ident_expr("x"));
        let inner = database.alloc_expr(Expr::lambda_expr(
            unannotated_param("y"),
            MISSING_TYPE,
            body,
        ));
        let _outer = database.alloc_expr(Expr::lambda_expr(
            unannotated_param("x"),
            MISSING_TYPE,
            inner,
        ));

        check_expr("\\x y -> x", &database);
    }

    #[test]
    fn lower_let() {
        let mut database = super::Database::default();

        let body = database.alloc_expr(Expr::ident_expr("d"));
        let defn = database.alloc_expr(Expr::ident_expr("c"));

        database.alloc_expr(Expr::let_expr(
            "a".into(),
            vec![unannotated_param("b")].into_boxed_slice(),
            MISSING_TYPE,
            defn,
            body,
        ));

        let module_syntax = parser::parse("def x { let a b = c; d }").module();
        let mut db = super::Database::default();

        db.lower_module(&module_syntax);

        assert_eq!(db.expressions, database.expressions);
        assert_eq!(db.type_expressions, database.type_expressions);
    }

    #[test]
    fn lower_lambda_missing_type() {
        let mut database = super::Database::default();

        let body = database.alloc_expr(Expr::ident_expr("x"));
        database.alloc_expr(Expr::lambda_expr(
            unannotated_param("x"),
            MISSING_TYPE,
            body,
        ));

        check_expr("\\x -> x", &database);
    }

    #[test]
    fn lower_lambda_missing_body() {
        let mut database = super::Database::default();

        database.alloc_expr(Expr::lambda_expr(
            unannotated_param("x"),
            MISSING_TYPE,
            MISSING_EXPR,
        ));

        check_expr("\\x ->", &database);
    }

    #[test]
    fn lower_lambda_missing_params() {
        let mut database = super::Database::default();

        let body = database.alloc_expr(Expr::ident_expr("x"));
        database.alloc_expr(Expr::lambda_expr(unannotated_param(""), MISSING_TYPE, body));

        check_expr("\\-> x", &database);
    }

    #[test]
    fn lower_let_missing_defn() {
        let mut database = super::Database::default();

        let params = vec![unannotated_param("b")].into_boxed_slice();
        let body = database.alloc_expr(Expr::ident_expr("d"));

        database.alloc_expr(Expr::let_expr(
            "a".into(),
            params,
            MISSING_TYPE,
            MISSING_EXPR,
            body,
        ));

        check_expr("{ let a b; d }", &database);
    }

    #[test]
    fn lower_def_func_with_return_type() {
        let module = parser::parse("def f x y : Int = 42;").module();
        let mut actual_db = super::Database::default();

        actual_db.lower_module(&module);

        let mut expected_db = super::Database::default();
        let body = expected_db.alloc_expr(Expr::int_expr(42));
        let return_type =
            expected_db.alloc_type_expr(TypeExpr::IdentTypeExpr { name: "Int".into() });
        let body =
            expected_db.alloc_expr(Expr::lambda_expr(unannotated_param("y"), return_type, body));
        let defn = expected_db.alloc_expr(Expr::lambda_expr(
            unannotated_param("x"),
            MISSING_TYPE,
            body,
        ));
        let definition = Definition {
            name: String::from("f"),
            defn,
        };
        expected_db.definitions.alloc(definition);

        assert_eq!(actual_db, expected_db);
    }

    #[test]
    fn lower_let_with_return_type() {
        let module = parser::parse("def f { let x : Int = 42; x }").module();
        let mut actual_db = super::Database::default();

        actual_db.lower_module(&module);

        let mut expected_db = super::Database::default();

        let return_type =
            expected_db.alloc_type_expr(TypeExpr::IdentTypeExpr { name: "Int".into() });

        let tail_expr = expected_db.alloc_expr(Expr::ident_expr("x"));
        let let_body = expected_db.alloc_expr(Expr::int_expr(42));

        let defn = expected_db.alloc_expr(Expr::let_expr(
            "x".into(),
            vec![].into_boxed_slice(),
            return_type,
            let_body,
            tail_expr,
        ));

        let definition = Definition {
            name: String::from("f"),
            defn,
        };
        expected_db.definitions.alloc(definition);

        assert_eq!(actual_db, expected_db);
    }
}
