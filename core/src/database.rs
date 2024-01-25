use crate::{hir::*, LetDecl};
use la_arena::Arena;
use parser::nodes as ast;

pub struct Database {
    expressions: Arena<Expr>,
    type_expressions: Arena<TypeExpr>,
}

#[allow(unreachable_code, unused)]
impl Database {
    pub fn new() -> Self {
        let mut expressions = Arena::new();
        let mut type_expressions = Arena::new();

        expressions.alloc(Expr::Missing);
        type_expressions.alloc(TypeExpr::Missing);

        Self {
            expressions,
            type_expressions,
        }
    }

    pub(crate) fn lower_decl(&mut self, ast: ast::Decl) -> Option<Declaration> {
        let r = match ast {
            ast::Decl::LetDecl(ast) => {
                let params = ast.params().map(|ast| {
                    ast.params()
                        .map(|param| self.lower_param(param))
                        .flatten()
                        .collect()
                })?;
                let defn = ast.expr().map(|expr| self.lower_expr(expr)).flatten()?;
                let defn = Box::new(defn);
                Declaration::LetDecl(Box::new(LetDecl {
                    name: ast.ident_lit()?.text().into(),
                    params,
                    defn,
                }))
            }
            ast::Decl::OpenDecl(ast) => Declaration::OpenDecl {
                path: ast.ident_lit()?.text().into(),
            },
            ast::Decl::TypeDecl(ast) => {
                let defn = ast
                    .type_expr()
                    .map(|typ| self.lower_type_expr(typ))
                    .flatten()?;
                let defn = Box::new(defn);
                Declaration::TypeDecl {
                    name: ast.ident_lit()?.text().into(),
                    defn,
                }
            }
        };
        None
    }

    fn lower_param(&mut self, ast: ast::Param) -> Option<Param> {
        let typ = ast.type_expr().and_then(|typ| self.lower_type_expr(typ));
        let typ = match typ {
            Some(typ) => Some(self.alloc_type_expr(typ)),
            None => None,
        };
        Some(Param {
            name: ast.ident_lit()?.text().into(),
            typ,
        })
    }

    fn lower_type_expr(&self, type_expr: ast::TypeExpr) -> Option<TypeExpr> {
        Some(match type_expr {
            ast::TypeExpr::TypeIdent(ast) => TypeExpr::IdentTypeExpr {
                name: ast.ident_lit()?.text().into(),
            },
            ast::TypeExpr::TypeArrow(ast) => TypeExpr::TypeArrow {
                from: todo!(),
                to: todo!(),
            },
            ast::TypeExpr::TypeParen(ast) => todo!(),
        })
    }

    fn lower_expr(&self, expr: ast::Expr) -> Option<Expr> {
        todo!()
    }

    fn alloc_expr(&mut self, expr: Expr) -> ExprIdx {
        if let Expr::Missing = expr {
            self.missing_expr_id()
        } else {
            self.expressions.alloc(expr)
        }
    }

    fn alloc_type_expr(&mut self, type_expr: TypeExpr) -> TypeExprIdx {
        if let TypeExpr::Missing = type_expr {
            self.missing_type_expr_id()
        } else {
            self.type_expressions.alloc(type_expr)
        }
    }

    fn missing_expr_id(&self) -> ExprIdx {
        let raw = la_arena::RawIdx::from_u32(0);
        ExprIdx::from_raw(raw)
    }

    fn missing_type_expr_id(&self) -> TypeExprIdx {
        let raw = la_arena::RawIdx::from_u32(0);
        TypeExprIdx::from_raw(raw)
    }
}
