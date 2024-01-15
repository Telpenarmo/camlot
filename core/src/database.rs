use crate::hir::*;
use la_arena::Arena;
use parser::nodes as ast;

pub struct Database {
    expressions: Arena<Expr>,
    type_expressions: Arena<TypeExpr>,
}

#[allow(unreachable_code, unused)]
impl Database {
    pub(crate) fn lower_decl(&mut self, ast: ast::Decl) -> Option<Declaration> {
        let r = match ast {
            ast::Decl::LetDecl(ast) => Declaration::LetDecl {
                name: ast.ident_lit()?.text().into(),
                params: ast.params().map(|params| {
                    params
                        .params()
                        .map(|param| self.lower_param(param))
                        .flatten()
                        .collect()
                })?,
                defn: ast.expr().map(|expr| self.lower_expr(expr)).flatten()?,
            },
            ast::Decl::OpenDecl(ast) => Declaration::OpenDecl {
                path: ast.ident_lit()?.text().into(),
            },
            ast::Decl::TypeDecl(ast) => Declaration::TypeDecl {
                name: ast.ident_lit()?.text().into(),
                defn: ast
                    .type_expr()
                    .map(|typ| self.lower_type_expr(typ))
                    .flatten()?,
            },
        };
        None
    }

    fn lower_param(&mut self, ast: ast::Param) -> Option<Param> {
        Some(Param {
            name: ast.ident_lit()?.text().into(),
            typ: ast.type_expr().and_then(|typ| self.lower_type_expr(typ)),
        })
    }

    pub fn new() -> Self {
        Self {
            expressions: Arena::new(),
            type_expressions: Arena::new(),
        }
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
}
