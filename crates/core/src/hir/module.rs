use crate::hir::{Definition, Expr, ExprIdx, Open, Param, TypeDefinition, TypeExpr, TypeExprIdx};
use crate::{intern::Interner, Name};
use la_arena::Arena;

use super::{DefinitionIdx, OpenIdx, TypeDefinitionIdx};

#[derive(Debug)]
#[allow(unused)]
pub struct Module {
    pub(super) definitions: Arena<Definition>,
    pub(super) opens: Arena<Open>,
    pub(super) type_definitions: Arena<TypeDefinition>,
    pub(super) expressions: Arena<Expr>,
    pub(super) type_expressions: Arena<TypeExpr>,
    pub(super) names: Interner<String>,
}

fn name_deep_eq(a_module: &Module, b_module: &Module, a: Name, b: Name) -> bool {
    a_module.names.lookup(a) == b_module.names.lookup(b)
}

fn param_deep_eq(a_module: &Module, b_module: &Module, a: &Param, b: &Param) -> bool {
    name_deep_eq(a_module, b_module, a.name, b.name)
        && type_expr_deep_eq(a_module, b_module, a.typ, b.typ)
}

pub(super) fn type_expr_deep_eq(
    a_module: &Module,
    b_module: &Module,
    a: TypeExprIdx,
    b: TypeExprIdx,
) -> bool {
    let a = a_module.get_type_expr(a);
    let b = b_module.get_type_expr(b);
    match (a, b) {
        (TypeExpr::Missing, TypeExpr::Missing) => true,
        (TypeExpr::IdentTypeExpr { name: a }, TypeExpr::IdentTypeExpr { name: b }) => {
            name_deep_eq(a_module, b_module, *a, *b)
        }
        (
            TypeExpr::TypeArrow { from, to },
            TypeExpr::TypeArrow {
                from: b_from,
                to: b_to,
            },
        ) => {
            type_expr_deep_eq(a_module, b_module, *from, *b_from)
                && type_expr_deep_eq(a_module, b_module, *to, *b_to)
        }
        _ => false,
    }
}

pub(super) fn expr_deep_eq(a_module: &Module, b_module: &Module, a: ExprIdx, b: ExprIdx) -> bool {
    let a = a_module.get_expr(a);
    let b = b_module.get_expr(b);
    match (a, b) {
        (Expr::Missing, Expr::Missing) => true,
        (Expr::LiteralExpr(a), Expr::LiteralExpr(b)) => a == b,
        (Expr::IdentExpr { name: a }, Expr::IdentExpr { name: b }) => {
            name_deep_eq(a_module, b_module, *a, *b)
        }
        (
            Expr::AppExpr { func, arg },
            Expr::AppExpr {
                func: b_func,
                arg: b_arg,
            },
        ) => {
            expr_deep_eq(a_module, b_module, *func, *b_func)
                && expr_deep_eq(a_module, b_module, *arg, *b_arg)
        }
        (Expr::LambdaExpr(l_lambda), Expr::LambdaExpr(b_lambda)) => {
            param_deep_eq(a_module, b_module, &l_lambda.param, &b_lambda.param)
                && expr_deep_eq(a_module, b_module, l_lambda.body, b_lambda.body)
                && type_expr_deep_eq(
                    a_module,
                    b_module,
                    l_lambda.return_type,
                    b_lambda.return_type,
                )
        }
        (Expr::LetExpr(l_let), Expr::LetExpr(b_let)) => {
            name_deep_eq(a_module, b_module, l_let.name, b_let.name)
                && type_expr_deep_eq(a_module, b_module, l_let.return_type, b_let.return_type)
                && expr_deep_eq(a_module, b_module, l_let.body, b_let.body)
                && expr_deep_eq(a_module, b_module, l_let.defn, b_let.defn)
                && l_let
                    .params
                    .iter()
                    .zip(b_let.params.iter())
                    .all(|(a, b)| param_deep_eq(a_module, b_module, a, b))
        }
        _ => false,
    }
}

impl PartialEq for Module {
    fn eq(&self, other: &Self) -> bool {
        self.definitions
            .values()
            .zip(other.definitions.values())
            .all(|(a, b)| {
                expr_deep_eq(self, other, a.defn, b.defn)
                    && name_deep_eq(self, other, a.name, b.name)
            })
            && self
                .opens
                .values()
                .zip(other.opens.values())
                .all(|(a, b)| name_deep_eq(self, other, a.path, b.path))
            && self
                .type_definitions
                .values()
                .zip(other.type_definitions.values())
                .all(|(a, b)| {
                    name_deep_eq(self, other, a.name, b.name)
                        && type_expr_deep_eq(self, other, a.defn, b.defn)
                })
    }
}

#[allow(unreachable_code, unused)]
impl Default for Module {
    fn default() -> Self {
        Self::new()
    }
}

#[allow(unused)]
impl Module {
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
            names: Interner::new(),
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

    pub fn definitions(&self) -> impl Iterator<Item = &Definition> + '_ {
        self.definitions.values()
    }
}
