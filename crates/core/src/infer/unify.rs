use std::collections::HashMap;

use ena::unify::InPlaceUnificationTable;

use crate::hir::ExprIdx;
use crate::types::UnificationVar;
use crate::{infer::Constraint, types::Type};

impl Type {
    fn occurs(&self, v: UnificationVar) -> bool {
        match self {
            Type::Arrow(from, to) => from.occurs(v) || to.occurs(v),
            Type::Unifier(u) => *u == v,
            _ => false,
        }
    }
}

pub enum UnifcationError {
    Occurs(Type, UnificationVar),
    NotUnifiable(Type, Type),
}

pub(super) struct Unifcation<'a> {
    unification_table: &'a mut InPlaceUnificationTable<UnificationVar>,
    expr_types: &'a HashMap<ExprIdx, Type>,
}

impl<'a> Unifcation<'a> {
    pub(super) fn new(
        unification_table: &'a mut InPlaceUnificationTable<UnificationVar>,
        expr_types: &'a HashMap<ExprIdx, Type>,
    ) -> Self {
        Self {
            unification_table,
            expr_types,
        }
    }

    pub(super) fn unify(
        &mut self,
        constraints: Vec<Constraint>,
    ) -> Vec<(ExprIdx, UnifcationError)> {
        constraints
            .into_iter()
            .filter_map(|constraint| match constraint {
                Constraint::TypeEqual(idx, expected, actual) => {
                    self.unify_eq(expected, actual).err().map(|e| (idx, e))
                }
            })
            .collect()
    }

    fn unify_eq(&mut self, expected: Type, actual: Type) -> Result<(), UnifcationError> {
        match (expected, actual) {
            (Type::Bool, Type::Bool) | (Type::Int, Type::Int) | (Type::Unit, Type::Unit) => Ok(()),

            (ref l @ Type::Var(ref a), ref r @ Type::Var(ref b)) => {
                if a == b {
                    Ok(())
                } else {
                    Err(UnifcationError::NotUnifiable(l.clone(), r.clone()))
                }
            }

            (Type::Arrow(from, to), Type::Arrow(from2, to2)) => {
                let from = self.unify_eq(*from, *from2);
                let to = self.unify_eq(*to, *to2);
                from.or(to)
            }

            (Type::Unifier(a), Type::Unifier(b)) => self
                .unification_table
                .unify_var_var(a, b)
                .map_err(|(a, b)| UnifcationError::NotUnifiable(a, b)),

            (Type::Unifier(u), ty) | (ty, Type::Unifier(u)) => {
                if ty.occurs(u) {
                    return Err(UnifcationError::Occurs(ty, u));
                }
                self.unification_table
                    .unify_var_value(u, Some(ty))
                    .map_err(|(a, b)| UnifcationError::NotUnifiable(a, b))
            }

            (l, r) => Err(UnifcationError::NotUnifiable(l, r)),
        }
    }
}
