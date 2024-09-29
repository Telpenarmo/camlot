use ena::unify::InPlaceUnificationTable;

use crate::hir::ExprIdx;
use crate::intern::Interner;
use crate::types::{TypeIdx, UnificationVar};
use crate::{infer::Constraint, types::Type};

impl Type {
    fn occurs(&self, types: &Interner<Type>, v: UnificationVar) -> bool {
        match self {
            Type::Arrow(from, to) => {
                let from = types.lookup(*from);
                let to = types.lookup(*to);
                from.occurs(types, v) || to.occurs(types, v)
            }
            Type::Unifier(u) => *u == v,
            _ => false,
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum UnifcationError {
    Occurs(TypeIdx, UnificationVar),
    NotUnifiable(TypeIdx, TypeIdx),
}

pub(super) struct Unifcation<'a> {
    unification_table: &'a mut InPlaceUnificationTable<UnificationVar>,
    types: &'a Interner<Type>,
}

impl<'a> Unifcation<'a> {
    pub(super) fn new(
        unification_table: &'a mut InPlaceUnificationTable<UnificationVar>,
        types: &'a Interner<Type>,
    ) -> Self {
        Self {
            unification_table,
            types,
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

    fn unify_eq(
        &mut self,
        expected_idx: TypeIdx,
        actual_idx: TypeIdx,
    ) -> Result<(), UnifcationError> {
        let expected = self.types.lookup(expected_idx);
        let actual = self.types.lookup(actual_idx);

        match ((expected, expected_idx), (actual, actual_idx)) {
            ((Type::Bool, _), (Type::Bool, _))
            | ((Type::Int, _), (Type::Int, _))
            | ((Type::Unit, _), (Type::Unit, _)) => Ok(()),

            ((&Type::Arrow(from, to), _), (&Type::Arrow(from2, to2), _)) => {
                let from = self.unify_eq(from, from2);
                let to = self.unify_eq(to, to2);
                from.or(to)
            }

            ((Type::Unifier(a), _), (Type::Unifier(b), _)) => self
                .unification_table
                .unify_var_var(*a, *b)
                .or_else(|(a, b)| self.unify_eq(a, b)),

            ((Type::Unifier(u), _), (ty, ty_idx)) | ((ty, ty_idx), (Type::Unifier(u), _)) => {
                if ty.occurs(self.types, *u) {
                    return Err(UnifcationError::Occurs(ty_idx, *u));
                }
                self.unification_table
                    .unify_var_value(*u, Some(ty_idx))
                    .or_else(|(a, b)| self.unify_eq(a, b))
            }

            _ => Err(UnifcationError::NotUnifiable(expected_idx, actual_idx)),
        }
    }
}
