use std::collections::HashMap;

use crate::intern::Interner;
use crate::types::{TypeIdx, UnificationVar};
use crate::{infer::Constraint, types::Type};

use super::normalize::normalize;
use super::{ConstraintReason, UnificationTable};

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
pub(super) enum UnifcationError {
    Occurs(TypeIdx, UnificationVar),
    NotUnifiable { expected: TypeIdx, actual: TypeIdx },
}

pub(super) struct Unifcation<'a> {
    unification_table: &'a mut UnificationTable,
    types: &'a mut Interner<Type>,
}

impl<'a> Unifcation<'a> {
    pub(super) fn new(
        unification_table: &'a mut UnificationTable,
        types: &'a mut Interner<Type>,
    ) -> Self {
        Self {
            unification_table,
            types,
        }
    }

    pub(super) fn unify(
        &mut self,
        constraints: Vec<Constraint>,
    ) -> Vec<(ConstraintReason, UnifcationError)> {
        constraints
            .into_iter()
            .flat_map(|constraint| match constraint {
                Constraint::TypeEqual(idx, expected, actual) => self
                    .unify_eq(expected, actual)
                    .into_iter()
                    .map(move |e| (idx, e)),
            })
            .collect()
    }

    fn unify_eq(&mut self, expected_idx: TypeIdx, actual_idx: TypeIdx) -> Vec<UnifcationError> {
        let mut cache = HashMap::new();
        let expected_idx = normalize(self.types, &mut cache, self.unification_table, expected_idx);
        let actual_idx = normalize(self.types, &mut cache, self.unification_table, actual_idx);

        let expected = self.types.lookup(expected_idx);
        let actual = self.types.lookup(actual_idx);
        match ((expected, expected_idx), (actual, actual_idx)) {
            ((Type::Bool, _), (Type::Bool, _))
            | ((Type::Int, _), (Type::Int, _))
            | ((Type::Unit, _), (Type::Unit, _)) => vec![],

            ((&Type::Arrow(from, to), _), (&Type::Arrow(from2, to2), _)) => {
                let mut from = self.unify_eq(from, from2);
                let mut to = self.unify_eq(to, to2);
                from.append(&mut to);
                from
            }

            ((Type::Unifier(a), _), (Type::Unifier(b), _)) => self
                .unification_table
                .unify_var_var(*a, *b)
                .err()
                .map(|(a, b)| self.unify_eq(a, b))
                .unwrap_or_default(),

            ((Type::Unifier(u), _), (ty, ty_idx)) | ((ty, ty_idx), (Type::Unifier(u), _)) => {
                if ty.occurs(self.types, *u) {
                    return vec![UnifcationError::Occurs(ty_idx, *u)];
                }
                self.unification_table
                    .unify_var_value(*u, Some(ty_idx))
                    .err()
                    .map(|(a, b)| self.unify_eq(a, b))
                    .unwrap_or_default()
            }

            _ => vec![UnifcationError::NotUnifiable {
                expected: expected_idx,
                actual: actual_idx,
            }],
        }
    }
}
