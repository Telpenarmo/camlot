use std::collections::HashMap;

use crate::intern::Interner;
use crate::types::Type;
use crate::types::{TypeIdx, UnificationVar};

use super::normalize::normalize;
use super::TypeInference;

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

impl TypeInference {
    pub(super) fn unify_eq(
        &mut self,
        types: &mut Interner<Type>,
        expected_idx: TypeIdx,
        actual_idx: TypeIdx,
    ) -> Vec<UnifcationError> {
        let mut cache = HashMap::new();
        let expected_idx = normalize(types, &mut cache, &mut self.unification_table, expected_idx);
        let actual_idx = normalize(types, &mut cache, &mut self.unification_table, actual_idx);

        let expected = types.lookup(expected_idx);
        let actual = types.lookup(actual_idx);
        match ((expected, expected_idx), (actual, actual_idx)) {
            ((Type::Bool, _), (Type::Bool, _))
            | ((Type::Int, _), (Type::Int, _))
            | ((Type::Unit, _), (Type::Unit, _)) => vec![],

            ((&Type::Arrow(from, to), _), (&Type::Arrow(from2, to2), _)) => {
                let mut from = self.unify_eq(types, from, from2);
                let mut to = self.unify_eq(types, to, to2);
                from.append(&mut to);
                from
            }

            ((Type::Unifier(a), _), (Type::Unifier(b), _)) => self
                .unification_table
                .unify_var_var(*a, *b)
                .err()
                .map(|(a, b)| self.unify_eq(types, a, b))
                .unwrap_or_default(),

            ((Type::Unifier(u), _), (ty, ty_idx)) | ((ty, ty_idx), (Type::Unifier(u), _)) => {
                if ty.occurs(types, *u) {
                    return vec![UnifcationError::Occurs(ty_idx, *u)];
                }
                self.unification_table
                    .unify_var_value(*u, Some(ty_idx))
                    .err()
                    .map(|(a, b)| self.unify_eq(types, a, b))
                    .unwrap_or_default()
            }

            _ => vec![UnifcationError::NotUnifiable {
                expected: expected_idx,
                actual: actual_idx,
            }],
        }
    }
}
