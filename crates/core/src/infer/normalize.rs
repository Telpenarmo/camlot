use std::collections::HashMap;

use crate::infer::arrow;

use crate::intern::Interner;
use crate::types::{Type, TypeIdx, UnificationTable};

use super::TypeInference;

pub(super) fn normalize(
    types: &mut Interner<Type>,
    cache: &mut HashMap<TypeIdx, TypeIdx>,
    unification_table: &mut UnificationTable,
    idx: TypeIdx,
) -> TypeIdx {
    if let Some(x) = cache.get(&idx) {
        *x
    } else {
        let res = substitute_type(types, cache, unification_table, idx);
        cache.insert(idx, res);
        res
    }
}

fn substitute_type(
    types: &mut Interner<Type>,
    old_to_new: &mut HashMap<TypeIdx, TypeIdx>,
    unification_table: &mut UnificationTable,
    idx: TypeIdx,
) -> TypeIdx {
    match types.lookup(idx) {
        Type::Unifier(u) => unification_table.probe_value(*u).map_or(idx, |idx| {
            normalize(types, old_to_new, unification_table, idx)
        }),
        &Type::Arrow(from, to) => {
            let from = normalize(types, old_to_new, unification_table, from);
            let to = normalize(types, old_to_new, unification_table, to);
            arrow(types, from, to)
        }
        _ => idx,
    }
}

impl TypeInference<'_> {
    pub(super) fn normalize(&mut self, idx: TypeIdx) -> TypeIdx {
        normalize(
            self.types,
            &mut self.substitution_cache,
            &mut self.unification_table,
            idx,
        )
    }
    pub(super) fn substitute(&mut self) {
        self.expr_types
            .values_mut()
            .chain(self.defn_types.values_mut())
            .for_each(|idx| {
                *idx = normalize(
                    self.types,
                    &mut self.substitution_cache,
                    &mut self.unification_table,
                    *idx,
                );
            });
    }
}
