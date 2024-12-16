use std::collections::HashMap;

use crate::infer::arrow;
use la_arena::ArenaMap;

use crate::hir::ExprIdx;
use crate::intern::Interner;
use crate::types::{Type, TypeIdx, UnificationTable};
use crate::DefinitionIdx;

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

impl TypeInference {
    pub(super) fn substitute(
        types: &mut Interner<Type>,
        expr_types: &mut ArenaMap<ExprIdx, TypeIdx>,
        defn_types: &mut ArenaMap<DefinitionIdx, TypeIdx>,
        unification_table: &mut UnificationTable,
    ) {
        let mut old_to_new = HashMap::new();

        expr_types.values_mut().for_each(|idx| {
            *idx = normalize(types, &mut old_to_new, unification_table, *idx);
        });

        defn_types.values_mut().for_each(|idx| {
            *idx = normalize(types, &mut old_to_new, unification_table, *idx);
        });
    }
}
