use imbl::{hashmap, HashMap};

use crate::{
    intern::Interner,
    types::{Type, Unique},
    Name, TypeIdx,
};

#[must_use]
pub(crate) fn builtin_types(
    names: &mut Interner<String>,
    types: &mut Interner<Type>,
) -> HashMap<Name, TypeIdx> {
    hashmap! {
        names.name("unit") => types.intern(Type::Unit),
        names.name("bool") => types.intern(Type::Bool),
        names.name("int") => types.intern(Type::Int),
    }
}

#[must_use]
pub(crate) fn builtin_defs(
    names: &mut Interner<String>,
    types: &mut Interner<Type>,
) -> HashMap<Name, TypeIdx> {
    let bool = types.intern(Type::Bool);
    let int = types.intern(Type::Int);
    let bool_to_bool = types.intern(Type::Arrow(bool, bool));
    let bool_to_bool_to_bool = types.intern(Type::Arrow(bool, bool_to_bool));

    let int_to_int = types.intern(Type::Arrow(int, int));
    let int_to_int_to_int = types.intern(Type::Arrow(int, int_to_int));

    let t = types.intern(Type::Bound(Unique::builtin(0)));
    let t_to_t = types.intern(Type::Arrow(t, t));
    let t_to_tt = types.intern(Type::Arrow(t, t_to_t));
    let bool_to_ttt = types.intern(Type::Arrow(bool, t_to_tt));

    hashmap! {
        names.name("not") => bool_to_bool,
        names.name("or") => bool_to_bool_to_bool,
        names.name("and") => bool_to_bool_to_bool,
        names.name("add") => int_to_int_to_int,
        names.name("sub") => int_to_int_to_int,
        names.name("mul") => int_to_int_to_int,
        names.name("if") => bool_to_ttt,
    }
}
