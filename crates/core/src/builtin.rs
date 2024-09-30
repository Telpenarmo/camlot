use im::{hashmap, HashMap};

use crate::{intern::Interner, types::Type, Module, Name, TypeIdx};

#[must_use]
pub(crate) fn builtin_types(
    module: &mut Module,
    types: &mut Interner<Type>,
) -> HashMap<Name, TypeIdx> {
    hashmap! {
        module.name("unit") => types.intern(Type::Unit),
        module.name("bool") => types.intern(Type::Bool),
        module.name("int") => types.intern(Type::Int),
    }
}

#[must_use]
pub(crate) fn builtin_defs(
    module: &mut Module,
    types: &mut Interner<Type>,
) -> HashMap<Name, TypeIdx> {
    let bool = types.intern(Type::Bool);
    let int = types.intern(Type::Int);
    let bool_to_bool = types.intern(Type::Arrow(bool, bool));
    let bool_to_bool_to_bool = types.intern(Type::Arrow(bool, bool_to_bool));

    let int_to_int = types.intern(Type::Arrow(int, int));
    let int_to_int_to_int = types.intern(Type::Arrow(int, int_to_int));

    hashmap! {
        module.name("not") => bool_to_bool,
        module.name("or") => bool_to_bool_to_bool,
        module.name("and") => bool_to_bool_to_bool,
        module.name("add") => int_to_int_to_int,
        module.name("sub") => int_to_int_to_int,
        module.name("mul") => int_to_int_to_int,
    }
}
