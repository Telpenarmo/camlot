use imbl::{hashmap, HashMap};

use crate::{
    intern::Interner,
    types::{Type, TypeScheme},
    Module, Name,
};

#[must_use]
pub(crate) fn builtin_types(
    module: &mut Module,
    types: &mut Interner<Type>,
) -> HashMap<Name, TypeScheme> {
    hashmap! {
        module.name("unit") => TypeScheme::empty(types.intern(Type::Unit)),
        module.name("bool") => TypeScheme::empty(types.intern(Type::Bool)),
        module.name("int") => TypeScheme::empty(types.intern(Type::Int)),
    }
}

#[must_use]
pub(crate) fn builtin_defs(
    module: &mut Module,
    types: &mut Interner<Type>,
) -> HashMap<Name, TypeScheme> {
    let bool = types.intern(Type::Bool);
    let int = types.intern(Type::Int);
    let bool_to_bool = types.intern(Type::Arrow(bool, bool));
    let bool_to_bool_to_bool = types.intern(Type::Arrow(bool, bool_to_bool));

    let int_to_int = types.intern(Type::Arrow(int, int));
    let int_to_int_to_int = types.intern(Type::Arrow(int, int_to_int));

    let t_n = module.name("t");
    let t = types.intern(Type::Var(t_n));
    let t_to_t = types.intern(Type::Arrow(t, t));
    let t_to_tt = types.intern(Type::Arrow(t, t_to_t));
    let bool_to_ttt = types.intern(Type::Arrow(bool, t_to_tt));

    hashmap! {
        module.name("not") => TypeScheme::empty(bool_to_bool),
        module.name("or") => TypeScheme::empty(bool_to_bool_to_bool),
        module.name("and") => TypeScheme::empty(bool_to_bool_to_bool),
        module.name("add") => TypeScheme::empty(int_to_int_to_int),
        module.name("sub") => TypeScheme::empty(int_to_int_to_int),
        module.name("mul") => TypeScheme::empty(int_to_int_to_int),
        module.name("if") => TypeScheme { params: Box::new([t_n]), body: bool_to_ttt },
    }
}
