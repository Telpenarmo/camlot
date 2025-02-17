use std::fmt::Display;

use crate::{intern, Interner, Name};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Unique(pub u16);

impl Display for Unique {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.0))
    }
}

pub type TypeIdx = intern::Interned<Type>;

#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy, PartialOrd, Ord)]
pub(super) struct Level(pub u16);

impl Display for Level {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.0))
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Type {
    Int,
    Bool,
    Unit,
    Error,
    Bound(u16, Name),
    Skolem(Skolem),
    Unifier(Unifier),
    Arrow(TypeIdx, TypeIdx),
    Link(TypeIdx, Unique),
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub struct Unifier {
    pub(crate) level: Level,
    pub(crate) tag: Unique,
}

impl Display for Unifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.tag))
    }
}
#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub(crate) struct Skolem {
    pub(crate) name: Name,
    pub(crate) level: Level,
    pub(crate) tag: Unique,
}

#[must_use]
pub fn display_type(types: &Interner<Type>, names: &Interner<String>, idx: TypeIdx) -> String {
    match types.lookup(idx) {
        Type::Int => "@int".to_string(),
        Type::Bool => "@bool".to_string(),
        Type::Unit => "@unit".to_string(),
        Type::Error => "{Error}".to_string(),
        Type::Unifier(unifier) => {
            format!("?{}_{}", unifier.tag, unifier.level)
        }
        Type::Arrow(from, to) => {
            format!(
                "({} -> {})",
                display_type(types, names, *from),
                display_type(types, names, *to)
            )
        }
        Type::Bound(_, name) | Type::Skolem(Skolem { name, .. }) => {
            names.get_name(*name).to_string()
        }
        Type::Link(typ, _) => display_type(types, names, *typ),
    }
}

#[must_use]
pub fn bound_variables(typ: TypeIdx, types: &Interner<Type>) -> Vec<Name> {
    match *types.lookup(typ) {
        Type::Bound(_, name) => vec![name],
        Type::Arrow(from, to) => {
            let mut vars = bound_variables(from, types);
            vars.extend(bound_variables(to, types));
            vars
        }
        Type::Link(typ, _) => bound_variables(typ, types),
        _ => vec![],
    }
}
