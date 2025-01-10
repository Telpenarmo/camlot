use std::fmt::Display;

use ena::unify::{EqUnifyValue, InPlaceUnificationTable, UnifyKey};

use crate::{intern, Interner, Name};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UnificationVar(pub u32);
impl UnifyKey for UnificationVar {
    type Value = Option<TypeIdx>;

    fn index(&self) -> u32 {
        self.0
    }

    fn from_index(u: u32) -> Self {
        Self(u)
    }

    fn tag() -> &'static str {
        "UnificationVar"
    }
}

impl Display for UnificationVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("^{}", self.0))
    }
}

pub type TypeIdx = intern::Interned<Type>;

pub(crate) type UnificationTable = InPlaceUnificationTable<UnificationVar>;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Type {
    Int,
    Bool,
    Unit,
    Error,
    Var(Name),
    Unifier(UnificationVar),
    Arrow(TypeIdx, TypeIdx),
}

impl EqUnifyValue for TypeIdx {}

#[must_use]
pub fn display_type(types: &Interner<Type>, idx: TypeIdx) -> String {
    match types.lookup(idx) {
        Type::Int => "@int".to_string(),
        Type::Bool => "@bool".to_string(),
        Type::Unit => "@unit".to_string(),
        Type::Error => "{Error}".to_string(),
        Type::Unifier(var) => format!("{var}"),
        Type::Arrow(from, to) => {
            format!(
                "({} -> {})",
                display_type(types, *from),
                display_type(types, *to)
            )
        }
        Type::Var(_name) => todo!(),
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeScheme {
    pub params: Box<[Name]>,
    pub body: TypeIdx,
}

impl TypeScheme {
    pub fn empty(typ: TypeIdx) -> Self {
        Self {
            params: Box::new([]),
            body: typ,
        }
    }
}
