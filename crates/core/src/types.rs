use ena::unify::{EqUnifyValue, InPlaceUnificationTable, UnifyKey};

use crate::{intern, Interner};

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

pub type TypeIdx = intern::Interned<Type>;

pub(crate) type UnificationTable = InPlaceUnificationTable<UnificationVar>;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Type {
    Int,
    Bool,
    Unit,
    Error,
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
        Type::Unifier(var) => format!("_{}", var.0),
        Type::Arrow(from, to) => {
            format!(
                "{} -> {}",
                display_type(types, *from),
                display_type(types, *to)
            )
        }
    }
}
