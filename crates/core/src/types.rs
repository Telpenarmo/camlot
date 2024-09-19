use ena::unify::{EqUnifyValue, InPlaceUnificationTable, UnifyKey};

use crate::{intern, Name};

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
    Var(Name),
    Unifier(UnificationVar),
    Arrow(TypeIdx, TypeIdx),
}

impl EqUnifyValue for TypeIdx {}
