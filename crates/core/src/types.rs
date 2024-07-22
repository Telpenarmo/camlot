use ena::unify::{EqUnifyValue, InPlaceUnificationTable, UnifyKey};

use crate::Name;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UnificationVar(pub u32);
impl UnifyKey for UnificationVar {
    type Value = Option<Type>;

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

pub(crate) type UnificationTable = InPlaceUnificationTable<UnificationVar>;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Type {
    Int,
    Bool,
    Unit,
    Error,
    Var(Name),
    Unifier(UnificationVar),
    Arrow(Box<Type>, Box<Type>),
}

impl Type {
    pub(crate) fn arrow(from: Type, to: Type) -> Type {
        Type::Arrow(Box::new(from), Box::new(to))
    }
}

impl EqUnifyValue for Type {}
