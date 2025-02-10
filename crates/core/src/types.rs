use std::collections::HashMap;

use indexmap::IndexSet;
use rand::random;

use crate::{intern, Interner, Name};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Unique(u32);

impl Unique {
    pub fn init() -> Self {
        let seed = u32::from(u16::MAX) + u32::from(random::<u16>()) * 0xFF;
        Self(seed)
    }

    pub fn next(&mut self) -> Unique {
        self.0 += 1;
        Self(self.0 - 1)
    }

    pub(crate) fn builtin(i: u16) -> Self {
        Self(u32::from(i))
    }
}

pub type TypeIdx = intern::Interned<Type>;

#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy, PartialOrd, Ord)]
pub(super) struct Level(pub u16);

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Type {
    Int,
    Bool,
    Unit,
    Error,
    Bound(Unique),
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

#[derive(Clone, PartialEq, Eq, Hash, Debug, Copy)]
pub struct Skolem {
    pub(crate) level: Level,
    pub(crate) tag: Unique,
}

impl Type {
    pub(crate) fn skolem(level: Level, tag: Unique) -> Type {
        Type::Skolem(Skolem { level, tag })
    }

    pub(crate) fn unifier(level: Level, tag: Unique) -> Type {
        Type::Unifier(Unifier { level, tag })
    }
}

pub struct GeneralizedLabels(HashMap<Unique, Name>);

impl GeneralizedLabels {
    #[must_use]
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    #[must_use]
    pub fn get_label(&self, names: &Interner<String>, tag: Unique) -> Option<String> {
        self.0.get(&tag).map(|name| names.lookup(*name).to_string())
    }

    pub fn add_label(&mut self, tag: Unique, name: Name) {
        let inserted = self.0.insert(tag, name);
        debug_assert!(inserted.is_none(), "Label already assigned");
    }

    pub fn add_if_absent<F: FnOnce() -> Name>(&mut self, tag: Unique, make_name: F) {
        self.0.entry(tag).or_insert_with(make_name);
    }
}

impl Default for GeneralizedLabels {
    fn default() -> Self {
        Self::new()
    }
}

#[must_use]
pub fn display_type(
    types: &Interner<Type>,
    names: &Interner<String>,
    labels: &GeneralizedLabels,
    idx: TypeIdx,
) -> String {
    struct TypeDisplayCtx<'a> {
        types: &'a Interner<Type>,
        names: &'a Interner<String>,
        labels: &'a GeneralizedLabels,
    }

    impl TypeDisplayCtx<'_> {
        fn get_label(&self, tag: Unique) -> String {
            self.labels
                .get_label(self.names, tag)
                .expect("Variable has no label.")
        }

        fn display_impl(&self, idx: TypeIdx) -> String {
            match *self.types.lookup(idx) {
                Type::Int => "@int".to_string(),
                Type::Bool => "@bool".to_string(),
                Type::Unit => "@unit".to_string(),
                Type::Error => "@Error".to_string(),
                Type::Unifier(unifier) => self.get_label(unifier.tag),
                Type::Skolem(Skolem { level: _, tag }) | Type::Bound(tag) => self.get_label(tag),
                Type::Arrow(from, to) => {
                    let from = self.display_impl(from);
                    let to = self.display_impl(to);
                    format!("({from} -> {to})")
                }
                Type::Link(typ, _) => self.display_impl(typ),
            }
        }
    }
    TypeDisplayCtx {
        types,
        names,
        labels,
    }
    .display_impl(idx)
}

#[must_use]
pub fn bound_variables(typ: TypeIdx, types: &Interner<Type>) -> Vec<Unique> {
    match *types.get_type(typ) {
        Type::Bound(id) => vec![id],
        Type::Arrow(from, to) => {
            let mut vars = bound_variables(from, types);
            vars.extend(bound_variables(to, types));
            let hashed: IndexSet<_> = vars.into_iter().collect();
            hashed.into_iter().collect()
        }
        _ => vec![],
    }
}
