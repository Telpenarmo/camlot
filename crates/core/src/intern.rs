use std::fmt::Debug;

use indexmap::IndexSet;

#[derive(Default, PartialEq)]
pub struct Interner<T: std::cmp::Eq + std::hash::Hash> {
    set: IndexSet<T>,
}

#[derive(PartialEq, Eq, Hash)]
pub struct Interned<T> {
    id: u32,
    phantom: std::marker::PhantomData<T>,
}

impl<T> Interned<T> {
    #[must_use]
    pub(crate) fn new(id: u32) -> Self {
        Self {
            id,
            phantom: std::marker::PhantomData,
        }
    }

    fn from_usize(id: usize) -> Self {
        Self::new(id.try_into().unwrap())
    }
}

impl<T> std::fmt::Debug for Interned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.id.fmt(f)
    }
}
impl<T> Clone for Interned<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> Copy for Interned<T> {}

impl<T> Interner<T>
where
    T: Eq + std::hash::Hash,
{
    #[must_use]
    pub fn new() -> Self {
        Self {
            set: IndexSet::new(),
        }
    }

    #[allow(clippy::missing_panics_doc)]
    pub fn intern(&mut self, value: T) -> Interned<T> {
        let (id, _) = self.set.insert_full(value);

        Interned::from_usize(id)
    }

    #[must_use]
    pub fn lookup(&self, id: Interned<T>) -> &T {
        &self.set[id.id as usize]
    }

    /// Swaps the value pointed to by `old` with `v`.
    ///
    /// # Panics
    ///
    /// Panics if `v` is already interned.
    pub fn replace_with_fresh(&mut self, old: Interned<T>, v: T) {
        let is_new = self.set.insert(v);
        assert!(is_new, "New element was already interned.");
        self.set.swap_remove_index(old.id as usize);
    }

    #[must_use]
    pub(crate) fn get_idx(&self, v: &T) -> Interned<T> {
        let id = self.set.get_index_of(v).unwrap();
        Interned::from_usize(id)
    }
}

impl<T: std::fmt::Debug + std::cmp::Eq + std::hash::Hash> Debug for Interner<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self.set.iter().enumerate()).finish()
    }
}

impl Interner<String> {
    pub(super) fn empty_name(&mut self) -> Interned<String> {
        self.intern("_".into())
    }

    pub(crate) fn name<S: Into<String>>(&mut self, name: S) -> Interned<String> {
        self.intern(name.into())
    }

    #[must_use]
    pub fn get_name(&self, name: Interned<String>) -> &str {
        self.lookup(name)
    }
}

impl Interner<crate::Type> {
    #[must_use]
    pub fn get_type(&self, idx: crate::TypeIdx) -> &crate::Type {
        let mut typ = self.lookup(idx);
        while let crate::Type::Link(t, _) = typ {
            typ = self.lookup(*t);
        }
        typ
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_interner() {
        let mut interner = Interner::new();
        let a = interner.intern("a");
        let b = interner.intern("b");
        assert_ne!(a, b);

        let a2 = interner.intern("a");
        assert_eq!(a, a2);
    }
}
