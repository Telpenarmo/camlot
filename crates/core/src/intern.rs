use std::{borrow::Borrow, cmp, fmt, fmt::Debug, hash, marker};

use indexmap::IndexSet;

#[derive(Default, PartialEq)]
pub struct Interner<T: cmp::Eq + hash::Hash> {
    set: IndexSet<T>,
}

#[derive(PartialEq, Eq, Hash)]
pub struct Interned<T> {
    id: u32,
    phantom: marker::PhantomData<T>,
}

impl<T> Interned<T> {
    #[must_use]
    pub(crate) fn new(id: u32) -> Self {
        Self {
            id,
            phantom: marker::PhantomData,
        }
    }

    fn from_usize(id: usize) -> Self {
        Self::new(id.try_into().unwrap())
    }
}

impl<T> fmt::Debug for Interned<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
    T: Eq + hash::Hash,
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

    /// Returns an index to previously interned item.
    ///
    /// # Panics
    ///
    /// Panics if `value` is not interned.
    pub fn idx_of<Q>(&self, value: &Q) -> Interned<T>
    where
        T: Borrow<Q>,
        Q: ?Sized + cmp::Eq + hash::Hash + std::fmt::Debug,
    {
        let id = self
            .set
            .get_index_of(value)
            .unwrap_or_else(|| panic!("Value {value:?} is not interned"));
        Interned::from_usize(id)
    }
}

impl<T: fmt::Debug + cmp::Eq + hash::Hash> Debug for Interner<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
