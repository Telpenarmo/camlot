use imbl::HashMap;

use crate::{Name, TypeIdx};

use super::{TypeError, TypeInference};

#[derive(Clone)]
pub(super) struct Environment(HashMap<Name, TypeIdx>);

impl From<HashMap<Name, TypeIdx>> for Environment {
    fn from(value: HashMap<Name, TypeIdx>) -> Self {
        Self(value)
    }
}

impl FromIterator<(Name, TypeIdx)> for Environment {
    fn from_iter<T: IntoIterator<Item = (Name, TypeIdx)>>(iter: T) -> Self {
        Self(HashMap::from_iter(iter))
    }
}

impl Environment {
    pub(super) fn union(self, other: Self) -> Self {
        Self(self.0.union(other.0))
    }

    pub(super) fn update(&self, name: Name, typ: TypeIdx) -> Self {
        Self(self.0.update(name, typ))
    }
}

impl TypeInference<'_> {
    pub(super) fn env_get(&mut self, env: &Environment, name: Name) -> Option<TypeIdx> {
        env.0.get(&name).map(|typ| self.instantiate(*typ))
    }

    pub(super) fn env_insert(&mut self, env: &mut Environment, name: Name, typ: TypeIdx) {
        if name != self.names.empty_name() {
            env.0.insert(name, typ);
        }
    }

    pub(super) fn insert_new<F: Fn(Name) -> TypeError>(
        &mut self,
        env: &mut Environment,
        name: Name,
        typ: TypeIdx,
        error: F,
    ) {
        if name == self.names.empty_name() {
            return;
        }
        match env.0.entry(name) {
            imbl::hashmap::Entry::Occupied(_prev) => {
                self.errors.push(error(name));
            }
            imbl::hashmap::Entry::Vacant(entry) => {
                entry.insert(typ);
            }
        }
    }
}
