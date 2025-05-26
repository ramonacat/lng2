use std::collections::HashMap;

use crate::identifier::Identifier;

use super::object::StoredValue;

pub struct Scope<'ctx, 'class> {
    variables: HashMap<Identifier, StoredValue<'ctx, 'class>>,
    parent: Option<Box<Scope<'ctx, 'class>>>,
}

impl<'ctx, 'class> Scope<'ctx, 'class> {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            parent: None,
        }
    }

    pub fn into_child(self) -> Self {
        Self {
            variables: HashMap::new(),
            parent: Some(Box::new(self)),
        }
    }

    pub fn into_parent(self) -> Option<Self> {
        self.parent.map(|x| *x)
    }

    pub(crate) fn set(&mut self, name: Identifier, value: StoredValue<'ctx, 'class>) {
        self.variables.insert(name, value);
    }

    pub(crate) fn get(&self, identifier: Identifier) -> Option<&StoredValue<'ctx, 'class>> {
        self.variables
            .get(&identifier)
            .or_else(|| self.parent.as_ref().and_then(|x| x.get(identifier)))
    }
}
