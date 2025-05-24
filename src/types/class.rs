use std::sync::atomic::AtomicU64;

use crate::{ast::NodeType, identifier::Identifiers};

#[derive(Debug)]
pub struct UncheckedClassType {}
impl UncheckedClassType {
    pub(crate) const fn new() -> Self {
        Self {}
    }
}

impl NodeType for UncheckedClassType {
    fn pretty(&self, _identifiers: &Identifiers) -> String {
        "(unchecked-class) ".to_string()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ClassId(u64);

pub struct ClassIdGenerator {
    current: AtomicU64,
}

impl ClassIdGenerator {
    pub const fn new() -> Self {
        Self {
            current: AtomicU64::new(0),
        }
    }

    pub fn next(&self) -> ClassId {
        ClassId(
            self.current
                .fetch_add(1, std::sync::atomic::Ordering::AcqRel),
        )
    }
}

#[derive(Debug)]
pub struct ClassType {
    id: ClassId,
}

impl ClassType {
    pub(crate) const fn new(id: ClassId) -> Self {
        Self { id }
    }

    pub(crate) const fn id(&self) -> ClassId {
        self.id
    }
}

impl NodeType for ClassType {
    fn pretty(&self, _identifiers: &Identifiers) -> String {
        "(class) ".to_string()
    }
}
