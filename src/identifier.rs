use string_interner::{StringInterner, Symbol, backend::StringBackend, symbol::SymbolUsize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Identifier(SymbolUsize);

impl Identifier {
    #[allow(unused)]
    pub(crate) fn from_id(val: usize) -> Self {
        Self(SymbolUsize::try_from_usize(val).unwrap())
    }

    pub(crate) fn into_id(self) -> usize {
        self.0.to_usize()
    }
}

pub struct Identifiers {
    interner: StringInterner<StringBackend<SymbolUsize>>,
}

impl Identifiers {
    pub(crate) fn new() -> Self {
        Self {
            interner: StringInterner::new(),
        }
    }

    pub(crate) fn intern(&mut self, string: &str) -> Identifier {
        Identifier(self.interner.get_or_intern(string))
    }

    #[allow(unused)]
    pub(crate) fn resolve(&self, identifier: Identifier) -> &str {
        self.interner.resolve(identifier.0).unwrap()
    }
}
