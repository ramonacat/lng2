use string_interner::{StringInterner, backend::StringBackend, symbol::SymbolUsize};

#[derive(Debug, Clone, Copy)]
pub struct Identifier(SymbolUsize);

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
