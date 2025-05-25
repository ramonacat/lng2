use std::collections::HashMap;

use crate::{
    ast,
    identifier::Identifier,
    types::{
        class::ClassId,
        expression::{ExpressionType, ExpressionTypeKind},
    },
};

#[derive(Debug)]
pub(super) enum IntermediateFieldType {
    Callable {
        arguments: Vec<ast::TypeConstraint>,
        return_type: ast::TypeConstraint,
    },
}

#[derive(Debug)]
pub(super) struct IntermediateClassType {
    id: ClassId,
    fields: HashMap<Identifier, IntermediateFieldType>,
}

impl IntermediateClassType {
    pub(crate) const fn new(
        id: ClassId,
        fields: HashMap<Identifier, IntermediateFieldType>,
    ) -> Self {
        Self { id, fields }
    }

    pub(crate) const fn to_expression_type(&self) -> ExpressionType {
        ExpressionType::new(ExpressionTypeKind::Class(self.id))
    }

    pub(crate) fn get_field(&self, identifier: Identifier) -> Option<&IntermediateFieldType> {
        self.fields.get(&identifier)
    }
}

#[derive(Debug)]
pub(super) struct Scope {
    // TODO support types other than Class
    values: HashMap<Identifier, IntermediateClassType>,
    parent: Option<Box<Scope>>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            parent: None,
        }
    }

    pub fn into_child(self) -> Self {
        Self {
            values: HashMap::new(),
            parent: Some(Box::new(self)),
        }
    }

    pub(crate) fn into_parent(self) -> Option<Self> {
        self.parent.map(|x| *x)
    }

    pub(crate) fn add(&mut self, name: Identifier, type_: IntermediateClassType) {
        self.values.insert(name, type_);
    }

    pub(crate) fn get(&self, identifier: Identifier) -> Option<&IntermediateClassType> {
        self.values
            .get(&identifier)
            .or_else(|| self.parent.as_ref().and_then(|x| x.get(identifier)))
    }

    pub(crate) fn get_class(&self, class_id: ClassId) -> Option<&IntermediateClassType> {
        self.values
            .iter()
            .find(|x| x.1.id == class_id)
            .map(|x| x.1)
            .or_else(|| self.parent.as_ref().and_then(|x| x.get_class(class_id)))
    }
}
