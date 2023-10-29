use super::identifier::{Identifier, QualifiedIdentifier, QualifiedIdentifierRef};
use crate::{
    common::diagnostic::Spanned,
    format::{FormatContext, ToCode},
};

#[derive(Clone, Debug, Default, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct LocalVariable {
    pub id: Identifier,
    pub discriminator: usize,
}
impl LocalVariable {
    pub fn new(id: Identifier, discriminator: usize) -> Self {
        Self { id, discriminator }
    }
    pub fn id(&self) -> &Identifier {
        &self.id
    }
    pub fn map_id<F: FnOnce(Identifier) -> Identifier>(self, f: F) -> Self {
        Self {
            id: f(self.id),
            discriminator: self.discriminator,
        }
    }
    pub fn map_discriminator<F: FnOnce(usize) -> usize>(self, f: F) -> Self {
        Self {
            id: self.id,
            discriminator: f(self.discriminator),
        }
    }
    // pub fn syntactic_eq_id(&self, other: &Identifier) -> bool {
    //     self.id.syntactic_eq(other) && self.discriminator == 0
    // }
    pub fn as_ref(&self) -> LocalVariableRef<'_> {
        LocalVariableRef {
            id: &self.id,
            discriminator: self.discriminator,
        }
    }
}
impl ToCode for LocalVariable {
    fn to_code(&self, _fmt_ctx: &FormatContext) -> String {
        self.id.as_str().to_string()
        // format!("{}#{}", self.id.as_str(), self.discriminator)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LocalVariableRef<'a> {
    pub id: &'a Identifier,
    pub discriminator: usize,
}
impl<'a> From<&'a LocalVariable> for LocalVariableRef<'a> {
    fn from(lvar: &'a LocalVariable) -> Self {
        Self {
            id: &lvar.id,
            discriminator: lvar.discriminator,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Variable {
    Local(LocalVariable),
    Qualified(QualifiedIdentifier),
}
impl Variable {
    // pub fn new_local_numbered(id: Identifier, number: usize) -> Self {
    //     Self::Local(LocalVariable::new_normal(id, number))
    // }
    // pub fn new_local_any(id: Identifier) -> Self {
    //     Self::Local(LocalVariable::new_any(id))
    // }
    pub fn new_local(id: Identifier, discriminator: usize) -> Self {
        Self::Local(LocalVariable::new(id, discriminator))
    }
    pub fn new_qualified(qid: QualifiedIdentifier) -> Self {
        Self::Qualified(qid)
    }
    pub fn new_qualified_head(id: Spanned<Identifier>) -> Self {
        Self::Qualified(QualifiedIdentifier::new_head_normal(id))
    }
    pub fn local(&self) -> Option<&LocalVariable> {
        match self {
            Self::Local(lv) => Some(lv),
            Self::Qualified(_) => None,
        }
    }
    pub fn qualified(&self) -> Option<&QualifiedIdentifier> {
        match self {
            Self::Local(_) => None,
            Self::Qualified(qid) => Some(qid),
        }
    }
    pub fn id(&self) -> &Identifier {
        match self {
            Self::Local(lv) => lv.id(),
            Self::Qualified(qid) => qid.head_normal().unwrap().value(),
        }
    }
    pub fn is_local(&self) -> bool {
        self.local().is_some()
    }
    pub fn is_qualified(&self) -> bool {
        self.qualified().is_some()
    }

    pub fn as_ref(&self) -> VariableRef<'_> {
        match self {
            Self::Local(lvar) => VariableRef::Local(lvar.as_ref()),
            Self::Qualified(qid) => VariableRef::Qualified(qid.as_ref()),
        }
    }
}
impl ToCode for Variable {
    fn to_code(&self, fmt_ctx: &FormatContext) -> String {
        match self {
            Self::Local(local) => local.to_code(fmt_ctx),
            Self::Qualified(qid) => qid.to_code(fmt_ctx),
        }
    }
}
impl PartialEq for Variable {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Local(lvar1), Self::Local(lvar2)) => lvar1 == lvar2,
            (Self::Qualified(qid1), Self::Qualified(qid2)) => qid1 == qid2,
            _ => false,
        }
    }
}
impl Eq for Variable {}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum VariableRef<'a> {
    Local(LocalVariableRef<'a>),
    Qualified(QualifiedIdentifierRef<'a>),
}
impl<'a> VariableRef<'a> {
    pub fn new_local(id: &'a Identifier, discriminator: usize) -> Self {
        Self::Local(LocalVariableRef { id, discriminator })
    }
    pub fn new_qualified(qid: QualifiedIdentifierRef<'a>) -> Self {
        Self::Qualified(qid)
    }
    pub fn new_qualified_head_normal(id: Spanned<&'a Identifier>) -> Self {
        Self::Qualified(QualifiedIdentifierRef::new_head_normal(id))
    }

    pub fn local(&self) -> Option<&LocalVariableRef<'_>> {
        match self {
            Self::Local(lvar) => Some(lvar),
            Self::Qualified(_) => None,
        }
    }
}
impl<'a> From<LocalVariableRef<'a>> for VariableRef<'a> {
    fn from(lvar: LocalVariableRef<'a>) -> Self {
        Self::Local(lvar)
    }
}
impl<'a> From<&'a LocalVariable> for VariableRef<'a> {
    fn from(lvar: &'a LocalVariable) -> Self {
        Self::Local(lvar.into())
    }
}
