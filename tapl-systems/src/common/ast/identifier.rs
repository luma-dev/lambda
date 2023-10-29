use crate::{
    common::diagnostic::Spanned,
    format::{FormatContext, ToCode},
};
// use super::syntactic_eq::SyntacticEq;

#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier(String);
impl Identifier {
    pub fn new(s: String) -> Self {
        // checks if s satisfies first char is alphabet and chars from second position is alphanumerical or _
        let mut chars = s.chars();
        if let Some(c) = chars.next() {
            if !c.is_alphabetic() {
                panic!(
                    "first character of identifier must be alphabet, but got {}",
                    c
                );
            }
            for c in chars {
                if !c.is_alphanumeric() && c != '_' && c != '-' {
                    panic!("characters of identifier must be alphanumerical or _ or -");
                }
            }
        } else {
            panic!("identifier must not be empty");
        }
        Self(s)
    }
    pub fn as_str(&self) -> &str {
        &self.0
    }
}
impl ToString for Identifier {
    fn to_string(&self) -> String {
        self.0.clone()
    }
}
impl ToCode for Identifier {
    fn to_code(&self, _: &FormatContext) -> String {
        self.0.clone()
    }
}
impl From<String> for Identifier {
    fn from(s: String) -> Self {
        Self::new(s)
    }
}
impl From<&str> for Identifier {
    fn from(s: &str) -> Self {
        Self::new(s.to_string())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SpecialIdentifier(Identifier);
impl SpecialIdentifier {
    pub fn new(s: String) -> Self {
        Self(Identifier::new(s))
    }
    pub fn from_identifier(id: Identifier) -> Self {
        Self(id)
    }
    pub fn as_str(&self) -> &str {
        &self.0 .0
    }
}
impl ToString for SpecialIdentifier {
    fn to_string(&self) -> String {
        format!("${}", self.0.to_string())
    }
}
impl ToCode for SpecialIdentifier {
    fn to_code(&self, _: &FormatContext) -> String {
        format!("${}", self.0.to_string())
    }
}
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum QualifiedIdentifierPart {
    Normal(Identifier),
    Special(SpecialIdentifier),
}
impl QualifiedIdentifierPart {
    pub fn normal(&self) -> Option<&Identifier> {
        match self {
            Self::Normal(id) => Some(id),
            _ => None,
        }
    }

    pub fn special_identifier(&self) -> Option<&SpecialIdentifier> {
        match self {
            Self::Special(id) => Some(id),
            _ => None,
        }
    }

    fn as_ref(&self) -> QualifiedIdentifierPartRef<'_> {
        match self {
            Self::Normal(id) => QualifiedIdentifierPartRef::Normal(id),
            Self::Special(id) => QualifiedIdentifierPartRef::Special(id),
        }
    }
}
impl ToCode for QualifiedIdentifierPart {
    fn to_code(&self, _: &FormatContext) -> String {
        match self {
            Self::Normal(id) => id.to_string(),
            Self::Special(id) => id.to_string(),
        }
    }
}
#[derive(Clone, Copy, Debug)]
pub enum QualifiedIdentifierPartRef<'a> {
    Normal(&'a Identifier),
    Special(&'a SpecialIdentifier),
}
impl PartialEq for QualifiedIdentifierPartRef<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Normal(id1), Self::Normal(id2)) => id1 == id2,
            (Self::Special(id1), Self::Special(id2)) => id1 == id2,
            _ => false,
        }
    }
}
impl Eq for QualifiedIdentifierPartRef<'_> {}

#[derive(Clone, Debug)]
pub enum QualifiedIdentifier {
    Head(Spanned<QualifiedIdentifierPart>),
    Sub(
        Box<Spanned<QualifiedIdentifier>>,
        Spanned<QualifiedIdentifierPart>,
    ),
}
impl QualifiedIdentifier {
    pub fn new_head_normal(id: Spanned<Identifier>) -> Self {
        Self::Head(id.map(QualifiedIdentifierPart::Normal))
    }
    pub fn new_head_special(id: Spanned<SpecialIdentifier>) -> Self {
        Self::Head(id.map(QualifiedIdentifierPart::Special))
    }

    pub fn head(&self) -> Option<&Spanned<QualifiedIdentifierPart>> {
        match self {
            Self::Head(id) => Some(id),
            Self::Sub(qid, _) => qid.head(),
        }
    }
    pub fn head_normal(&self) -> Option<Spanned<&Identifier>> {
        self.head()
            .and_then(|Spanned { span, value: id }| id.normal().map(|id| Spanned::new(*span, id)))
    }
    pub fn head_special(&self) -> Option<Spanned<&SpecialIdentifier>> {
        self.head().and_then(|Spanned { span, value: id }| {
            id.special_identifier().map(|id| Spanned::new(*span, id))
        })
    }

    pub fn as_ref(&self) -> QualifiedIdentifierRef<'_> {
        QualifiedIdentifierRef::Full(self)
    }
}
impl ToCode for QualifiedIdentifier {
    fn to_code(&self, fmt_ctx: &FormatContext) -> String {
        match self {
            Self::Head(p) => match p.value {
                QualifiedIdentifierPart::Normal(_) => {
                    format!("$this::{}", p.to_code(fmt_ctx))
                }
                QualifiedIdentifierPart::Special(_) => p.to_code(fmt_ctx),
            },
            Self::Sub(qid, id) => format!("{}::{}", qid.to_code(fmt_ctx), id.to_code(fmt_ctx)),
        }
    }
}
impl PartialEq for QualifiedIdentifier {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Head(id1), Self::Head(id2)) => id1.value() == id2.value(),
            (Self::Sub(qid1, id1), Self::Sub(qid2, id2)) => {
                qid1 == qid2 && id1.value() == id2.value()
            }
            _ => false,
        }
    }
}
impl Eq for QualifiedIdentifier {}

#[derive(Clone, Copy, Debug)]
pub enum QualifiedIdentifierRef<'a> {
    HeadOnly(Spanned<QualifiedIdentifierPartRef<'a>>),
    Full(&'a QualifiedIdentifier),
}
impl<'a> QualifiedIdentifierRef<'a> {
    pub fn new_head_normal(id: Spanned<&'a Identifier>) -> Self {
        Self::HeadOnly(id.map(QualifiedIdentifierPartRef::Normal))
    }
    pub fn new_head_special(id: Spanned<&'a SpecialIdentifier>) -> Self {
        Self::HeadOnly(id.map(QualifiedIdentifierPartRef::Special))
    }
}
impl PartialEq for QualifiedIdentifierRef<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::HeadOnly(id1), Self::HeadOnly(id2)) => id1 == id2,
            (Self::Full(id1), Self::Full(id2)) => id1 == id2,
            (Self::HeadOnly(id1), Self::Full(QualifiedIdentifier::Head(id2))) => {
                *id1.value() == id2.value().as_ref()
            }
            (Self::Full(QualifiedIdentifier::Head(id1)), Self::HeadOnly(id2)) => {
                id1.value().as_ref() == *id2.value()
            }
            _ => false,
        }
    }
}
impl Eq for QualifiedIdentifierRef<'_> {}
