pub use crate::common::ast::identifier::Identifier;
use std::fmt::{self, Display, Formatter};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum NamelessTerm {
    Var(usize),
    Abs(Box<NamelessTerm>),
    Apply(Box<NamelessTerm>, Box<NamelessTerm>),
}
impl NamelessTerm {
    pub fn to_latex(&self) -> String {
        match self {
            Self::Var(i) => format!("#_{{{}}}", i),
            Self::Abs(t) => format!(r"\lambda.{}", t.to_latex()),
            Self::Apply(t1, t2) => format!(r"{} \medspace {}", t1.to_latex(), {
                match t2.as_ref() {
                    Self::Abs(_) | Self::Apply(_, _) => format!("({})", t2.to_latex()),
                    _ => t2.to_latex(),
                }
            }),
        }
    }
    pub fn to_code(&self) -> String {
        match self {
            Self::Var(i) => format!("#{}", i),
            Self::Abs(t) => format!(r"(\.{})", t.to_code()),
            Self::Apply(t1, t2) => format!("{} {}", t1.to_code(), {
                match t2.as_ref() {
                    Self::Abs(_) | Self::Apply(_, _) => format!("({})", t2.to_code()),
                    _ => t2.to_code(),
                }
            }),
        }
    }
}
impl Display for NamelessTerm {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_code())
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct LeveledNamelessTerm(NamelessTerm);
impl LeveledNamelessTerm {
    pub fn new(t: NamelessTerm) -> Self {
        Self(t)
    }
}
