pub use crate::common::ast::identifier::Identifier;
use core::fmt::{self, Display, Formatter};
use std::collections::HashSet;

#[derive(Clone, Debug)]
pub enum Bind {
    Identifier(Identifier),
    Wildcard,
}

impl Bind {
    pub fn new_identifier(id: Identifier) -> Self {
        Self::Identifier(id)
    }
    pub fn new_wildcard() -> Self {
        Self::Wildcard
    }

    pub fn as_str(&self) -> &str {
        match self {
            Self::Identifier(id) => id.as_str(),
            Self::Wildcard => "_",
        }
    }
    pub fn to_code(&self) -> String {
        match self {
            Self::Identifier(id) => id.to_string(),
            Self::Wildcard => "_".to_string(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum NamedTerm {
    Var(Identifier),
    Abs {
        binds: Vec<Bind>,
        body: Box<NamedTerm>,
    },
    Apply(Box<NamedTerm>, Box<NamedTerm>),
}
impl NamedTerm {
    pub fn new_var(id: Identifier) -> Self {
        Self::Var(id)
    }
    pub fn new_abs(binds: Vec<Bind>, body: NamedTerm) -> Self {
        if binds.is_empty() {
            panic!("bind variables in lambda abstraction (NamedTerm::Abs) must not be empty");
        }
        Self::Abs {
            binds,
            body: body.into(),
        }
    }
    pub fn new_apply(t1: NamedTerm, t2: NamedTerm) -> Self {
        Self::Apply(t1.into(), t2.into())
    }

    pub fn free_vars(&self) -> HashSet<&Identifier> {
        match self {
            Self::Var(id) => {
                let mut free_vars = HashSet::new();
                free_vars.insert(id);
                free_vars
            }
            Self::Abs { binds, body } => {
                let mut free_vars = body.free_vars();
                for bind in binds {
                    match bind {
                        Bind::Identifier(id) => {
                            free_vars.retain(|e| e != &id);
                        }
                        Bind::Wildcard => {}
                    }
                }
                free_vars
            }
            Self::Apply(t1, t2) => {
                let mut free_vars = t1.free_vars();
                free_vars.extend(t2.free_vars());
                free_vars
            }
        }
    }

    pub fn to_code(&self) -> String {
        match self {
            Self::Var(id) => id.to_string(),
            Self::Abs { binds, body } => format!(
                r"(\{}.{})",
                binds
                    .iter()
                    .map(|b| b.to_code())
                    .collect::<Vec<String>>()
                    .join(" "),
                body.to_code()
            ),
            Self::Apply(t1, t2) => format!("{} {}", t1.to_code(), {
                match t2.as_ref() {
                    Self::Abs { .. } | Self::Apply(_, _) => format!("({})", t2.to_code()),
                    _ => t2.to_string(),
                }
            }),
        }
    }
    pub fn apply(&self, t: NamedTerm) -> NamedTerm {
        NamedTerm::Apply(Box::new(self.clone()), Box::new(t))
    }
}
impl Display for NamedTerm {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_code())
    }
}
