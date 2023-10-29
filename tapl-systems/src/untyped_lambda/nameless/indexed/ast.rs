use super::super::ast::NamelessTerm;
use std::fmt::{self, Display, Formatter};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct IndexedNamelessTerm(NamelessTerm);
impl IndexedNamelessTerm {
    pub fn new(t: NamelessTerm) -> Self {
        Self(t)
    }
    pub fn nameless_term(&self) -> &NamelessTerm {
        &self.0
    }
    pub fn into_nameless_term(self) -> NamelessTerm {
        self.0
    }
    pub fn is_value(term: &NamelessTerm, var_is_val: bool) -> bool {
        match term {
            NamelessTerm::Var(_) => var_is_val,
            NamelessTerm::Abs(_) => true,
            NamelessTerm::Apply(t1, t2) => {
                var_is_val
                    && match t1.as_ref() {
                        NamelessTerm::Abs(_) => false,
                        _ => Self::is_value(t1, var_is_val) && Self::is_value(t2, var_is_val),
                    }
            }
        }
    }
}
impl Display for IndexedNamelessTerm {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.to_code())
    }
}
