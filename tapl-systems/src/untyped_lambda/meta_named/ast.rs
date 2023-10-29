pub use super::super::named::ast::*;
pub use crate::common::ast::identifier::Identifier;
use std::collections::HashSet;

#[derive(Clone, Debug)]
pub struct Source {
    defs: Vec<Def>,
    term: NamedTerm,
}
impl Source {
    pub fn new(defs: Vec<Def>, term: NamedTerm) -> Self {
        let mut defined_names: HashSet<&Identifier> = HashSet::new();
        for def in defs.iter() {
            if defined_names.contains(&def.name) {
                panic!("duplicated definition: {}", def.name.as_str());
            }
            let undefined_names: HashSet<&Identifier> = def
                .term()
                .free_vars()
                .difference(&defined_names)
                .copied()
                .collect();
            if !undefined_names.is_empty() {
                panic!("undefined variables: {:?}", undefined_names);
            }
            defined_names.insert(def.name());
        }
        Self { defs, term }
    }
    pub fn defs(&self) -> &Vec<Def> {
        &self.defs
    }
    pub fn term(&self) -> &NamedTerm {
        &self.term
    }
}

impl Source {
    pub fn into_term(self) -> NamedTerm {
        let mut term = self.term;
        for def in self.defs.into_iter().rev() {
            term = term.substitute_identifier(&def.name, &def.term);
        }
        term
    }
}

#[derive(Clone, Debug)]
pub struct Def {
    name: Identifier,
    term: NamedTerm,
}
impl Def {
    pub fn new(name: Identifier, term: NamedTerm) -> Self {
        Self { name, term }
    }
    pub fn name(&self) -> &Identifier {
        &self.name
    }
    pub fn term(&self) -> &NamedTerm {
        &self.term
    }
}
