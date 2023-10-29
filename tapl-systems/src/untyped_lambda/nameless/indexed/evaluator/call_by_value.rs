use crate::common::evaluator::small_step::*;
use crate::untyped_lambda::{
    nameless::ast::NamelessTerm, nameless::indexed::ast::IndexedNamelessTerm,
};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Eval1Tree {
    EApp1 {
        premise: Box<Self>,
        t2: IndexedNamelessTerm,
    },
    EApp2 {
        premise: Box<Self>,
        v1: IndexedNamelessTerm,
    },
    EAppAbs {
        t1_2: IndexedNamelessTerm,
        v2: IndexedNamelessTerm,
    },
}
impl Eval1Tree {
    pub fn premise(&self) -> Option<&Self> {
        match self {
            Self::EApp1 { premise, .. } => Some(premise),
            Self::EApp2 { premise, .. } => Some(premise),
            Self::EAppAbs { .. } => None,
        }
    }
}
impl SmallStepTree for Eval1Tree {
    type Stmt = IndexedNamelessTerm;
    fn premises(&self) -> Vec<&Self> {
        self.premise().into_iter().collect()
    }
    fn conclusion(&self) -> SmallStepRelation<IndexedNamelessTerm> {
        match self {
            Self::EApp1 { premise, t2 } => {
                let SmallStepRelation {
                    from: t1_1,
                    to: t1_2,
                } = premise.conclusion();
                let from = IndexedNamelessTerm::new(NamelessTerm::Apply(
                    t1_1.into_nameless_term().into(),
                    t2.clone().into_nameless_term().into(),
                ));
                let to = IndexedNamelessTerm::new(NamelessTerm::Apply(
                    t1_2.into_nameless_term().into(),
                    t2.clone().into_nameless_term().into(),
                ));
                SmallStepRelation { from, to }
            }
            Self::EApp2 { premise, v1 } => {
                let SmallStepRelation {
                    from: t2_1,
                    to: t2_2,
                } = premise.conclusion();
                let from = IndexedNamelessTerm::new(NamelessTerm::Apply(
                    v1.clone().into_nameless_term().into(),
                    t2_1.into_nameless_term().into(),
                ));
                let to = IndexedNamelessTerm::new(NamelessTerm::Apply(
                    v1.clone().into_nameless_term().into(),
                    t2_2.into_nameless_term().into(),
                ));
                SmallStepRelation { from, to }
            }
            Self::EAppAbs { t1_2, v2 } => {
                let from = IndexedNamelessTerm::new(NamelessTerm::Apply(
                    NamelessTerm::Abs(t1_2.clone().into_nameless_term().into()).into(),
                    v2.clone().into_nameless_term().into(),
                ));
                let to = t1_2.clone().substitute(0, &v2.shift(1, 0)).shift(-1, 0);
                SmallStepRelation { from, to }
            }
        }
    }
}

pub struct Evaluator {
    var_is_val: bool,
}
impl Evaluator {
    pub fn new(var_is_val: bool) -> Self {
        Self { var_is_val }
    }
    pub fn construct_tree_nameless_term(&self, term: &NamelessTerm) -> Option<Eval1Tree> {
        match term {
            NamelessTerm::Var(_) => None,
            NamelessTerm::Abs(_) => None,
            NamelessTerm::Apply(t1, t2) => {
                if !IndexedNamelessTerm::is_value(t1, self.var_is_val) {
                    self.construct_tree_nameless_term(t1)
                        .map(|tree| Eval1Tree::EApp1 {
                            premise: tree.into(),
                            t2: IndexedNamelessTerm::new(*t2.clone()),
                        })
                } else if !IndexedNamelessTerm::is_value(t2, self.var_is_val) {
                    self.construct_tree_nameless_term(t2)
                        .map(|tree| Eval1Tree::EApp2 {
                            premise: Box::new(tree),
                            v1: IndexedNamelessTerm::new(*t1.clone()),
                        })
                } else {
                    match t1.as_ref() {
                        NamelessTerm::Abs(body) => {
                            let tree = Eval1Tree::EAppAbs {
                                t1_2: IndexedNamelessTerm::new(*body.clone()),
                                v2: IndexedNamelessTerm::new(*t2.clone()),
                            };
                            Some(tree)
                        }
                        _ => None,
                    }
                }
            }
        }
    }
}
impl SmallStepConstructor for Evaluator {
    type Stmt = IndexedNamelessTerm;
    type Tree = Eval1Tree;
    fn construct_tree(&self, term: &Self::Stmt) -> Option<Self::Tree> {
        self.construct_tree_nameless_term(term.nameless_term())
    }
}
