use crate::common::evaluator::small_step::*;
use crate::untyped_lambda::named::ast::{Bind, NamedTerm};

#[derive(Clone, Debug)]
pub enum Eval1Tree {
    EApp {
        premise: Box<Eval1Tree>,
        t2: NamedTerm,
    },
    EAppAbs {
        bind: Bind,
        t1_2: NamedTerm,
        t2: NamedTerm,
    },
}

impl Eval1Tree {
    pub fn premise(&self) -> Option<&Eval1Tree> {
        match self {
            Self::EApp { premise, .. } => Some(premise),
            Self::EAppAbs { .. } => None,
        }
    }
}
impl SmallStepTree for Eval1Tree {
    type Stmt = NamedTerm;
    fn premises(&self) -> Vec<&Self> {
        self.premise().into_iter().collect()
    }
    fn conclusion(&self) -> SmallStepRelation<Self::Stmt> {
        match self {
            Self::EApp { premise, t2 } => {
                let SmallStepRelation {
                    from: t1_1,
                    to: t1_2,
                } = premise.conclusion();
                let from = NamedTerm::Apply(t1_1.into(), t2.clone().into());
                let to = NamedTerm::Apply(t1_2.into(), t2.clone().into());
                SmallStepRelation { from, to }
            }
            Self::EAppAbs { bind, t1_2, t2 } => {
                let from = NamedTerm::Apply(
                    NamedTerm::Abs {
                        binds: vec![bind.clone()],
                        body: t1_2.clone().into(),
                    }
                    .into(),
                    t2.clone().into(),
                );
                let to = t1_2.substitute(bind, t2);
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
}
impl SmallStepConstructor for Evaluator {
    type Stmt = NamedTerm;
    type Tree = Eval1Tree;
    fn construct_tree(&self, term: &Self::Stmt) -> Option<Self::Tree> {
        match term {
            NamedTerm::Var(_) => None,
            NamedTerm::Abs { .. } => None,
            NamedTerm::Apply(t1, t2) => {
                if !t1.is_value(self.var_is_val) {
                    self.construct_tree(t1).map(|tree| Eval1Tree::EApp {
                        premise: tree.into(),
                        t2: *t2.clone(),
                    })
                } else {
                    match t1.as_ref() {
                        NamedTerm::Abs { binds, body } => {
                            let bind = binds[0].clone();
                            let body = if binds.len() > 1 {
                                NamedTerm::Abs {
                                    binds: binds[1..].to_vec(),
                                    body: body.clone(),
                                }
                            } else {
                                *body.clone()
                            };
                            let tree = Eval1Tree::EAppAbs {
                                bind,
                                t1_2: body,
                                t2: *t2.clone(),
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
