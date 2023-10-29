use super::ast::Term;
use std::collections::BTreeSet;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Const {
    True,
    False,
    Zero,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Value {
    Bool(bool),
    Nat(u32),
}

impl Term {
    fn to_value(&self) -> Option<Value> {
        match self {
            Term::True => Some(Value::Bool(true)),
            Term::False => Some(Value::Bool(false)),
            Term::Zero => Some(Value::Nat(0)),
            Term::Succ(t) => match t.to_value() {
                Some(Value::Nat(n)) => Some(Value::Nat(n + 1)),
                _ => None,
            },
            _ => None,
        }
    }
}

impl From<Value> for Term {
    fn from(v: Value) -> Self {
        v.to_term()
    }
}

impl Value {
    pub fn to_term(&self) -> Term {
        match self {
            Value::Bool(v) => {
                if *v {
                    Term::True
                } else {
                    Term::False
                }
            }
            Value::Nat(n) => {
                let mut t = Term::Zero;
                for _ in 0..*n {
                    t = Term::Succ(Box::new(t));
                }
                t
            }
        }
    }
    pub fn to_latex(&self) -> String {
        match self {
            Value::Bool(v) => {
                if *v {
                    r"\mathrm{true}".to_string()
                } else {
                    r"\mathrm{false}".to_string()
                }
            }
            Value::Nat(n) => n.to_string(),
        }
    }
}

/// Extract all constants as set
pub fn consts(t: &Term) -> BTreeSet<Const> {
    match t {
        Term::True => [Const::True].into(),
        Term::False => [Const::False].into(),
        Term::If {
            cond,
            then_clause,
            else_clause,
        } => consts(cond)
            .union(&consts(then_clause))
            .cloned()
            .collect::<BTreeSet<Const>>()
            .union(&consts(else_clause))
            .cloned()
            .collect(),
        Term::Zero => [Const::Zero].into(),
        Term::Succ(t) => consts(t),
        Term::Pred(t) => consts(t),
        Term::Iszero(t) => consts(t),
    }
}

/// Size of term
pub fn size(t: &Term) -> usize {
    match t {
        Term::True => 1,
        Term::False => 1,
        Term::If {
            cond,
            then_clause,
            else_clause,
        } => size(cond) + size(then_clause) + size(else_clause),
        Term::Zero => 1,
        Term::Succ(t) => size(t) + 1,
        Term::Pred(t) => size(t) + 1,
        Term::Iszero(t) => size(t) + 1,
    }
}

/// Depth of term
pub fn depth(t: &Term) -> usize {
    match t {
        Term::True => 1,
        Term::False => 1,
        Term::If {
            cond,
            then_clause,
            else_clause,
        } => depth(cond).max(depth(then_clause)).max(depth(else_clause)) + 1,
        Term::Zero => 1,
        Term::Succ(t) => depth(t) + 1,
        Term::Pred(t) => depth(t) + 1,
        Term::Iszero(t) => depth(t) + 1,
    }
}

pub fn value_of(t: &Term) -> Option<Value> {
    match t {
        Term::True => Some(Value::Bool(true)),
        Term::False => Some(Value::Bool(false)),
        Term::Zero => Some(Value::Nat(0)),
        Term::Succ(t) => value_of(t).and_then(|e| match e {
            Value::Nat(n) => Some(Value::Nat(n + 1)),
            _ => None,
        }),
        _ => None,
    }
}

pub fn is_value(t: &Term) -> bool {
    match t {
        Term::True => true,
        Term::False => true,
        _ => is_number_value(t),
    }
}

pub fn is_number_value(t: &Term) -> bool {
    match t {
        Term::Zero => true,
        Term::Succ(t) => is_number_value(t),
        _ => false,
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Eval1StepRelation {
    from: Term,
    to: Term,
}

impl Eval1StepRelation {
    pub fn from(&self) -> &Term {
        &self.from
    }
    pub fn to(&self) -> &Term {
        &self.to
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Eval1Tree {
    EIfTrue {
        t2: Term,
        t3: Term,
    },
    EIfFalse {
        t2: Term,
        t3: Term,
    },
    EIf {
        premise: Box<Eval1Tree>,
        t2: Term,
        t3: Term,
    },
    ESucc {
        premise: Box<Eval1Tree>,
    },
    EPredZero,
    EPredSucc {
        nv: Value,
    },
    EPred {
        premise: Box<Eval1Tree>,
    },
    EIszeroZero,
    EIszeroSucc {
        t1: Term,
    },
    EIszero {
        premise: Box<Eval1Tree>,
    },
}

impl Eval1Tree {
    pub fn premise(&self) -> Option<&Eval1Tree> {
        match self {
            Self::EIfTrue { .. } => None,
            Self::EIfFalse { .. } => None,
            Self::EIf { premise, .. } => Some(premise),
            Self::ESucc { premise } => Some(premise),
            Self::EPredZero => None,
            Self::EPredSucc { .. } => None,
            Self::EPred { premise } => Some(premise),
            Self::EIszeroZero => None,
            Self::EIszeroSucc { .. } => None,
            Self::EIszero { premise } => Some(premise),
        }
    }
    pub fn conclusion(&self) -> Eval1StepRelation {
        match self {
            Self::EIfTrue { t2, t3 } => Eval1StepRelation {
                from: Term::If {
                    cond: Term::True.into(),
                    then_clause: t2.clone().into(),
                    else_clause: t3.clone().into(),
                },
                to: t2.clone(),
            },
            Self::EIfFalse { t2, t3 } => Eval1StepRelation {
                from: Term::If {
                    cond: Term::False.into(),
                    then_clause: t2.clone().into(),
                    else_clause: t3.clone().into(),
                },
                to: t3.clone(),
            },
            Self::EIf { premise, t2, t3 } => {
                let Eval1StepRelation { from, to } = premise.conclusion();
                Eval1StepRelation {
                    from: Term::If {
                        cond: from.into(),
                        then_clause: t2.clone().into(),
                        else_clause: t3.clone().into(),
                    },
                    to: Term::If {
                        cond: to.into(),
                        then_clause: t2.clone().into(),
                        else_clause: t3.clone().into(),
                    },
                }
            }
            Self::ESucc { premise } => {
                let Eval1StepRelation { from, to } = premise.conclusion();
                Eval1StepRelation {
                    from: Term::Succ(Box::new(from)),
                    to: Term::Succ(Box::new(to)),
                }
            }
            Self::EPredZero => Eval1StepRelation {
                from: Term::Pred(Term::Zero.into()),
                to: Term::Zero,
            },
            Self::EPredSucc { nv } => Eval1StepRelation {
                from: Term::Pred(Term::Succ(nv.clone().to_term().into()).into()),
                to: nv.to_term(),
            },
            Self::EPred { premise } => {
                let Eval1StepRelation { from, to } = premise.conclusion();
                Eval1StepRelation {
                    from: Term::Pred(Box::new(from)),
                    to: Term::Pred(Box::new(to)),
                }
            }
            Self::EIszeroZero => Eval1StepRelation {
                from: Term::Iszero(Term::Zero.into()),
                to: Term::True,
            },
            Self::EIszeroSucc { t1 } => Eval1StepRelation {
                from: Term::Iszero(Term::Succ(t1.clone().into()).into()),
                to: Term::False,
            },
            Self::EIszero { premise } => {
                let Eval1StepRelation { from, to } = premise.conclusion();
                Eval1StepRelation {
                    from: Term::Iszero(Box::new(from)),
                    to: Term::Iszero(Box::new(to)),
                }
            }
        }
    }

    pub fn to_latex(&self) -> String {
        match self {
            Self::EIfTrue { t2, t3 } => {
                let t2 = t2.to_latex();
                let t3 = t3.to_latex();
                format!(r"\inferrule{{}}{{\if{{\true}}{{{t2}}}{{{t3}}} \to {t2}}}",)
            }
            Self::EIfFalse { t2, t3 } => {
                let t2 = t2.to_latex();
                let t3 = t3.to_latex();
                format!(r"\inferrule{{}}{{\if{{\false}}{{{t2}}}{{{t3}}} \to {t3}}}",)
            }
            Self::EIf { premise, t2, t3 } => {
                let premise = premise.to_latex();
                let Eval1StepRelation { from, to } = self.conclusion();
                let from = from.to_latex();
                let to = to.to_latex();
                let t2 = t2.to_latex();
                let t3 = t3.to_latex();
                format!(
                    r"\inferrule{{{premise}}}{{\if{{{from}}}{{{t2}}}{{{t3}}} \to \if{{{to}}}{{{t2}}}{{{t3}}}}}",
                )
            }
            Self::ESucc { premise } => {
                let premise = premise.to_latex();
                let Eval1StepRelation { from, to } = self.conclusion();
                let from = from.to_latex();
                let to = to.to_latex();
                format!(r"\inferrule{{{premise}}}{{\succ{{{from}}} \to \succ{{{to}}}}}")
            }
            Self::EPredZero => r"\inferrule{}{\pred{\zero} \to \zero}".to_string(),
            Self::EPredSucc { nv } => {
                let nv = nv.to_latex();
                format!(r"\inferrule{{}}{{\pred{{\succ{{{nv}}}}} \to {{{nv}}}}}")
            }
            Self::EPred { premise } => {
                let premise = premise.to_latex();
                let Eval1StepRelation { from, to } = self.conclusion();
                let from = from.to_latex();
                let to = to.to_latex();
                format!(r"\inferrule{{{premise}}}{{\pred{{{from}}} \to \pred{{{to}}}}}")
            }
            Self::EIszeroZero => r"\inferrule{}{\iszero{\zero} \to \true}".to_string(),
            Self::EIszeroSucc { t1 } => {
                let t1 = t1.to_latex();
                format!(r"\inferrule{{}}{{\iszero{{\succ{{{t1}}}}} \to \false}}")
            }
            Self::EIszero { premise } => {
                let premise = premise.to_latex();
                let Eval1StepRelation { from, to } = self.conclusion();
                let from = from.to_latex();
                let to = to.to_latex();
                format!(r"\inferrule{{{premise}}}{{\iszero{{{from}}} \to \iszero{{{to}}}}}")
            }
        }
    }
}

/// Eval 1 step
/// If it is normal form, it returns None.
pub fn eval1(t: &Term) -> Option<Eval1Tree> {
    match t {
        Term::True => None,
        Term::False => None,
        Term::If {
            cond,
            then_clause,
            else_clause,
        } => match &**cond {
            Term::True => Some(Eval1Tree::EIfTrue {
                t2: *then_clause.clone(),
                t3: *else_clause.clone(),
            }),
            Term::False => Some(Eval1Tree::EIfFalse {
                t2: *then_clause.clone(),
                t3: *else_clause.clone(),
            }),
            t => eval1(t).map(|t| Eval1Tree::EIf {
                premise: t.into(),
                t2: *then_clause.clone(),
                t3: *else_clause.clone(),
            }),
        },
        Term::Zero => None,
        Term::Succ(t) => eval1(t).map(|t| Eval1Tree::ESucc { premise: t.into() }),
        Term::Pred(t) => match &**t {
            Term::Zero => Some(Eval1Tree::EPredZero),
            Term::Succ(v) if is_number_value(v) => Some(Eval1Tree::EPredSucc {
                nv: v.to_value().unwrap(),
            }),
            t => eval1(t).map(|t| Eval1Tree::EPred { premise: t.into() }),
        },
        Term::Iszero(t) => match &**t {
            Term::Zero => Some(Eval1Tree::EIszeroZero),
            Term::Succ(v) if is_number_value(v) => Some(Eval1Tree::EIszeroSucc { t1: *v.clone() }),
            t => eval1(t).map(|t| Eval1Tree::EIszero { premise: t.into() }),
        },
    }
}
