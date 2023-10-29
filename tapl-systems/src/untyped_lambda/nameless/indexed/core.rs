use crate::common::ToLatex;
use crate::common::{ast::identifier::Identifier, evaluator::small_step::*};
use crate::untyped_lambda::{
    core::ChurchValue,
    named::ast::{Bind, NamedTerm},
    nameless::ast::NamelessTerm,
    nameless::indexed::ast::IndexedNamelessTerm,
};
use std::collections::HashMap;

impl NamedTerm {
    pub fn to_indexed_nameless_term(&self) -> IndexedNamelessTerm {
        let mut bound = Default::default();
        IndexedNamelessTerm::new(self.to_indexed_nameless_term_internal(&mut bound, 0))
    }
    fn to_indexed_nameless_term_internal(
        &self,
        bound: &mut HashMap<Identifier, usize>,
        next_level: usize,
    ) -> NamelessTerm {
        match self {
            Self::Var(v) => {
                if let Some(&level) = bound.get(v) {
                    NamelessTerm::Var(next_level - level - 1)
                } else {
                    panic!("unbound variable")
                }
            }
            Self::Abs { binds, body } => {
                let mut added = HashMap::new();
                for (i, bind) in binds.iter().enumerate() {
                    if let Bind::Identifier(id) = bind {
                        if !added.contains_key(id) {
                            added.insert(id.clone(), bound.get(id).copied());
                        }
                        bound.insert(id.clone(), next_level + i);
                    }
                }

                let mut t = body.to_indexed_nameless_term_internal(bound, next_level + binds.len());
                for _ in 0..binds.len() {
                    t = NamelessTerm::Abs(t.into());
                }

                for (bind, level) in added.iter() {
                    bound.remove(bind);
                    if let Some(level) = level {
                        bound.insert(bind.clone(), *level);
                    }
                }

                t
            }
            Self::Apply(t1, t2) => NamelessTerm::Apply(
                t1.to_indexed_nameless_term_internal(bound, next_level)
                    .into(),
                t2.to_indexed_nameless_term_internal(bound, next_level)
                    .into(),
            ),
        }
    }
}

impl From<NamedTerm> for IndexedNamelessTerm {
    fn from(t: NamedTerm) -> Self {
        t.to_indexed_nameless_term()
    }
}

impl NamelessTerm {
    pub fn to_indexed(&self) -> IndexedNamelessTerm {
        IndexedNamelessTerm::new(self.clone())
    }
}

impl IndexedNamelessTerm {
    pub fn shift(&self, d: isize, c: usize) -> Self {
        Self::new(IndexedNamelessTerm::shift_nameless_term(
            self.nameless_term(),
            d,
            c,
        ))
    }
    pub fn shift_nameless_term(term: &NamelessTerm, d: isize, c: usize) -> NamelessTerm {
        match term {
            NamelessTerm::Var(v) => {
                if *v < c {
                    NamelessTerm::Var(*v)
                } else {
                    NamelessTerm::Var((*v as isize + d) as usize)
                }
            }
            NamelessTerm::Abs(body) => {
                NamelessTerm::Abs(Self::shift_nameless_term(body, d, c + 1).into())
            }
            NamelessTerm::Apply(t1, t2) => NamelessTerm::Apply(
                Self::shift_nameless_term(t1, d, c).into(),
                Self::shift_nameless_term(t2, d, c).into(),
            ),
        }
    }

    pub fn substitute(&self, j: usize, s: &Self) -> Self {
        Self::new(IndexedNamelessTerm::substitute_nameless_term(
            self.nameless_term(),
            j,
            s,
            0,
        ))
    }
    pub fn substitute_nameless_term(
        term: &NamelessTerm,
        j: usize,
        s: &IndexedNamelessTerm,
        d: usize,
    ) -> NamelessTerm {
        match term {
            NamelessTerm::Var(v) => {
                if *v == j {
                    s.shift(d as isize, 0).into_nameless_term()
                } else {
                    NamelessTerm::Var(*v)
                }
            }
            NamelessTerm::Abs(body) => {
                NamelessTerm::Abs(Self::substitute_nameless_term(body, j + 1, s, d + 1).into())
            }
            NamelessTerm::Apply(t1, t2) => NamelessTerm::Apply(
                Self::substitute_nameless_term(t1, j, s, d).into(),
                Self::substitute_nameless_term(t2, j, s, d).into(),
            ),
        }
    }
    pub fn to_church_value(
        &self,
        inner_evaluator: &impl SmallStepEvaluator<Stmt = IndexedNamelessTerm>,
        search_limit: usize,
        depth_limit: usize,
    ) -> ChurchValue {
        Self::to_church_value_nameless_term(
            self.nameless_term(),
            inner_evaluator,
            search_limit,
            depth_limit,
        )
    }

    pub fn to_church_value_nameless_term(
        term: &NamelessTerm,
        evaluator: &impl SmallStepEvaluator<Stmt = IndexedNamelessTerm>,
        search_limit: usize,
        depth_limit: usize,
    ) -> ChurchValue {
        match term {
            NamelessTerm::Abs(body) => {
                match evaluator
                    .eval_iter(&body.to_indexed())
                    .eval_with_limit(search_limit)
                {
                    None => ChurchValue::EvalStepLimitExceeded,
                    Some(body) => match body.into_nameless_term() {
                        NamelessTerm::Abs(body) => {
                            match evaluator
                                .eval_iter(&body.to_indexed())
                                .eval_with_limit(search_limit)
                            {
                                None => ChurchValue::EvalStepLimitExceeded,
                                Some(body) => match body.into_nameless_term() {
                                    NamelessTerm::Var(1) => ChurchValue::BoolTrue,
                                    body => {
                                        let mut n = 0;
                                        let mut t = body;
                                        loop {
                                            match &t {
                                                NamelessTerm::Apply(t1, t2)
                                                    if **t1 == NamelessTerm::Var(1) =>
                                                {
                                                    t = *t2.clone();
                                                    n += 1;
                                                }
                                                NamelessTerm::Var(0) => {
                                                    return ChurchValue::Nat(n);
                                                }
                                                _ => return ChurchValue::Term,
                                            }
                                        }
                                    }
                                },
                            }
                        }
                        NamelessTerm::Apply(t1, t2) => match *t1 {
                            NamelessTerm::Apply(t1_1, t1_2) => match *t1_1 {
                                NamelessTerm::Var(0) => {
                                    if depth_limit == 0 {
                                        ChurchValue::EvalDepthLimitExceeded
                                    } else {
                                        ChurchValue::Pair(
                                            IndexedNamelessTerm::to_church_value_nameless_term(
                                                t1_2.as_ref(),
                                                evaluator,
                                                search_limit,
                                                depth_limit - 1,
                                            )
                                            .into(),
                                            IndexedNamelessTerm::to_church_value_nameless_term(
                                                t2.as_ref(),
                                                evaluator,
                                                search_limit,
                                                depth_limit - 1,
                                            )
                                            .into(),
                                        )
                                    }
                                }
                                _ => ChurchValue::Term,
                            },
                            _ => ChurchValue::Term,
                        },
                        _ => ChurchValue::Term,
                    },
                }
            }
            _ => ChurchValue::Term,
        }
    }
}

impl ToLatex for IndexedNamelessTerm {
    fn to_latex(&self) -> String {
        self.nameless_term().to_latex()
    }
}
