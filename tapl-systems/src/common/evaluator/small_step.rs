use crate::common::ToLatex;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct SmallStepRelation<Stmt> {
    pub from: Stmt,
    pub to: Stmt,
}

impl<Stmt> ToLatex for SmallStepRelation<Stmt>
where
    Stmt: ToLatex,
{
    fn to_latex(&self) -> String {
        format!(
            r"{from} \longrightarrow {to}",
            from = self.from.to_latex(),
            to = self.to.to_latex(),
        )
    }
}

pub trait SmallStepTree {
    type Stmt;
    fn premises(&self) -> Vec<&Self>;
    fn conclusion(&self) -> SmallStepRelation<Self::Stmt>;
}

impl<Stmt, T> ToLatex for T
where
    T: SmallStepTree<Stmt = Stmt>,
    Stmt: ToLatex,
{
    fn to_latex(&self) -> String {
        format!(
            r"\inferrule{{{premises}}}{{{conclusion}}}",
            premises = self
                .premises()
                .iter()
                .map(|premise| premise.to_latex())
                .collect::<Vec<String>>()
                .join(r" \thickspace "),
            conclusion = self.conclusion().to_latex(),
        )
    }
}

pub trait SmallStepConstructor {
    type Stmt: Clone;
    type Tree: SmallStepTree<Stmt = Self::Stmt>;
    fn construct_tree(&self, stmt: &Self::Stmt) -> Option<Self::Tree>;
}

pub trait SmallStepEvaluator {
    type Stmt: Clone;
    fn eval1(&self, stmt: &Self::Stmt) -> Option<Self::Stmt>;

    fn is_normal_form(&self, stmt: &Self::Stmt) -> bool {
        self.eval1(stmt).is_none()
    }

    fn eval_iter<'a>(&'a self, stmt: &'a Self::Stmt) -> EvalIter<'a, Self::Stmt>
    where
        Self: Sized,
    {
        EvalIter {
            evaluator: self,
            current: Some(Cow::Borrowed(stmt)),
        }
    }
}

impl<Stmt, Eval1Tree, T> SmallStepEvaluator for T
where
    T: SmallStepConstructor<Stmt = Stmt, Tree = Eval1Tree>,
    Eval1Tree: self::SmallStepTree<Stmt = Stmt>,
    Stmt: Clone,
{
    type Stmt = Stmt;
    fn eval1(&self, stmt: &Self::Stmt) -> Option<Self::Stmt> {
        self.construct_tree(stmt).map(|t| t.conclusion().to)
    }
}

use std::borrow::Cow;
pub struct EvalIter<'a, Stmt>
where
    Stmt: Clone,
{
    evaluator: &'a dyn self::SmallStepEvaluator<Stmt = Stmt>,
    current: Option<Cow<'a, Stmt>>,
}

impl<'a, Stmt> EvalIter<'a, Stmt>
where
    Stmt: Clone,
    // Evaluator: SmallStepEvaluator<Stmt = Stmt> + ?Sized,
{
    pub fn new(evaluator: &'a dyn self::SmallStepEvaluator<Stmt = Stmt>, stmt: &'a Stmt) -> Self {
        Self {
            evaluator,
            current: Some(Cow::Borrowed(stmt)),
        }
    }
    pub fn eval_with_limit(self, limit: usize) -> Option<Stmt> {
        self.map_then_eval_with_limit(limit, |iter| iter)
    }

    pub fn map_then_eval_with_limit<F, I>(self, limit: usize, f: F) -> Option<Stmt>
    where
        F: Fn(Self) -> I,
        I: Iterator<Item = Cow<'a, Stmt>>,
    {
        let evaluator = self.evaluator;
        f(self)
            .take(limit)
            .last()
            .filter(|t| evaluator.is_normal_form(t.as_ref()))
            .map(|cow| cow.into_owned())
    }
}

impl<'a, Stmt> Iterator for EvalIter<'a, Stmt>
where
    Stmt: Clone,
{
    type Item = Cow<'a, Stmt>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.current.take() {
            None => None,
            Some(current) => {
                self.current = self
                    .evaluator
                    .eval1(current.as_ref())
                    .map(|t| Cow::Owned(t));
                Some(current)
            }
        }
    }
}
