use super::ast::Term;
use super::core as lang_core;
use super::syntax;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct UntypedArithCalcStep {
    pub step: u64,
    pub is_normal_form: bool,
    pub size: usize,
    pub depth: usize,

    formatted_latex: String,
    formatted_code: String,
    formatted_latex_as_value: Option<String>,
}

#[wasm_bindgen]
impl UntypedArithCalcStep {
    pub fn formatted_latex(&self) -> String {
        self.formatted_latex.clone()
    }
    pub fn formatted_code(&self) -> String {
        self.formatted_code.clone()
    }
    pub fn formatted_latex_as_value(&self) -> Option<String> {
        self.formatted_latex_as_value.clone()
    }
}

// pub enum Eval1Tree {
//     EIfTrue {
//         t2: Term,
//         t3: Term,
//     },
//     EIfFalse {
//         t2: Term,
//         t3: Term,
//     },
//     EIf {
//         premise: Box<Eval1Tree>,
//         t2: Term,
//         t3: Term,
//     },
//     ESucc {
//         premise: Box<Eval1Tree>,
//     },
//     EPredZero,
//     EPredSucc {
//         nv: Value,
//     },
//     EPred {
//         premise: Box<Eval1Tree>,
//     },
//     EIszeroZero,
//     EIszeroSucc {
//         t1: Term,
//     },
//     EIszero {
//         premise: Box<Eval1Tree>,
//     },
// }

impl lang_core::Eval1Tree {
    pub const fn to_index(&self) -> usize {
        match self {
            Self::EIfTrue { .. } => 0,
            Self::EIfFalse { .. } => 1,
            Self::EIf { .. } => 2,
            Self::ESucc { .. } => 3,
            Self::EPredZero { .. } => 4,
            Self::EPredSucc { .. } => 5,
            Self::EPred { .. } => 6,
            Self::EIszeroZero { .. } => 7,
            Self::EIszeroSucc { .. } => 8,
            Self::EIszero { .. } => 9,
        }
    }
    pub fn to_index_list(&self) -> Vec<usize> {
        let mut list = vec![self.to_index()];
        let mut v = self;
        while let Some(p) = v.premise() {
            list.push(p.to_index());
            v = p;
        }
        list
    }
}

impl UntypedArithCalcStep {
    pub fn term_as_step(t: &Term, step: u64) -> UntypedArithCalcStep {
        let next = lang_core::eval1(t);
        UntypedArithCalcStep {
            step,
            is_normal_form: next.is_none(),
            size: lang_core::size(t),
            depth: lang_core::depth(t),
            formatted_latex: t.to_latex(),
            formatted_code: t.to_code("  ", "\n"),
            formatted_latex_as_value: lang_core::value_of(t).map(|v| v.to_latex()),
        }
    }
}

#[wasm_bindgen]
#[derive(Clone)]
pub struct UntypedArithCalc {
    term: Option<Term>,
    eval1_tree: Option<lang_core::Eval1Tree>,
}

#[wasm_bindgen]
impl UntypedArithCalc {
    pub fn parse(str: String) -> UntypedArithCalc {
        let term = syntax::TermParser::new().parse(&str);
        let (term, _err) = match term {
            Ok(term) => (Some(term), None),
            Err(err) => (None, Some(err)),
        };
        Self {
            term: term.clone(),
            eval1_tree: term.and_then(|term| lang_core::eval1(&term)),
        }
    }

    // pub fn step(&self) -> Option<UntypedArithCalcStep> {
    //     let step = self
    //         .term
    //         .as_ref()
    //         .map(|term| UntypedArithCalcStep::term_as_step(term, 0));
    //     step
    // }

    pub fn formatted_latex(&self) -> String {
        self.term
            .as_ref()
            .map(|term| term.to_latex())
            .unwrap_or_default()
    }

    pub fn formatted_code(&self) -> String {
        self.term
            .as_ref()
            .map(|term| term.to_code("  ", "\n"))
            .unwrap_or_default()
    }

    pub fn is_parse_ok(&self) -> bool {
        self.term.is_some()
    }

    pub fn next(&self) -> Self {
        let term = self
            .eval1_tree
            .as_ref()
            .map(|t| t.conclusion().to().clone());
        let eval1_tree = term.as_ref().and_then(lang_core::eval1);
        Self { term, eval1_tree }
    }

    pub fn is_normal_form(&self) -> bool {
        self.eval1_tree.is_none()
    }

    pub fn wasm_clone(&self) -> Self {
        Clone::clone(self)
    }
}
