pub use crate::common::ast::identifier::{Identifier, SpecialIdentifier};
use crate::common::ast::variable::{LocalVariable, Variable, VariableRef};
use crate::common::context::{ContextStackPopper, VariableLevelContext, VariableLevelContextStack};
use crate::common::diagnostic::{merge_diags, Diagnostic, DiagnosticLevel, Span, Spanned};
use crate::format::{FormatContext, ToCode};
use num_bigint::BigUint;
use std::cell::{Ref, RefCell};
use std::collections::{HashMap, HashSet};
use std::fmt::{self, Display, Formatter};

// TODO bindのカウント変更が必要
macro_rules! inc {
    ($cs:expr, $term_bind:ident, $number:expr) => {
        let $term_bind = $term_bind.map(|e| e.number_term_var($cs, $number));
        let $term_bind = if let Pattern::Named(lvar) = $term_bind.value.pat.value {
            *$number += 1;
            Spanned::new(
                $term_bind.span,
                TermBind {
                    pat: Spanned::new(
                        $term_bind.value.pat.span,
                        Pattern::Named(lvar.map(|e| e.map_discriminator(|_| *$number))),
                    ),
                    typ: $term_bind.value.typ,
                },
            )
        } else {
            $term_bind
        };
        let _scope = $term_bind
            .spanned_local_variable()
            .map(|lvar| $cs.push(Spanned::new(lvar.span, lvar.id().clone()), *$number));
    };
}

pub trait GrammaticalSize {
    fn grammatical_size(&self) -> usize;
}

fn to_code_paren(term: &(impl ToCode + GrammaticalSize), fmt_ctx: &FormatContext) -> String {
    if let FormatContext::Pretty { .. } = fmt_ctx {
        let size = term.grammatical_size();
        if size < 10 {
            let c = term.to_code(&FormatContext::new_compact());
            format!("({})", c)
        } else {
            let c = term.to_code(&fmt_ctx.indented());
            let current_indent = fmt_ctx.current_indent();
            let next_indent = fmt_ctx.indented();
            let next_indent = next_indent.current_indent();
            let new_line = fmt_ctx.new_line();
            format!("({new_line}{next_indent}{c}{new_line}{current_indent})")
        }
    } else {
        format!("({})", term.to_code(fmt_ctx))
    }
}
fn to_code_lambda_like(
    head: &str,
    bind: &impl ToCode,
    body: &impl ToCode,
    fmt_ctx: &FormatContext,
) -> String {
    let bind = bind.to_code(fmt_ctx);
    let body = body.to_code(fmt_ctx);
    let new_line = fmt_ctx.new_line();
    let current_indent = fmt_ctx.current_indent();
    format!(r"{head}{bind}.{new_line}{current_indent}{body}")
}

// struct Traverser<
//     C,
//     FnSource: FnMut(&mut C, Source) -> Source,
//     FnDef: FnMut(&mut C, Def) -> Def,
//     FnTerm: FnMut(&mut C, Term) -> Term,
//     FnType: FnMut(&mut C, Type) -> Type,
//     FnInductionConstructor: FnMut(&mut C, InductionConstructor) -> InductionConstructor,
// > {
//     source: FnSource,
//     def: FnDef,
//     term: FnTerm,
//     typ: FnType,
//     induction_constructor: FnInductionConstructor,
// }
// pub trait Traverse {
//     // fn traverse<
//     //     C,
//     //     FnSource: FnMut(&mut C, Source) -> Source,
//     //     FnDef: FnMut(&mut C, Def) -> Def,
//     //     FnTerm: FnMut(&mut C, Term) -> Term,
//     //     FnType: FnMut(&mut C, Type) -> Type,
//     //     FnInductionConstructor: FnMut(&mut C, InductionConstructor) -> InductionConstructor,
//     // >(
//     //     self,
//     //     context: &mut C,
//     //     traverser: Traverser<C, FnSource, FnDef, FnTerm, FnType, FnInductionConstructor>,
//     // ) -> Self;
//     // fn traverse<C>(
//     //     self,
//     //     context: &mut C,
//     //     f: &mut impl FnMut(&mut C, Traversed) -> Option<Traversed>,
//     // ) -> Self;
//     // fn traverse<C, F: >(
//     //     self,
//     //     context: &mut C,
//     //     f: &mut impl FnMut(&mut C, Traversed) -> Traversed,
//     //     push_term_var: &mut impl FnMut(&mut C, Identifier, Type) -> LocalVariable,
//     // ) -> Self;
// }
pub trait NumberTermVar {
    fn number_term_var(
        self,
        cs: &VariableLevelContextStack<Identifier>,
        number: &mut usize,
    ) -> Self;
}
// pub fn f() {
//     let ctx = 0;
//     let a = Traverser {
//         source: |_, s| s,
//         def: |_, d| d,
//         term: |_, t| t,
//         typ: |_, t| t,
//         induction_constructor: |_, i| i,
//     };
// }
// pub trait Traverse<C> {
//     fn traverse(
//         self,
//         context: &mut C,
//         f_source: &mut impl FnMut(&mut C, Source) -> Source,
//         f_def: &mut impl FnMut(&mut C, Def) -> Def,
//         f_term: &mut impl FnMut(&mut C, Term) -> Term,
//         f_type: &mut impl FnMut(&mut C, Type) -> Type,
//         f_induction_constructor: &mut impl FnMut(&mut C, InductionConstructor) -> InductionConstructor,
//     ) -> Self;
// }

#[derive(Clone, Debug)]
pub struct SpannedVars<'a> {
    bound_size: usize,
    map: HashMap<&'a Identifier, Vec<Span>>,
}
impl<'a> SpannedVars<'a> {
    pub fn new() -> Self {
        Self {
            bound_size: 0,
            map: HashMap::new(),
        }
    }
    pub fn insert<'b: 'a>(&mut self, name: Spanned<&'b Identifier>) {
        let name_span = name.span;
        let entry = self.map.entry(name.value());
        let entry = entry.or_insert_with(Vec::new);
        if let Some(span) = name_span {
            self.bound_size += 1;
            entry.push(span);
        }
    }
    pub fn get(&self, name: &Identifier) -> Option<&Vec<Span>> {
        self.map.get(name)
    }
    pub fn remove(&mut self, name: &Identifier) -> Option<Vec<Span>> {
        self.bound_size -= self.map.get(name).map_or(0, |v| v.len());
        self.map.remove(name)
    }
    pub fn size(&self) -> usize {
        self.bound_size + self.map.len()
    }
    pub fn len(&self) -> usize {
        self.map.len()
    }
    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }
    pub fn extend(mut self, mut other: SpannedVars<'a>) -> Self {
        if self.size() < other.size() {
            std::mem::swap(&mut self, &mut other);
        }
        for (name, spans) in other.map {
            let entry = self.map.entry(name).or_insert_with(Vec::new);
            self.bound_size += spans.len();
            entry.extend(spans);
        }
        self
    }
}
impl Default for SpannedVars<'_> {
    fn default() -> Self {
        Self::new()
    }
}

pub trait SubstitutableWithTerm {
    fn substitute_term(self, from: VariableRef, to_term: &Spanned<Term>) -> Self;
}
pub trait SubstitutableWithType {
    fn substitute_type(self, from: &Identifier, to_typ: &Spanned<Type>) -> Self;
}

#[derive(Clone, Debug)]
pub enum ContextDef {
    TermDef {
        name: Spanned<Identifier>,
        term: Spanned<Term>,
    },
    TypeDef {
        name: Spanned<Identifier>,
        typ: Spanned<Type>,
    },
    AxiomDef {
        name: Spanned<Identifier>,
        typ: Spanned<Type>,
    },
    InductionDef {
        name: Spanned<Identifier>,
        constructors: Vec<Spanned<InductionConstructor>>,
    },
}
impl SubstitutableWithTerm for ContextDef {
    fn substitute_term(self, from: VariableRef, to_term: &Spanned<Term>) -> Self {
        match self {
            ContextDef::TermDef { name, term } => ContextDef::TermDef {
                name,
                term: term.map(|term| term.substitute_term(from, to_term)),
            },
            ContextDef::TypeDef { name, typ } => ContextDef::TypeDef {
                name,
                typ: typ.map(|typ| typ.substitute_term(from, to_term)),
            },
            ContextDef::AxiomDef { name, typ } => ContextDef::AxiomDef {
                name,
                typ: typ.map(|typ| typ.substitute_term(from, to_term)),
            },
            ContextDef::InductionDef { .. } => unimplemented!(),
        }
    }
}
impl SubstitutableWithType for ContextDef {
    fn substitute_type(self, from: &Identifier, to_typ: &Spanned<Type>) -> Self {
        match self {
            ContextDef::TermDef { name, term } => ContextDef::TermDef {
                name,
                term: term.map(|term| term.substitute_type(from, to_typ)),
            },
            ContextDef::TypeDef { name, typ } => ContextDef::TypeDef {
                name,
                typ: typ.map(|typ| typ.substitute_type(from, to_typ)),
            },
            ContextDef::AxiomDef { name, typ } => ContextDef::AxiomDef {
                name,
                typ: typ.map(|typ| typ.substitute_type(from, to_typ)),
            },
            ContextDef::InductionDef { .. } => unimplemented!(),
        }
    }
}
impl ToCode for ContextDef {
    fn to_code(&self, fmt_ctx: &FormatContext) -> String {
        match self {
            ContextDef::TermDef { name, term } => {
                let name = name.to_code(fmt_ctx);
                let term = to_code_paren(term.value(), fmt_ctx);
                format!("def {name} = {term};")
            }
            ContextDef::TypeDef { name, typ } => {
                let name = name.to_code(fmt_ctx);
                let typ = to_code_paren(typ.value(), fmt_ctx);
                format!("type {name} = {typ};")
            }
            ContextDef::AxiomDef { name, typ } => {
                let name = name.to_code(fmt_ctx);
                let typ = to_code_paren(typ.value(), fmt_ctx);
                format!("axiom {name}: {typ};")
            }
            ContextDef::InductionDef { .. } => unimplemented!(),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct ContextOrdered {
    pub defs: Vec<ContextDef>,
}
impl ContextOrdered {
    pub fn push(&mut self, def: ContextDef) {
        self.defs.push(def);
    }
    // MEMO: 最初に全部捜査して lvar(id, n) は qid(head(id)) になっているので問題ない
    pub fn substitute_of<T>(&self, mut t: T) -> T
    where
        T: SubstitutableWithTerm + SubstitutableWithType,
    {
        for def in self.defs.iter().rev() {
            match def {
                ContextDef::TermDef { name, term } => {
                    t = t.substitute_term(
                        VariableRef::new_qualified_head_normal(name.as_ref()),
                        term,
                    );
                }
                ContextDef::TypeDef { name, typ } => {
                    t = t.substitute_type(&name.value, typ);
                }
                ContextDef::AxiomDef { .. } => {}
                ContextDef::InductionDef { .. } => unimplemented!(),
            }
        }
        t
    }
}

type TermVars = Vec<(Identifier, (Option<Span>, Spanned<Type>))>;
#[derive(Clone, Debug, Default)]
pub struct ContextBuilder {
    pub term_vars: TermVars,
    pub defs: Vec<ContextDef>,
}
impl ContextBuilder {
    pub fn push_term_var(&mut self, name: Spanned<Identifier>, typ: Spanned<Type>) {
        self.term_vars.push((name.value, (name.span, typ)));
    }
    pub fn push_def(&mut self, def: ContextDef) {
        self.defs.push(def);
    }
    pub fn into_context(self) -> Context {
        // let term_vars: TermVarDefHashMap = ;
        Context {
            global_term_vars: RefCell::new(
                self.term_vars
                    .into_iter()
                    .map(|(id, (span, typ))| (id, (span, typ, None)))
                    .collect(),
            ),
            term_vars: Default::default(),
            type_vars: Default::default(),
            defs: RefCell::new(self.defs.into_iter().collect()),
        }
    }
}
impl SubstitutableWithTerm for ContextBuilder {
    fn substitute_term(mut self, from: VariableRef, to_term: &Spanned<Term>) -> Self {
        self.term_vars = self
            .term_vars
            .into_iter()
            .map(|(name, (span, typ))| {
                (
                    name,
                    (span, typ.map(|typ| typ.substitute_term(from, to_term))),
                )
            })
            .collect();
        self.defs = self
            .defs
            .into_iter()
            .map(|def| def.substitute_term(from, to_term))
            .collect();
        self
    }
}
impl SubstitutableWithType for ContextBuilder {
    fn substitute_type(mut self, from: &Identifier, to_typ: &Spanned<Type>) -> Self {
        self.term_vars = self
            .term_vars
            .into_iter()
            .map(|(name, (span, typ))| {
                (
                    name,
                    (span, typ.map(|typ| typ.substitute_type(from, to_typ))),
                )
            })
            .collect();
        self.defs = self
            .defs
            .into_iter()
            .map(|def| def.substitute_type(from, to_typ))
            .collect();
        self
    }
}

pub type GlobalTermVarDefMap =
    HashMap<Identifier, (Option<Span>, Spanned<Type>, Option<Spanned<Term>>)>;
pub type TermVarDefMap =
    HashMap<LocalVariable, (Option<Span>, Spanned<Type>, Option<Spanned<Term>>)>;
pub type TypeVarDefMap = HashMap<Identifier, (Option<Span>, Spanned<Kind>, Option<Spanned<Type>>)>;
pub type GlobalTermDefMap = HashMap<Identifier, (Option<Span>, Spanned<Term>)>;
pub type TypeDefMap = HashMap<Identifier, (Option<Span>, Spanned<Type>)>;
#[derive(Clone, Debug, Default)]
pub struct Context {
    pub global_term_vars: RefCell<GlobalTermVarDefMap>,
    pub term_vars: RefCell<TermVarDefMap>,
    pub type_vars: RefCell<TypeVarDefMap>,
    // initするまでの間一時的に持つ
    pub defs: RefCell<Vec<ContextDef>>,
}
impl Context {
    pub fn global_term_vars(&self) -> Ref<GlobalTermVarDefMap> {
        self.global_term_vars.borrow()
    }
    pub fn term_vars(&self) -> Ref<TermVarDefMap> {
        self.term_vars.borrow()
    }
    pub fn type_vars(&self) -> Ref<TypeVarDefMap> {
        self.type_vars.borrow()
    }
    pub fn defs(&self) -> Ref<Vec<ContextDef>> {
        self.defs.borrow()
    }
    pub fn make_stack<'a: 'b, 'b: 'a>(&'a mut self) -> ContextStack<'b> {
        ContextStack {
            term_var_stack: RefCell::new(Vec::new()),
            type_var_stack: RefCell::new(Vec::new()),
            original: self,
        }
    }
    pub fn term_def_map(&self) -> GlobalTermDefMap {
        self.defs.clone().into_inner().into_iter().fold(
            HashMap::new(),
            |mut map: GlobalTermDefMap, def| match def {
                ContextDef::TermDef { name, term } => {
                    map.insert(name.value, (name.span, term));
                    map
                }
                ContextDef::TypeDef { .. } => map,
                ContextDef::AxiomDef { .. } => map,
                ContextDef::InductionDef { .. } => unimplemented!(),
            },
        )
    }
    pub fn type_def_map(&self) -> TypeDefMap {
        self.defs.clone().into_inner().into_iter().fold(
            HashMap::new(),
            |mut map: TypeDefMap, def| match def {
                ContextDef::TermDef { .. } => map,
                ContextDef::TypeDef { name, typ } => {
                    map.insert(name.value, (name.span, typ));
                    map
                }
                ContextDef::AxiomDef { .. } => map,
                ContextDef::InductionDef { .. } => unimplemented!(),
            },
        )
    }
}
impl ToCode for Context {
    fn to_code(&self, fmt_ctx: &FormatContext) -> String {
        self.defs
            .borrow()
            .iter()
            .map(|def| def.to_code(fmt_ctx))
            .collect::<Vec<_>>()
            .join({
                let new_line = fmt_ctx.new_line();
                let current_indent = fmt_ctx.current_indent();
                format!("{new_line}{current_indent}").as_str()
            })
    }
}

type TermVarDefStack = Vec<(
    LocalVariable,
    Option<(Option<Span>, Spanned<Type>, Option<Spanned<Term>>)>,
)>;
type TypeVarDefStack = Vec<(
    Identifier,
    Option<(Option<Span>, Spanned<Kind>, Option<Spanned<Type>>)>,
)>;
pub type TermVarDefHashMap<'a> =
    HashMap<LocalVariable, (Option<Span>, Spanned<Type>, Option<Spanned<Term>>)>;
pub type TermVarDefRef<'a> = Ref<'a, (Option<Span>, Spanned<Type>, Option<Spanned<Term>>)>;
pub type TypeVarDefRef<'a> = Ref<'a, (Option<Span>, Spanned<Kind>, Option<Spanned<Type>>)>;
#[derive(Debug)]
pub struct ContextStack<'a> {
    term_var_stack: RefCell<TermVarDefStack>,
    type_var_stack: RefCell<TypeVarDefStack>,
    original: &'a mut Context,
}
impl<'a> ContextStack<'a> {
    pub fn push_term_var<'b: 'a>(
        &'b self,
        name: Spanned<LocalVariable>,
        typ: Spanned<Type>,
    ) -> ContextStackPopper<'a, Self, fn(&ContextStack<'a>)> {
        let saved = self
            .original
            .term_vars
            .borrow_mut()
            .insert(name.value.clone(), (name.span, typ, None));
        self.term_var_stack.borrow_mut().push((name.value, saved));
        ContextStackPopper::new(self, |stack| stack.pop_term_var())
    }
    fn pop_term_var(&self) {
        let (name, saved) = self.term_var_stack.borrow_mut().pop().unwrap();
        if let Some(saved) = saved {
            self.original.term_vars.borrow_mut().insert(name, saved);
        } else {
            self.original.term_vars.borrow_mut().remove(&name);
        }
    }
    pub fn term_vars(&self) -> Ref<TermVarDefMap> {
        self.original.term_vars()
    }
    pub fn push_type_var(
        &self,
        name: Spanned<Identifier>,
        kind: Spanned<Kind>,
    ) -> ContextStackPopper<'_, Self, fn(&ContextStack<'_>)> {
        let saved = self
            .original
            .type_vars
            .borrow_mut()
            .insert(name.value.clone(), (name.span, kind, None));
        self.type_var_stack.borrow_mut().push((name.value, saved));
        ContextStackPopper::new(self, |stack| stack.pop_type_var())
    }
    fn pop_type_var(&self) {
        let (name, saved) = self.type_var_stack.borrow_mut().pop().unwrap();
        if let Some(saved) = saved {
            self.original.type_vars.borrow_mut().insert(name, saved);
        } else {
            self.original.type_vars.borrow_mut().remove(&name);
        }
    }
    // pub fn get_term(&self, qid: &QualifiedIdentifier) -> Option<TermVarDefRef> {
    //     match qid {
    //         QualifiedIdentifier::Head(id) => {
    //             Ref::filter_map(self.original.term_vars.borrow(), |e| e.get(id)).ok()
    //         }
    //         _ => None,
    //     }
    // }
    pub fn get_term(&self, id: &LocalVariable) -> Option<TermVarDefRef> {
        Ref::filter_map(self.original.term_vars.borrow(), |e| e.get(id)).ok()
    }
    pub fn get_type(&self, id: &Identifier) -> Option<TypeVarDefRef> {
        Ref::filter_map(self.original.type_vars.borrow(), |e| e.get(id)).ok()
    }
    pub fn get_global_term(&self, id: &Identifier) -> Option<TermVarDefRef> {
        Ref::filter_map(self.original.global_term_vars.borrow(), |e| e.get(id)).ok()
    }
    pub fn type_vars(&self) -> Ref<TypeVarDefMap> {
        self.original.type_vars()
    }

    pub fn init<'b: 'a, Typing, Kinding>(
        &'b self,
        typing: Typing,
        kinding: Kinding,
    ) -> (
        ContextStackPopper<'a, Self, impl Fn(&ContextStack<'a>)>,
        Vec<Diagnostic>,
    )
    where
        Typing: Fn(&ContextStack<'a>, &Spanned<Term>) -> Result<Spanned<Type>, Vec<Diagnostic>>,
        Kinding: Fn(&ContextStack<'a>, &Spanned<Type>) -> Result<Spanned<Kind>, Vec<Diagnostic>>,
    {
        let mut diags = Vec::new();

        let mut type_var_push_cnt = 0;
        self.original.defs().iter().for_each(|def| match def {
            ContextDef::TermDef { name, term } => {
                let typ = typing(self, term);
                typ.map_or_else(
                    |e| merge_diags(&mut diags, e),
                    |typ| {
                        // NOTE: Global variables are managed separately, so unecessary to save
                        self.original
                            .global_term_vars
                            .borrow_mut()
                            .insert(name.value.clone(), (name.span, typ, Some(term.clone())));
                    },
                )
            }
            ContextDef::TypeDef { name, typ } => {
                let kind = kinding(self, typ);
                kind.map_or_else(
                    |e| merge_diags(&mut diags, e),
                    |kind| {
                        let saved = self
                            .original
                            .type_vars
                            .borrow_mut()
                            .insert(name.value.clone(), (name.span, kind, Some(typ.clone())));
                        self.type_var_stack
                            .borrow_mut()
                            .push((name.value.clone(), saved));
                        type_var_push_cnt += 1;
                    },
                )
            }
            ContextDef::AxiomDef { name, typ } => {
                let kind = kinding(self, typ);
                kind.map_or_else(
                    |e| merge_diags(&mut diags, e),
                    |_| {
                        self.original
                            .global_term_vars
                            .borrow_mut()
                            .insert(name.value.clone(), (name.span, typ.clone(), None));
                    },
                )
            }
            ContextDef::InductionDef { .. } => unimplemented!(),
        });
        (
            ContextStackPopper::new(self, move |stack| {
                // for _ in 0..term_var_push_cnt {
                //     stack.pop_term_var();
                // }
                for _ in 0..type_var_push_cnt {
                    stack.pop_type_var();
                }
            }),
            // ContextStackPopper {
            //     stack: self,
            //     f: move |stack| {
            //         for _ in 0..term_var_push_cnt {
            //             stack.pop_term_var();
            //         }
            //         for _ in 0..type_var_push_cnt {
            //             stack.pop_type_var();
            //         }
            //     },
            // },
            diags,
        )
    }
    pub fn fork(&self) -> Context {
        self.original.clone()
    }
}
impl Drop for ContextStack<'_> {
    fn drop(&mut self) {
        if self.term_var_stack.borrow().len() != 0 {
            // TODO
            // panic!("ContextStack term definitions are not fully poped");
        }
        if self.type_var_stack.borrow().len() != 0 {
            // TODO
            // panic!("ContextStack type definitions are not fully poped");
        }
    }
}

#[derive(Clone, Debug)]
pub struct Source {
    pub defs: Vec<Spanned<Def>>,
    pub term: Spanned<Term>,
}
impl Source {
    pub fn spanned_defs(&self) -> &Vec<Spanned<Def>> {
        &self.defs
    }
    pub fn spanned_term(&self) -> &Spanned<Term> {
        &self.term
    }
    pub fn check_closure(&self) -> Result<(), Vec<Diagnostic>> {
        let mut bound_term = HashSet::new();
        let mut bound_type = HashSet::new();
        for def in self.defs.iter() {
            match def.value() {
                Def::TermDef { name, .. } => {
                    if bound_term.contains(&name.value) {
                        return Err(vec![Diagnostic::new(
                            DiagnosticLevel::Error,
                            format!("{} is already bound as term", name.to_string()),
                            name.span,
                        )]);
                    }
                    // for (name, spans) in term.free_term_vars().map {
                    //     if !bound_term.contains(name) {
                    //         return Err(spans
                    //             .into_iter()
                    //             .map(|span| {
                    //                 Diagnostic::new(
                    //                     DiagnosticLevel::Error,
                    //                     format!("{} is not bound", name.to_string()),
                    //                     Some(*span),
                    //                 )
                    //             })
                    //             .collect());
                    //     }
                    // }
                    // TODO: check free type vars
                    bound_term.insert(&name.value);
                }
                Def::TypeDef { name, .. } => {
                    if bound_type.contains(&name.value) {
                        return Err(vec![Diagnostic::new(
                            DiagnosticLevel::Error,
                            format!("{} is already bound as type", name.to_string()),
                            name.span,
                        )]);
                    }
                    // TODO: check free term vars
                    // TODO: check free type vars
                    bound_type.insert(&name.value);
                }
                Def::AxiomDef { name, .. } => {
                    if bound_term.contains(&name.value) {
                        return Err(vec![Diagnostic::new(
                            DiagnosticLevel::Error,
                            format!("{} is already bound as term", name.to_string()),
                            name.span,
                        )]);
                    }
                    bound_term.insert(&name.value);
                }
                Def::InductionDef { name, .. } => {
                    return Err(vec![Diagnostic::new(
                        DiagnosticLevel::Error,
                        "Induction is not supported yet".to_string(),
                        name.span,
                    )]);
                }
            }
        }
        Ok(())
    }
    // pub fn into_term(self) -> (Term, Vec<(Spanned<Identifier>, Spanned<Type>)>) {
    pub fn into_term(self) -> (Spanned<Term>, Context) {
        let mut global_ctx = ContextBuilder::default();
        let mut global_ctx_inline = ContextOrdered::default();
        let mut global_term_var_names = HashMap::<Identifier, Option<Span>>::default();
        for def in self.defs.into_iter() {
            match def.value {
                Def::TermDef { name, term, inline } => {
                    if inline {
                        global_ctx_inline.push(ContextDef::TermDef { name, term });
                    } else {
                        global_term_var_names.insert(name.value().clone(), name.span);
                        let term = term.map(|term| global_ctx_inline.substitute_of(term));
                        global_ctx.push_def(ContextDef::TermDef { name, term });
                    }
                }
                Def::TypeDef { name, typ, inline } => {
                    if inline {
                        global_ctx_inline.push(ContextDef::TypeDef { name, typ });
                    } else {
                        let typ = typ.map(|typ| global_ctx_inline.substitute_of(typ));
                        global_ctx.push_def(ContextDef::TypeDef { name, typ });
                    }
                }
                Def::AxiomDef { name, typ } => {
                    global_term_var_names.insert(name.value().clone(), name.span);
                    let typ = typ.map(|typ| global_ctx_inline.substitute_of(typ));
                    // global_ctx.push_term_var(name, typ);
                    global_ctx.push_def(ContextDef::AxiomDef { name, typ });
                }
                Def::InductionDef { .. } => {
                    unimplemented!()
                    // TODO
                }
            }
        }
        let term = self.term.map(|term| global_ctx_inline.substitute_of(term));
        // for (name, span) in global_term_var_names {
        //     let this_scoped_var = Spanned::new(
        //         span,
        //         Term::Var(Spanned::new(
        //             span,
        //             Variable::new_qualified(QualifiedIdentifier::Sub(
        //                 Spanned::new(
        //                     span,
        //                     QualifiedIdentifier::Head(Spanned::new(
        //                         span,
        //                         QualifiedIdentifierPart::Special(SpecialIdentifier::new(
        //                             "this".to_string(),
        //                         )),
        //                     )),
        //                 )
        //                 .into(),
        //                 Spanned::new(span, QualifiedIdentifierPart::Normal(name.clone())),
        //             )),
        //         )),
        //     );
        //     // let from = LocalVariable::new_any(name);
        //     // term = term.map(|term| term.substitute_term(&from, &this_scoped_var));
        //     // global_ctx = global_ctx.substitute_term(&from, &this_scoped_var);
        // }
        (term, global_ctx.into_context())
    }
}
impl ToCode for Source {
    fn to_code(&self, fmt_ctx: &FormatContext) -> String {
        let mut code = String::new();
        for def in self.defs.iter() {
            code.push_str(&def.to_code(fmt_ctx));
            code.push('\n');
        }
        code.push_str(&self.term.to_code(fmt_ctx));
        code
    }
}
impl NumberTermVar for Source {
    fn number_term_var(
        self,
        cs: &VariableLevelContextStack<Identifier>,
        number: &mut usize,
    ) -> Self {
        Self {
            defs: self
                .defs
                .into_iter()
                .map(|def| def.map(|def| def.number_term_var(cs, number)))
                .collect(),
            term: self.term.map(|term| term.number_term_var(cs, number)),
        }
    }
}

#[derive(Clone, Debug)]
pub struct InductionConstructor {
    pub name: Spanned<Identifier>,
    pub typ: Spanned<Type>,
}
impl InductionConstructor {
    pub fn name(&self) -> &Spanned<Identifier> {
        &self.name
    }
    pub fn typ(&self) -> &Spanned<Type> {
        &self.typ
    }

    // pub fn free_term_vars(&self) -> SpannedVars {
    //     self.typ.free_term_vars()
    // }
    pub fn free_type_vars(&self) -> SpannedVars {
        self.typ.free_type_vars()
    }
}
impl ToCode for InductionConstructor {
    fn to_code(&self, fmt_ctx: &FormatContext) -> String {
        format!("{}: {}", self.name.to_string(), self.typ.to_code(fmt_ctx))
    }
}
impl NumberTermVar for InductionConstructor {
    fn number_term_var(
        self,
        cs: &VariableLevelContextStack<Identifier>,
        number: &mut usize,
    ) -> Self {
        Self {
            name: self.name,
            typ: self.typ.map(|typ| typ.number_term_var(cs, number)),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Def {
    TermDef {
        name: Spanned<Identifier>,
        term: Spanned<Term>,
        inline: bool,
    },
    TypeDef {
        name: Spanned<Identifier>,
        typ: Spanned<Type>,
        inline: bool,
    },
    AxiomDef {
        name: Spanned<Identifier>,
        typ: Spanned<Type>,
    },
    InductionDef {
        name: Spanned<Identifier>,
        constructors: Vec<Spanned<InductionConstructor>>,
    },
}
impl Def {
    pub fn spanned_name(&self) -> &Spanned<Identifier> {
        match self {
            Self::TermDef { name, .. }
            | Self::TypeDef { name, .. }
            | Self::AxiomDef { name, .. }
            | Self::InductionDef { name, .. } => name,
        }
    }
    pub fn name(&self) -> &Identifier {
        self.spanned_name().value()
    }
    pub fn subterms_term(&self) -> Vec<&Spanned<Term>> {
        match self {
            Self::TermDef { term, .. } => vec![term],
            Self::TypeDef { .. } | Self::AxiomDef { .. } => vec![],
            Self::InductionDef { .. } => vec![],
        }
    }
    pub fn subterms_type(&self) -> Vec<&Spanned<Type>> {
        match self {
            Self::TermDef { .. } => vec![],
            Self::TypeDef { typ, .. } | Self::AxiomDef { typ, .. } => vec![typ],
            Self::InductionDef { constructors, .. } => constructors
                .iter()
                .flat_map(|constructor| constructor.typ.subterms_type())
                .collect(),
        }
    }
    // pub fn free_term_vars(&self) -> SpannedVars {
    //     match self {
    //         Self::TermDef { term, .. } => term.free_term_vars(),
    //         Self::TypeDef { typ, .. } => typ.free_term_vars(),
    //         Self::AxiomDef { typ, .. } => typ.free_term_vars(),
    //         Self::InductionDef { constructors, .. } => {
    //             let mut free_term_vars = SpannedVars::new();
    //             for constructor in constructors.iter() {
    //                 free_term_vars = free_term_vars.extend(constructor.free_term_vars());
    //             }
    //             free_term_vars.remove(self.name());
    //             free_term_vars
    //         }
    //     }
    // }
    pub fn free_type_vars(&self) -> SpannedVars {
        match self {
            Self::TermDef { term, .. } => term.free_type_vars(),
            Self::TypeDef { typ, .. } => typ.free_type_vars(),
            Self::AxiomDef { typ, .. } => typ.free_type_vars(),
            Self::InductionDef { constructors, .. } => {
                let mut free_type_vars = SpannedVars::new();
                for constructor in constructors.iter() {
                    free_type_vars = free_type_vars.extend(constructor.free_type_vars());
                }
                free_type_vars.remove(self.name());
                free_type_vars
            }
        }
    }
}
impl ToCode for Def {
    fn to_code(&self, fmt_ctx: &FormatContext) -> String {
        match self {
            Self::TermDef { name, term, inline } => {
                let name = name.as_str();
                let term = term.to_code(fmt_ctx);
                let inline = if *inline { "!" } else { "" };
                format!("def{inline} {name} = {term};")
            }
            Self::TypeDef { name, typ, inline } => {
                let name = name.as_str();
                let typ = typ.to_code(fmt_ctx);
                let inline = if *inline { "!" } else { "" };
                format!("type{inline} {name} = {typ};")
            }
            Self::AxiomDef { name, typ, .. } => {
                let name = name.as_str();
                let typ = typ.to_code(fmt_ctx);
                format!("axiom {name} = {typ};")
            }
            Self::InductionDef {
                name, constructors, ..
            } => format!(
                "ind {} {{\n{}}}",
                name.as_str(),
                constructors
                    .iter()
                    .map(|constructor| constructor.to_code(fmt_ctx))
                    .map(|code| format!("  {},\n", code))
                    .collect::<Vec<String>>()
                    .join("")
            ),
        }
    }
}
impl NumberTermVar for Def {
    fn number_term_var(
        self,
        cs: &VariableLevelContextStack<Identifier>,
        number: &mut usize,
    ) -> Self {
        match self {
            Self::TermDef { name, term, inline } => Self::TermDef {
                name,
                term: term.map(|term| term.number_term_var(cs, number)),
                inline,
            },
            Self::TypeDef { name, typ, inline } => Self::TypeDef {
                name,
                typ: typ.map(|typ| typ.number_term_var(cs, number)),
                inline,
            },
            Self::AxiomDef { name, typ } => Self::AxiomDef {
                name,
                typ: typ.map(|typ| typ.number_term_var(cs, number)),
            },
            Self::InductionDef { name, constructors } => Self::InductionDef {
                name,
                constructors: constructors
                    .into_iter()
                    .map(|constructor| {
                        constructor.map(|constructor| constructor.number_term_var(cs, number))
                    })
                    .collect(),
            },
        }
    }
}

#[derive(Clone, Debug)]
pub enum Pattern {
    Named(Spanned<LocalVariable>),
    Wildcard,
}
impl Pattern {
    pub fn is_named(&self) -> bool {
        match self {
            Self::Named(_) => true,
            Self::Wildcard => false,
        }
    }
    pub fn is_wildcard(&self) -> bool {
        match self {
            Self::Named(_) => false,
            Self::Wildcard => true,
        }
    }

    pub fn spanned_local_variable(&self) -> Option<&Spanned<LocalVariable>> {
        match self {
            Self::Named(lvar) => Some(lvar),
            Self::Wildcard => None,
        }
    }
    pub fn local_variable(&self) -> Option<&LocalVariable> {
        match self {
            Self::Named(lvar) => Some(lvar),
            Self::Wildcard => None,
        }
    }

    pub fn spanned_identifier(&self) -> Option<Spanned<&Identifier>> {
        match self {
            Self::Named(lvar) => Some(lvar.as_ref().map(|lvar| lvar.id())),
            Self::Wildcard => None,
        }
    }
    pub fn identifier(&self) -> Option<&Identifier> {
        match self {
            Self::Named(lvar) => Some(lvar.id()),
            Self::Wildcard => None,
        }
    }
}
impl GrammaticalSize for Pattern {
    fn grammatical_size(&self) -> usize {
        1
    }
}
impl ToCode for Pattern {
    fn to_code(&self, fmt_ctx: &FormatContext) -> String {
        match self {
            Self::Named(lvar) => lvar.to_code(fmt_ctx),
            Self::Wildcard => "_".to_string(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct TermBind {
    pub pat: Spanned<Pattern>,
    pub typ: Box<Spanned<Type>>,
}
impl TermBind {
    pub fn spanned_identifier(&self) -> Option<Spanned<&Identifier>> {
        self.pat.spanned_identifier()
    }
    pub fn identifier(&self) -> Option<&Identifier> {
        self.pat.identifier()
    }
    pub fn spanned_local_variable(&self) -> Option<&Spanned<LocalVariable>> {
        self.pat.spanned_local_variable()
    }
    pub fn local_variable(&self) -> Option<&LocalVariable> {
        self.pat.local_variable()
    }

    pub fn spanned_type(&self) -> &Spanned<Type> {
        self.typ.as_ref()
    }
    pub fn typ(&self) -> &Type {
        self.typ.as_ref()
    }
}
impl NumberTermVar for TermBind {
    fn number_term_var(
        self,
        cs: &VariableLevelContextStack<Identifier>,
        number: &mut usize,
    ) -> Self {
        TermBind {
            pat: self.pat,
            typ: self.typ.map(|e| e.number_term_var(cs, number)).into(),
        }
    }
}
impl GrammaticalSize for TermBind {
    fn grammatical_size(&self) -> usize {
        self.pat.grammatical_size() + self.typ.grammatical_size() + 1
    }
}
impl ToCode for TermBind {
    fn to_code(&self, fmt_ctx: &FormatContext) -> String {
        format!(
            "{}: {}",
            self.pat.to_code(fmt_ctx),
            self.typ.to_code_subterm(fmt_ctx)
        )
    }
}
impl SubstitutableWithTerm for TermBind {
    fn substitute_term(self, from: VariableRef, to_term: &Spanned<Term>) -> TermBind {
        TermBind {
            pat: self.pat,
            typ: self.typ.map(|e| e.substitute_term(from, to_term)).into(),
        }
    }
}
impl SubstitutableWithType for TermBind {
    fn substitute_type(self, from: &Identifier, to_typ: &Spanned<Type>) -> TermBind {
        TermBind {
            pat: self.pat,
            typ: self.typ.map(|e| e.substitute_type(from, to_typ)).into(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Kind {
    ProperTypes,
    Arrow(Box<Spanned<Kind>>, Box<Spanned<Kind>>),
    TypeFamilies {
        term_bind: Spanned<TermBind>,
        body: Box<Spanned<Kind>>,
    },
}
impl Kind {
    pub fn is_proper_types(&self) -> bool {
        matches!(self, Self::ProperTypes)
    }
}
impl SubstitutableWithTerm for Kind {
    fn substitute_term(self, from: VariableRef, to_term: &Spanned<Term>) -> Self {
        match self {
            Self::ProperTypes => Self::ProperTypes,
            Self::Arrow(k1, k2) => Self::Arrow(
                k1.map(|e| e.substitute_term(from, to_term)).into(),
                k2.map(|e| e.substitute_term(from, to_term)).into(),
            ),
            Self::TypeFamilies { term_bind, body } => Self::TypeFamilies {
                term_bind: term_bind.map(|e| e.substitute_term(from, to_term)),
                body: body.map(|e| e.substitute_term(from, to_term)).into(),
            },
        }
    }
}
impl GrammaticalSize for Kind {
    fn grammatical_size(&self) -> usize {
        match self {
            Self::ProperTypes => 1,
            Self::Arrow(k1, k2) => 1 + k1.grammatical_size() + k2.grammatical_size(),
            Self::TypeFamilies { term_bind, body } => {
                1 + term_bind.grammatical_size() + body.grammatical_size()
            }
        }
    }
}
impl ToCode for Kind {
    fn to_code(&self, fmt_ctx: &FormatContext) -> String {
        match self {
            Self::ProperTypes => "*".to_string(),
            Self::Arrow(k1, k2) => format!(
                "{} => {}",
                {
                    match k1.value() {
                        Self::Arrow(..) => to_code_paren(k1.value(), fmt_ctx),
                        _ => k1.to_code(fmt_ctx),
                    }
                },
                k2.to_code(fmt_ctx)
            ),
            Self::TypeFamilies { term_bind, body } => {
                to_code_lambda_like(r"pi ", term_bind.value(), body.value(), fmt_ctx)
            }
        }
    }
}
impl NumberTermVar for Kind {
    fn number_term_var(
        self,
        cs: &VariableLevelContextStack<Identifier>,
        number: &mut usize,
    ) -> Self {
        match self {
            Self::ProperTypes => Self::ProperTypes,
            Self::Arrow(k1, k2) => Self::Arrow(
                k1.map(|e| e.number_term_var(cs, number)).into(),
                k2.map(|e| e.number_term_var(cs, number)).into(),
            ),
            Self::TypeFamilies { term_bind, body } => Self::TypeFamilies {
                term_bind: term_bind.map(|e| e.number_term_var(cs, number)),
                body: body.map(|e| e.number_term_var(cs, number)).into(),
            },
        }
    }
}
// impl TraverseTerm for Kind {
//     fn traverse_term(self, f: &mut impl FnMut(Term) -> Term) -> Self {
//         match self {
//             Self::ProperTypes => Self::ProperTypes,
//             Self::Arrow(k1, k2) => Self::Arrow(
//                 k1.map(|e| e.traverse(context, f)).into(),
//                 k2.map(|e| e.traverse(context, f)).into(),
//             ),
//             Self::TypeFamilies { term_bind, body } => Self::TypeFamilies {
//                 term_bind: term_bind.map(|e| e.traverse(context, f)),
//                 body: body.map(|e| e.traverse(context, f)).into(),
//             },
//         }
//     }
// }

#[derive(Clone, Debug)]
pub struct TypeBind {
    pub pat: Spanned<Pattern>,
    pub kind: Box<Spanned<Kind>>,
}
impl TypeBind {
    pub fn identifier(&self) -> Option<&Identifier> {
        self.pat.identifier()
    }
    pub fn spanned_identifier(&self) -> Option<Spanned<&Identifier>> {
        self.pat.spanned_identifier()
    }
    pub fn spanned_local_variable(&self) -> Option<&Spanned<LocalVariable>> {
        self.pat.spanned_local_variable()
    }
    pub fn local_variable(&self) -> Option<&LocalVariable> {
        self.pat.local_variable()
    }
    pub fn kind(&self) -> &Kind {
        self.kind.as_ref()
    }
    pub fn spanned_kind(&self) -> &Spanned<Kind> {
        self.kind.as_ref()
    }
}
impl GrammaticalSize for TypeBind {
    fn grammatical_size(&self) -> usize {
        self.pat.grammatical_size() + self.kind.grammatical_size() + 1
    }
}
impl ToCode for TypeBind {
    fn to_code(&self, fmt_ctx: &FormatContext) -> String {
        format!(
            "{}:: {}",
            self.pat.to_code(fmt_ctx),
            self.kind.to_code(fmt_ctx)
        )
    }
}
impl NumberTermVar for TypeBind {
    fn number_term_var(
        self,
        cs: &VariableLevelContextStack<Identifier>,
        number: &mut usize,
    ) -> Self {
        Self {
            pat: self.pat,
            kind: self.kind.map(|e| e.number_term_var(cs, number)).into(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Type {
    Var(Spanned<Identifier>),

    // Dependent types
    Pi {
        term_bind: Box<Spanned<TermBind>>,
        body: Box<Spanned<Type>>,
    },
    Sigma {
        term_bind: Box<Spanned<TermBind>>,
        body: Box<Spanned<Type>>,
    },

    // Universal types
    Forall {
        type_bind: Spanned<TypeBind>,
        body: Box<Spanned<Type>>,
    },

    // Higher-order types
    AbsType {
        type_bind: Spanned<TypeBind>,
        body: Box<Spanned<Type>>,
    },
    ApplyTerm(Box<Spanned<Type>>, Spanned<Term>),
    ApplyType(Box<Spanned<Type>>, Box<Spanned<Type>>),

    // Recursive types
    Mu {
        mu_name: Spanned<Identifier>,
        body: Box<Spanned<Type>>,
    },

    // extensions
    Unit,
    Nat,
    Bool,
    Prop,
    Prf,
}
impl Type {
    pub fn subterms_term(&self) -> Vec<&Spanned<Term>> {
        match self {
            Self::ApplyTerm(_, t2) => vec![t2],
            _ => vec![],
        }
    }
    pub fn subterms_type(&self) -> Vec<&Spanned<Type>> {
        match self {
            Self::Var(..) => vec![],
            Self::Pi { term_bind, body } => vec![term_bind.value.typ.as_ref(), body],
            Self::Sigma { term_bind, body } => vec![term_bind.value.typ.as_ref(), body],
            Self::Forall { body, .. } => vec![body],
            Self::AbsType { body, .. } => vec![body],
            Self::ApplyTerm(t1, _) => vec![t1],
            Self::ApplyType(t1, t2) => vec![t1, t2],
            Self::Mu { body, .. } => vec![body],
            Self::Unit | Self::Nat | Self::Bool | Self::Prop | Self::Prf => vec![],
        }
    }

    pub fn is_prop(&self) -> bool {
        matches!(self, Self::Prop)
    }
    pub fn is_prf(&self) -> bool {
        matches!(self, Self::Prf)
    }

    pub fn new_arrow(t1: Spanned<Type>, t2: Spanned<Type>) -> Self {
        Self::Pi {
            term_bind: Spanned::new(
                t1.span().cloned(),
                TermBind {
                    pat: Spanned::new(t1.span().cloned(), Pattern::Wildcard),
                    typ: t1.into(),
                },
            )
            .into(),
            body: t2.into(),
        }
    }

    // pub fn free_term_vars(&self) -> SpannedVars {
    //     match self {
    //         Self::Var(..) => Default::default(),
    //         Self::ApplyTerm(t1, t2) => t1.free_term_vars().extend(t2.free_term_vars()),
    //         Self::ApplyType(t1, typ) => t1.free_term_vars().extend(typ.free_term_vars()),
    //         Self::Pi { term_bind, body } => {
    //             let mut free_vars = body.free_term_vars();
    //             if let Pattern::Named { 0: name, .. } = &term_bind.pat.value {
    //                 free_vars.remove(name.value());
    //             }
    //             free_vars.extend(term_bind.typ().free_term_vars())
    //         }
    //         Self::Sigma { term_bind, body } => {
    //             let mut free_vars = body.free_term_vars();
    //             if let Pattern::Named { 0: name, .. } = &term_bind.pat.value {
    //                 free_vars.remove(name.value());
    //             }
    //             body.free_term_vars()
    //                 .extend(term_bind.typ().free_term_vars())
    //         }
    //         Self::Forall { body, .. } => body.free_term_vars(),
    //         Self::AbsType { body, .. } => body.free_term_vars(),
    //         Self::Mu { body, .. } => body.free_term_vars(),
    //         Self::Unit | Self::Nat | Self::Bool | Self::Prop | Self::Prf => Default::default(),
    //     }
    // }
    pub fn free_type_vars(&self) -> SpannedVars {
        match self {
            Self::Var(id) => {
                let mut vars = SpannedVars::default();
                vars.insert(id.as_ref());
                vars
            }
            Self::ApplyTerm(t1, t2) => t1.free_type_vars().extend(t2.free_type_vars()),
            Self::ApplyType(t1, typ) => t1.free_type_vars().extend(typ.free_type_vars()),
            Self::Pi { body, .. } => body.free_type_vars(),
            Self::Sigma { body, .. } => body.free_type_vars(),
            Self::Forall { type_bind, body } => {
                let mut vars = body.free_type_vars();
                if let Some(id) = type_bind.identifier() {
                    vars.remove(id);
                }
                vars
            }
            Self::AbsType { body, type_bind } => {
                let mut set = body.free_type_vars();
                if let Some(id) = type_bind.identifier() {
                    set.remove(id);
                }
                set
            }
            Self::Mu { mu_name, body } => {
                let mut set = body.free_type_vars();
                set.remove(mu_name.value());
                set
            }
            Self::Unit | Self::Nat | Self::Bool | Self::Prop | Self::Prf => Default::default(),
        }
    }

    pub fn to_code_subterm(&self, fmt_ctx: &FormatContext) -> String {
        match self {
            Self::Var(..) => self.to_code(fmt_ctx),
            Self::Unit | Self::Nat | Self::Bool | Self::Prop | Self::Prf => self.to_code(fmt_ctx),
            Self::ApplyType(..) => self.to_code(fmt_ctx),
            _ => to_code_paren(self, fmt_ctx),
        }
    }
    pub fn type_kind_name(&self) -> &'static str {
        match self {
            Self::Unit => "unit type",
            Self::Nat => "natural number type",
            Self::Bool => "boolean type",
            Self::Prop => "proposition type",
            Self::Prf => "proof type",
            Self::Var(..) => "type variables",
            Self::ApplyTerm(..) => "type application by term",
            Self::ApplyType(..) => "type application by type",
            Self::Pi { .. } => "pi types",
            Self::Sigma { .. } => "sigma types",
            Self::Forall { .. } => "forall types",
            Self::AbsType { .. } => "type abstraction by type",
            Self::Mu { .. } => "mu type",
        }
    }
}
impl GrammaticalSize for Type {
    fn grammatical_size(&self) -> usize {
        match self {
            Self::Var(..) => 1,
            Self::ApplyTerm(t1, t2) => t1.grammatical_size() + t2.grammatical_size() + 1,
            Self::ApplyType(t1, t2) => t1.grammatical_size() + t2.grammatical_size() + 1,
            Self::Pi { term_bind, body } => {
                term_bind.grammatical_size() + body.grammatical_size() + 1
            }
            Self::Sigma { term_bind, body } => {
                term_bind.grammatical_size() + body.grammatical_size() + 1
            }
            Self::Forall { type_bind, body } => {
                type_bind.grammatical_size() + body.grammatical_size() + 1
            }
            Self::AbsType { type_bind, body } => {
                type_bind.grammatical_size() + body.grammatical_size() + 1
            }
            // TODO
            Self::Mu { body, .. } => body.grammatical_size() + 1,
            Self::Unit | Self::Nat | Self::Bool | Self::Prop | Self::Prf => 1,
        }
    }
}
impl ToCode for Type {
    fn to_code(&self, fmt_ctx: &FormatContext) -> String {
        match self {
            Self::Var(id) => id.as_str().to_string(),
            Self::ApplyTerm(t1, t2) => {
                let s1 = match t1.value() {
                    Self::ApplyTerm(..) | Self::ApplyType(..) => t1.to_code(fmt_ctx),
                    _ => t1.to_code_subterm(fmt_ctx),
                };
                let s2 = t2.to_code_right_subterm(fmt_ctx);
                format!("{} {}", s1, s2)
            }
            Self::ApplyType(t1, t2) => {
                format!("{}[{}]", t1.to_code_subterm(fmt_ctx), t2.to_code(fmt_ctx,))
            }
            Self::Pi { term_bind, body } => {
                let TermBind { pat: name, typ } = term_bind.value();
                match name.value() {
                    Pattern::Wildcard => {
                        let c1 = match typ.value() {
                            Self::Pi { term_bind, .. } if term_bind.pat.is_wildcard() => {
                                to_code_paren(typ.value(), fmt_ctx)
                            }
                            _ => typ.to_code(fmt_ctx),
                        };
                        let c2 = body.to_code(fmt_ctx);
                        format!("{} -> {}", c1, c2)
                    }
                    Pattern::Named(..) => {
                        to_code_lambda_like(r"pi ", term_bind.value(), body.value(), fmt_ctx)
                    }
                }
            }
            Self::Sigma { term_bind, body } => {
                to_code_lambda_like(r"sigma ", term_bind.value(), body.value(), fmt_ctx)
            }
            Self::Forall { type_bind, body } => {
                to_code_lambda_like(r"forall ", type_bind.value(), body.value(), fmt_ctx)
            }
            Self::Mu { mu_name, body } => {
                format!("mu {}. {}", mu_name.to_string(), body.to_code(fmt_ctx,))
            }
            Self::AbsType { type_bind, body } => {
                format!(
                    r"\{}. {}",
                    type_bind.to_code(fmt_ctx),
                    body.to_code(fmt_ctx,)
                )
            }
            Self::Unit => "Unit".to_string(),
            Self::Nat => "Nat".to_string(),
            Self::Bool => "Bool".to_string(),
            Self::Prop => "Prop".to_string(),
            Self::Prf => "Prf".to_string(),
        }
    }
}
impl NumberTermVar for Type {
    fn number_term_var(
        self,
        cs: &VariableLevelContextStack<Identifier>,
        number: &mut usize,
    ) -> Self {
        match self {
            Self::Var(..) => self,
            Self::ApplyTerm(t1, t2) => Self::ApplyTerm(
                t1.map(|e| e.number_term_var(cs, number)).into(),
                t2.map(|e| e.number_term_var(cs, number)),
            ),
            Self::ApplyType(t1, t2) => Self::ApplyType(
                t1.map(|e| e.number_term_var(cs, number)).into(),
                t2.map(|e| e.number_term_var(cs, number)).into(),
            ),
            Self::Pi { term_bind, body } => {
                // let term_bind = term_bind.map(|e| e.number_term_var(cs, number));
                // let term_bind = if let Pattern::Named(lvar) = term_bind.value.pat.value {
                //     *number += 1;
                //     Spanned::new(
                //         term_bind.span,
                //         TermBind {
                //             pat: Spanned::new(
                //                 term_bind.value.pat.span,
                //                 Pattern::Named(lvar.map(|e| e.map_discriminator(|_| *number))),
                //             ),
                //             typ: term_bind.value.typ,
                //         },
                //     )
                // } else {
                //     term_bind
                // };
                inc!(cs, term_bind, number);
                let body = body.map(|e| e.number_term_var(cs, number)).into();
                Self::Pi {
                    term_bind: term_bind.into(),
                    body,
                }
            }
            Self::Sigma { term_bind, body } => {
                // let term_bind = term_bind.map(|e| e.number_term_var(cs, number));
                inc!(cs, term_bind, number);
                let body = body.map(|e| e.number_term_var(cs, number)).into();
                Self::Sigma {
                    term_bind: term_bind.into(),
                    body,
                }
            }
            Self::Forall { type_bind, body } => Self::Forall {
                type_bind: type_bind.map(|e| e.number_term_var(cs, number)),
                body: body.map(|e| e.number_term_var(cs, number)).into(),
            },
            Self::AbsType { type_bind, body } => Self::AbsType {
                type_bind: type_bind.map(|e| e.number_term_var(cs, number)),
                body: body.map(|e| e.number_term_var(cs, number)).into(),
            },
            Self::Mu { mu_name, body } => Self::Mu {
                mu_name,
                body: body.map(|e| e.number_term_var(cs, number)).into(),
            },
            Self::Unit | Self::Nat | Self::Bool | Self::Prop | Self::Prf => self,
        }
    }
}
impl SubstitutableWithTerm for Type {
    fn substitute_term(self, from: VariableRef, to_term: &Spanned<Term>) -> Self {
        match self {
            Self::Var(_) => self,
            Self::Pi { term_bind, body } => {
                let body = if term_bind
                    .local_variable()
                    .zip(from.local())
                    .map(|(lvar, from)| *from == lvar.as_ref())
                    .unwrap_or_default()
                {
                    body
                } else {
                    body.map(|e| e.substitute_term(from, to_term)).into()
                };
                Self::Pi {
                    term_bind: term_bind.map(|e| e.substitute_term(from, to_term)).into(),
                    body,
                }
            }
            Self::Sigma { term_bind, body } => {
                let body = if term_bind
                    .local_variable()
                    .zip(from.local())
                    .map(|(lvar, from)| *from == lvar.as_ref())
                    .unwrap_or_default()
                {
                    body
                } else {
                    body.map(|e| e.substitute_term(from, to_term)).into()
                };
                Self::Sigma {
                    term_bind: term_bind.map(|e| e.substitute_term(from, to_term)).into(),
                    body,
                }
            }
            Self::Forall { type_bind, body } => Self::Forall {
                type_bind,
                body: body.map(|e| e.substitute_term(from, to_term)).into(),
            },
            Self::AbsType { type_bind, body } => Self::AbsType {
                type_bind,
                body: body.map(|e| e.substitute_term(from, to_term)).into(),
            },
            Self::ApplyTerm(t1, t2) => Self::ApplyTerm(
                t1.map(|e| e.substitute_term(from, to_term)).into(),
                t2.map(|e| e.substitute_term(from, to_term)),
            ),
            Self::ApplyType(t1, t2) => {
                Self::ApplyType(t1.map(|e| e.substitute_term(from, to_term)).into(), t2)
            }
            Self::Mu { mu_name, body } => Self::Mu {
                mu_name,
                body: body.map(|e| e.substitute_term(from, to_term)).into(),
            },
            Self::Unit | Self::Nat | Self::Bool | Self::Prop | Self::Prf => self,
        }
    }
}
impl SubstitutableWithType for Type {
    fn substitute_type(self, from: &Identifier, to_typ: &Spanned<Type>) -> Self {
        match self {
            Self::Var(id) => {
                if id.value() == from {
                    to_typ.value().clone()
                } else {
                    Self::Var(id)
                }
            }
            Self::Pi { term_bind, body } => Self::Pi {
                term_bind: term_bind.map(|e| e.substitute_type(from, to_typ)).into(),
                body: body.map(|e| e.substitute_type(from, to_typ)).into(),
            },
            Self::Sigma { term_bind, body } => Self::Sigma {
                term_bind: term_bind.map(|e| e.substitute_type(from, to_typ)).into(),
                body: body.map(|e| e.substitute_type(from, to_typ)).into(),
            },
            Self::Forall { type_bind, body } => {
                if type_bind.identifier() == Some(from) {
                    Self::Forall { type_bind, body }
                } else {
                    Self::Forall {
                        type_bind,
                        body: body.map(|e| e.substitute_type(from, to_typ)).into(),
                    }
                }
            }
            Self::AbsType { type_bind, body } => {
                if type_bind.identifier() == Some(from) {
                    Self::AbsType { type_bind, body }
                } else {
                    Self::AbsType {
                        type_bind,
                        body: body.map(|e| e.substitute_type(from, to_typ)).into(),
                    }
                }
            }
            Self::ApplyTerm(t1, t2) => Self::ApplyTerm(
                t1.map(|e| e.substitute_type(from, to_typ)).into(),
                t2.map(|e| e.substitute_type(from, to_typ)),
            ),
            Self::ApplyType(t1, t2) => Self::ApplyType(
                t1.map(|e| e.substitute_type(from, to_typ)).into(),
                t2.map(|e| e.substitute_type(from, to_typ)).into(),
            ),
            Self::Mu { mu_name, body } => {
                if mu_name.value() == from {
                    Self::Mu { mu_name, body }
                } else {
                    Self::Mu {
                        mu_name,
                        body: body.map(|e| e.substitute_type(from, to_typ)).into(),
                    }
                }
            }
            Self::Unit | Self::Nat | Self::Bool | Self::Prop | Self::Prf => self,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Term {
    Var(Spanned<Variable>),
    AbsTerm {
        term_bind: Spanned<TermBind>,
        body: Box<Spanned<Term>>,
    },
    AbsType {
        type_bind: Spanned<TypeBind>,
        body: Box<Spanned<Term>>,
    },
    ApplyTerm(Box<Spanned<Term>>, Box<Spanned<Term>>),
    ApplyType(Box<Spanned<Term>>, Box<Spanned<Type>>),

    // extensions
    Nat(BigUint),
    NatSucc,
    NatPred,
    NatIsZero,
    Bool(bool),
    Let {
        pat: Spanned<Pattern>,
        val: Box<Spanned<Term>>,
        body: Box<Spanned<Term>>,
    },
    Seq(Box<Spanned<Term>>, Box<Spanned<Term>>),
    BoolIf {
        cond: Box<Spanned<Term>>,
        then_clause: Box<Spanned<Term>>,
        else_clause: Box<Spanned<Term>>,
    },
    Unit,
    All {
        term_bind: Spanned<TermBind>,
        body: Box<Spanned<Term>>,
    },
    Ex {
        term_bind: Spanned<TermBind>,
        body: Box<Spanned<Term>>,
    },
    Match {
        scrutinee: Box<Spanned<Term>>,
        arms: Vec<Spanned<MatchArm>>,
    },

    // fix by unary operator
    Fix(Box<Spanned<Term>>),

    // fix by lambda definition variant
    LambdaFix {
        term_bind: Spanned<TermBind>,
        body: Box<Spanned<Term>>,
    },
    LambdaFixCallee,
}
impl Term {
    pub fn subterms_term(&self) -> Vec<&Spanned<Term>> {
        match self {
            Self::Var(..) | Self::Nat(..) | Self::Bool(..) | Self::Unit => vec![],
            Self::AbsTerm { body, .. } => vec![body],
            Self::AbsType { body, .. } => vec![body],
            Self::ApplyTerm(t1, t2) => vec![t1, t2],
            Self::ApplyType(t1, _) => vec![t1],
            Self::NatSucc | Self::NatPred | Self::NatIsZero => vec![],
            Self::BoolIf {
                cond,
                then_clause,
                else_clause,
            } => vec![cond, then_clause, else_clause],
            Self::Let { val, body, .. } => vec![val, body],
            Self::Seq(t1, t2) => vec![t1, t2],
            Self::All { body, .. } | Self::Ex { body, .. } => {
                vec![body]
            }
            Self::Match { scrutinee, arms } => {
                let mut subterms = vec![scrutinee.as_ref()];
                for arm in arms {
                    subterms.push(&arm.body);
                }
                subterms
            }
            Self::Fix(t) => vec![t],
            Self::LambdaFix { body, .. } => vec![body],
            Self::LambdaFixCallee => vec![],
        }
    }
    pub fn subterms_type(&self) -> Vec<&Spanned<Type>> {
        match self {
            Self::Var(..) | Self::Nat(..) | Self::Bool(..) | Self::Unit => vec![],
            Self::AbsTerm { term_bind, .. } => {
                vec![term_bind.value.typ.as_ref()]
            }
            Self::AbsType { .. } => vec![],
            Self::ApplyTerm(..) => vec![],
            Self::ApplyType(_, typ) => vec![typ.as_ref()],
            Self::NatSucc | Self::NatPred | Self::NatIsZero => vec![],
            Self::BoolIf { .. } => vec![],
            Self::Let { .. } => vec![],
            Self::Seq(..) => vec![],
            Self::All { term_bind, .. } | Self::Ex { term_bind, .. } => {
                vec![term_bind.value.typ.as_ref()]
            }
            Self::Match { .. } => vec![],
            Self::Fix(..) => vec![],
            Self::LambdaFix { term_bind, .. } => {
                vec![term_bind.value.typ.as_ref()]
            }
            Self::LambdaFixCallee => vec![],
        }
    }
    // pub fn free_term_vars(&self) -> SpannedVars {
    //     match self {
    //         Self::Var(qid) => {
    //             let mut free_vars = SpannedVars::new();
    //             // TODO: このパターンだけ自由変数とみなす正当性はない
    //             if let Some(p) = qid.head() {
    //                 if let QualifiedIdentifierPart::Normal(id) = p.value() {
    //                     free_vars.insert(&Spanned::new(p.span, id.clone()));
    //                 }
    //             }
    //             free_vars
    //         }
    //         Self::AbsTerm { term_bind, body } => {
    //             let TermBind { pat: name, typ } = term_bind.value();
    //             let mut free_vars = body.free_term_vars().extend(typ.free_term_vars());
    //             match name.value() {
    //                 Pattern::Named(id) => {
    //                     free_vars.remove(id.value());
    //                 }
    //                 Pattern::Wildcard => {}
    //             }
    //             free_vars
    //         }
    //         Self::AbsType { type_bind, body } => {
    //             let mut free_vars = body.free_term_vars();
    //             if let Some(id) = type_bind.identifier() {
    //                 free_vars.remove(id);
    //             }
    //             free_vars
    //         }
    //         Self::ApplyTerm(t1, t2) => {
    //             let free_vars = t1.free_term_vars().extend(t2.free_term_vars());
    //             free_vars
    //         }
    //         Self::ApplyType(t1, t2) => {
    //             let free_vars = t1.free_term_vars().extend(t2.free_term_vars());
    //             free_vars
    //         }
    //         Self::All { term_bind, body } | Self::Ex { term_bind, body } => {
    //             let TermBind { pat: name, typ } = term_bind.value();
    //             let mut free_vars = body.free_term_vars().extend(typ.free_term_vars());
    //             if let Some(id) = name.identifier() {
    //                 free_vars.remove(id);
    //             }
    //             free_vars
    //         }
    //         Self::Fix(t) => t.free_term_vars(),
    //         Self::LambdaFix { term_bind, body } => {
    //             let mut free_vars = body.free_term_vars();
    //             if let Some(id) = term_bind.identifier() {
    //                 free_vars.remove(id);
    //             }
    //             free_vars
    //         }
    //         Self::Match { scrutinee, arms } => {
    //             let mut free_vars = scrutinee.free_term_vars();
    //             for arm in arms {
    //                 free_vars = free_vars.extend(arm.free_term_vars());
    //             }
    //             free_vars
    //         }
    //         Self::Let { pat, val, body } => {
    //             let mut free_vars = body.free_term_vars();
    //             if let Some(name) = pat.identifier() {
    //                 free_vars.remove(name);
    //             }
    //             free_vars = free_vars.extend(val.free_term_vars());
    //             free_vars
    //         }
    //         Self::Seq(t1, t2) => {
    //             let free_vars = t1.free_term_vars().extend(t2.free_term_vars());
    //             free_vars
    //         }
    //         Self::BoolIf {
    //             cond,
    //             then_clause,
    //             else_clause,
    //         } => {
    //             let free_vars = cond
    //                 .free_term_vars()
    //                 .extend(then_clause.free_term_vars())
    //                 .extend(else_clause.free_term_vars());
    //             free_vars
    //         }
    //         Self::Unit
    //         | Self::NatSucc
    //         | Self::NatPred
    //         | Self::NatIsZero
    //         | Self::Nat(..)
    //         | Self::Bool(..)
    //         | Self::LambdaFixCallee => Default::default(),
    //     }
    // }
    pub fn free_type_vars(&self) -> SpannedVars {
        match self {
            Self::Var(..) => SpannedVars::new(),
            Self::AbsTerm { term_bind, body } => {
                let TermBind { pat: name, typ } = term_bind.value();
                let mut free_vars = body.free_type_vars();
                match name.value() {
                    Pattern::Named(lvar) => {
                        free_vars.remove(lvar.id());
                    }
                    Pattern::Wildcard => {}
                }
                free_vars = free_vars.extend(typ.free_type_vars());
                free_vars
            }
            Self::AbsType { type_bind, body } => {
                let mut free_vars = body.free_type_vars();
                if let Some(id) = type_bind.spanned_identifier() {
                    free_vars.insert(id);
                }
                free_vars
            }
            Self::ApplyTerm(t1, t2) => {
                let free_vars = t1.free_type_vars().extend(t2.free_type_vars());
                free_vars
            }
            Self::ApplyType(t1, t2) => {
                let free_vars = t1.free_type_vars().extend(t2.free_type_vars());
                free_vars
            }
            Self::All { term_bind, body } | Self::Ex { term_bind, body } => {
                let TermBind { pat: name, typ } = term_bind.value();
                let mut free_vars = body.free_type_vars().extend(typ.free_type_vars());
                if let Some(id) = name.identifier() {
                    free_vars.remove(id);
                }
                free_vars = free_vars.extend(typ.free_type_vars());
                free_vars
            }
            Self::Fix(t) => t.free_type_vars(),
            Self::LambdaFix { term_bind, body } => {
                let mut free_vars = body.free_type_vars();
                if let Some(id) = term_bind.spanned_identifier() {
                    free_vars.insert(id);
                }
                free_vars
            }
            Self::Match { scrutinee, arms } => {
                let mut free_vars = scrutinee.free_type_vars();
                for arm in arms {
                    free_vars = free_vars.extend(arm.free_type_vars());
                }
                free_vars
            }
            Self::Let { val, body, .. } => {
                let free_vars = body.free_type_vars().extend(val.free_type_vars());
                free_vars
            }
            Self::Seq(t1, t2) => {
                let free_vars = t1.free_type_vars().extend(t2.free_type_vars());
                free_vars
            }
            Self::BoolIf {
                cond,
                then_clause,
                else_clause,
            } => {
                let free_vars = cond
                    .free_type_vars()
                    .extend(then_clause.free_type_vars())
                    .extend(else_clause.free_type_vars());
                free_vars
            }
            Self::Unit
            | Self::NatSucc
            | Self::NatPred
            | Self::NatIsZero
            | Self::Nat(..)
            | Self::Bool(..)
            | Self::LambdaFixCallee => SpannedVars::new(),
        }
    }

    pub fn to_code_subterm(&self, fmt_ctx: &FormatContext) -> String {
        match self {
            Self::Var(..)
            | Self::Nat(..)
            | Self::Bool(..)
            | Self::Unit
            | Self::LambdaFixCallee
            | Self::NatSucc
            | Self::NatPred
            | Self::NatIsZero => self.to_code(fmt_ctx),
            Self::ApplyTerm(..) | Self::ApplyType(..) => self.to_code(fmt_ctx),
            // Self::AbsTerm { .. } | Self::AbsType { .. } | Self::All { .. } | Self::Ex { .. } => {
            //     self.to_code(new_line,indent,current_indent)
            // }
            // _ => format!("({})", self.to_code(fmt_ctx)),
            _ => to_code_paren(self, fmt_ctx),
        }
    }
    pub fn to_code_right_subterm(&self, fmt_ctx: &FormatContext) -> String {
        match self {
            Self::Var(..)
            | Self::Nat(..)
            | Self::Bool(..)
            | Self::Unit
            | Self::LambdaFixCallee
            | Self::NatSucc
            | Self::NatPred
            | Self::NatIsZero => self.to_code(fmt_ctx),
            Self::ApplyType(..) => self.to_code(fmt_ctx),
            _ => to_code_paren(self, fmt_ctx),
            // _ => format!("({})", self.to_code(fmt_ctx)),
        }
    }

    pub fn nat(&self) -> Option<&BigUint> {
        match self {
            Self::Nat(n) => Some(n),
            _ => None,
        }
    }

    pub fn term_kind_name(&self) -> &'static str {
        match self {
            Self::Var(..) => "variable",
            Self::Nat(..) => "natural number values",
            Self::Bool(..) => "boolean values",
            Self::Unit => "unit value",
            Self::LambdaFixCallee => "lambda fix callee",
            Self::NatSucc => "nat succ",
            Self::NatPred => "nat pred",
            Self::NatIsZero => "nat is zero",
            Self::ApplyTerm(..) => "term application",
            Self::ApplyType(..) => "type application",
            Self::AbsTerm { .. } => "term abstraction",
            Self::AbsType { .. } => "type abstraction",
            Self::All { .. } => "forall",
            Self::Ex { .. } => "exists",
            Self::Fix(..) => "fix",
            Self::LambdaFix { .. } => "lambda fix",
            Self::Match { .. } => "match",
            Self::Let { .. } => "let",
            Self::Seq(..) => "sequence",
            Self::BoolIf { .. } => "if",
        }
    }
}
impl GrammaticalSize for Term {
    fn grammatical_size(&self) -> usize {
        match self {
            Self::Var(..)
            | Self::Nat(..)
            | Self::Bool(..)
            | Self::Unit
            | Self::LambdaFixCallee
            | Self::NatSucc
            | Self::NatPred
            | Self::NatIsZero => 1,
            Self::ApplyTerm(t1, t2) => t1.grammatical_size() + t2.grammatical_size() + 1,
            Self::ApplyType(t1, t2) => t1.grammatical_size() + t2.grammatical_size() + 1,
            Self::AbsTerm { term_bind, body } => {
                term_bind.grammatical_size() + body.grammatical_size() + 1
            }
            Self::AbsType { type_bind, body } => {
                type_bind.grammatical_size() + body.grammatical_size() + 1
            }
            Self::All { term_bind, body } => {
                term_bind.grammatical_size() + body.grammatical_size() + 1
            }
            Self::Ex { term_bind, body } => {
                term_bind.grammatical_size() + body.grammatical_size() + 1
            }
            Self::Fix(t) => t.grammatical_size() + 1,
            Self::LambdaFix { term_bind, body } => {
                term_bind.grammatical_size() + body.grammatical_size() + 1
            }
            Self::Match { scrutinee, arms } => {
                scrutinee.grammatical_size()
                    + arms.iter().map(|a| a.grammatical_size()).sum::<usize>()
                    + 1
            }
            Self::Let { val, body, .. } => val.grammatical_size() + body.grammatical_size() + 1,
            Self::Seq(t1, t2) => t1.grammatical_size() + t2.grammatical_size() + 1,
            Self::BoolIf {
                cond,
                then_clause,
                else_clause,
            } => {
                cond.grammatical_size()
                    + then_clause.grammatical_size()
                    + else_clause.grammatical_size()
                    + 1
            }
        }
    }
}
impl ToCode for Term {
    fn to_code(&self, fmt_ctx: &FormatContext) -> String {
        match self {
            Self::Var(var) => var.to_code(fmt_ctx),
            Self::AbsTerm { term_bind, body } => {
                to_code_lambda_like(r"\", term_bind.value(), body.value(), fmt_ctx)
            }
            Self::AbsType { type_bind, body } => {
                to_code_lambda_like(r"\", type_bind.value(), body.value(), fmt_ctx)
            }
            Self::ApplyTerm(t1, t2) => {
                let callee = t1.to_code_subterm(fmt_ctx);
                let arg = t2.to_code_right_subterm(fmt_ctx);
                format!("{callee} {arg}",)
            }
            Self::ApplyType(t1, typ) => {
                format!("{}[{}]", t1.to_code_subterm(fmt_ctx), typ.to_code(fmt_ctx))
            }
            Self::All { term_bind, body } => {
                to_code_lambda_like(r"all ", term_bind.value(), body.value(), fmt_ctx)
            }
            Self::Ex { term_bind, body } => {
                to_code_lambda_like(r"ex ", term_bind.value(), body.value(), fmt_ctx)
            }

            Self::Unit => "unit".to_string(),
            Self::NatSucc => "succ".to_string(),
            Self::NatPred => "pred".to_string(),
            Self::NatIsZero => "is_zero".to_string(),
            Self::Nat(n) => n.to_string(),
            Self::Bool(b) => b.to_string(),
            Self::Fix(t) => format!("fix {}", t.to_code_subterm(fmt_ctx)),
            Self::LambdaFix { term_bind, body } => {
                to_code_lambda_like(r"fix ", term_bind.value(), body.value(), fmt_ctx)
            }
            Self::LambdaFixCallee => "%".to_string(),
            Self::Match { scrutinee, arms } => format!(
                "match {} {{ {} }}",
                scrutinee.to_code(fmt_ctx),
                arms.iter()
                    .map(|arm| arm.to_code(fmt_ctx))
                    .collect::<Vec<_>>()
                    .join(" | ")
            ),
            Self::Let {
                pat: name,
                val,
                body,
            } => {
                let name = name.to_code(fmt_ctx);
                let val = val.to_code_subterm(fmt_ctx);
                let body = body.to_code(fmt_ctx);
                let new_line = fmt_ctx.new_line();
                let current_indent = fmt_ctx.current_indent();
                format!("let {name} = {val};{new_line}{current_indent}{body}")
            }
            Self::Seq(t1, t2) => {
                let s1 = t1.to_code_subterm(fmt_ctx);
                let s2 = match t2.value() {
                    Self::Seq { .. } => t2.to_code(fmt_ctx),
                    _ => t2.to_code_subterm(fmt_ctx),
                };
                let new_line = fmt_ctx.new_line();
                let current_indent = fmt_ctx.current_indent();
                format!("{s1};{new_line}{current_indent}{s2}")
            }
            Self::BoolIf {
                cond,
                then_clause,
                else_clause,
            } => format!(
                "if {} {{{}}} else {{{}}}",
                cond.to_code(fmt_ctx),
                then_clause.to_code(fmt_ctx),
                else_clause.to_code(fmt_ctx)
            ),
        }
    }
}

impl NumberTermVar for Term {
    fn number_term_var(
        self,
        cs: &VariableLevelContextStack<Identifier>,
        number: &mut usize,
    ) -> Self {
        match self {
            Self::Var(var) => match var.value {
                Variable::Local(lvar) => Self::Var(Spanned::new(
                    var.span,
                    match cs.get(&lvar.id) {
                        Some((_, number)) => Variable::new_local(lvar.id, number),
                        None => Variable::new_qualified_head(Spanned::new(var.span, lvar.id)),
                    },
                )),
                _ => Self::Var(var),
            },
            Self::AbsTerm { term_bind, body } => {
                // let term_bind = term_bind.map(|e| e.number_term_var(cs, number));
                inc!(cs, term_bind, number);
                let body = body.map(|e| e.number_term_var(cs, number)).into();
                Self::AbsTerm { term_bind, body }
            }
            Self::AbsType { type_bind, body } => Self::AbsType {
                type_bind: type_bind.map(|e| e.number_term_var(cs, number)),
                body: body.map(|e| e.number_term_var(cs, number)).into(),
            },
            Self::ApplyTerm(t1, t2) => Self::ApplyTerm(
                t1.map(|e| e.number_term_var(cs, number)).into(),
                t2.map(|e| e.number_term_var(cs, number)).into(),
            ),
            Self::ApplyType(t1, typ) => Self::ApplyType(
                t1.map(|e| e.number_term_var(cs, number)).into(),
                typ.map(|e| e.number_term_var(cs, number)).into(),
            ),
            Self::All { term_bind, body } => {
                // let term_bind = term_bind.map(|e| e.number_term_var(cs, number));
                inc!(cs, term_bind, number);
                let body = body.map(|e| e.number_term_var(cs, number)).into();
                Self::All { term_bind, body }
            }
            Self::Ex { term_bind, body } => {
                // let term_bind = term_bind.map(|e| e.number_term_var(cs, number));
                inc!(cs, term_bind, number);
                let body = body.map(|e| e.number_term_var(cs, number)).into();
                Self::Ex { term_bind, body }
            }
            Self::Unit => Self::Unit,
            Self::NatSucc => Self::NatSucc,
            Self::NatPred => Self::NatPred,
            Self::NatIsZero => Self::NatIsZero,
            Self::Nat(n) => Self::Nat(n),
            Self::Bool(b) => Self::Bool(b),
            Self::Fix(t) => Self::Fix(t.map(|e| e.number_term_var(cs, number)).into()),
            Self::LambdaFix { term_bind, body } => {
                // let term_bind = term_bind.map(|e| e.number_term_var(cs, number));
                inc!(cs, term_bind, number);
                let body = body.map(|e| e.number_term_var(cs, number)).into();
                Self::LambdaFix { term_bind, body }
            }
            Self::LambdaFixCallee => Self::LambdaFixCallee,
            Self::Match { scrutinee, arms } => Self::Match {
                scrutinee: scrutinee.map(|e| e.number_term_var(cs, number)).into(),
                arms: arms
                    .into_iter()
                    .map(|arm| arm.map(|e| e.number_term_var(cs, number)))
                    .collect::<Vec<_>>(),
            },
            Self::Let { pat, val, body } => {
                // let val = val.map(|e| e.number_term_var(cs, number)).into();
                // inc!(cs, pat, number);
                let val = val.map(|e| e.number_term_var(cs, number)).into();
                let pat = if let Pattern::Named(lvar) = pat.value {
                    *number += 1;
                    Spanned::new(
                        pat.span,
                        Pattern::Named(lvar.map(|e| e.map_discriminator(|_| *number))),
                    )
                } else {
                    pat
                };
                let _scope = pat
                    .spanned_local_variable()
                    .map(|lvar| cs.push(Spanned::new(lvar.span, lvar.id().clone()), *number));
                let body = body.map(|e| e.number_term_var(cs, number)).into();
                Self::Let { pat, val, body }
            }
            Self::Seq(t1, t2) => Self::Seq(
                t1.map(|e| e.number_term_var(cs, number)).into(),
                t2.map(|e| e.number_term_var(cs, number)).into(),
            ),
            Self::BoolIf {
                cond,
                then_clause,
                else_clause,
            } => Self::BoolIf {
                cond: cond.map(|e| e.number_term_var(cs, number)).into(),
                then_clause: then_clause.map(|e| e.number_term_var(cs, number)).into(),
                else_clause: else_clause.map(|e| e.number_term_var(cs, number)).into(),
            },
        }
    }
}
impl SubstitutableWithTerm for Term {
    fn substitute_term(self, from: VariableRef, to_term: &Spanned<Term>) -> Self {
        match self {
            Self::Var(var) => {
                if var.value().as_ref() == from {
                    to_term.value().clone()
                } else {
                    Self::Var(var)
                }
            }
            Self::AbsTerm { term_bind, body } => {
                let body = if term_bind
                    .pat
                    .local_variable()
                    .zip(from.local())
                    .map(|(lvar, from)| *from == lvar.as_ref())
                    .unwrap_or_default()
                {
                    body
                } else {
                    body.map(|e| e.substitute_term(from, to_term)).into()
                };
                Self::AbsTerm {
                    term_bind: term_bind.map(|e| e.substitute_term(from, to_term)),
                    body,
                }
            }
            Self::All { term_bind, body } => {
                let body = if term_bind
                    .pat
                    .local_variable()
                    .zip(from.local())
                    .map(|(lvar, from)| *from == lvar.as_ref())
                    .unwrap_or_default()
                {
                    body
                } else {
                    body.map(|e| e.substitute_term(from, to_term)).into()
                };
                Self::All {
                    term_bind: term_bind.map(|e| e.substitute_term(from, to_term)),
                    body,
                }
            }
            Self::Ex { term_bind, body } => {
                let body = if term_bind
                    .pat
                    .local_variable()
                    .zip(from.local())
                    .map(|(lvar, from)| *from == lvar.as_ref())
                    .unwrap_or_default()
                {
                    body
                } else {
                    body.map(|e| e.substitute_term(from, to_term)).into()
                };
                Self::Ex {
                    term_bind: term_bind.map(|e| e.substitute_term(from, to_term)),
                    body,
                }
            }
            Self::Fix(t) => Self::Fix(t.map(|e| e.substitute_term(from, to_term)).into()),
            Self::LambdaFix { term_bind, body } => {
                let body = if term_bind
                    .pat
                    .local_variable()
                    .zip(from.local())
                    .map(|(lvar, from)| *from == lvar.as_ref())
                    .unwrap_or_default()
                {
                    body
                } else {
                    body.map(|e| e.substitute_term(from, to_term)).into()
                };
                Self::LambdaFix {
                    term_bind: term_bind.map(|e| e.substitute_term(from, to_term)),
                    body,
                }
            }
            Self::AbsType { type_bind, body } => Self::AbsType {
                type_bind,
                body: body.map(|e| e.substitute_term(from, to_term)).into(),
            },
            Self::ApplyTerm(t1, t2) => Self::ApplyTerm(
                t1.map(|e| e.substitute_term(from, to_term)).into(),
                t2.map(|e| e.substitute_term(from, to_term)).into(),
            ),
            Self::ApplyType(t1, t2) => Self::ApplyType(
                t1.map(|e| e.substitute_term(from, to_term)).into(),
                t2.map(|e| e.substitute_term(from, to_term)).into(),
            ),
            Self::Nat(..)
            | Self::Bool(..)
            | Self::NatSucc
            | Self::NatPred
            | Self::NatIsZero
            | Self::Unit
            | Self::LambdaFixCallee => self,
            Self::Let {
                pat: name,
                val,
                body,
            } => {
                if name
                    .local_variable()
                    .zip(from.local())
                    .map(|(lvar, from)| *from == lvar.as_ref())
                    .unwrap_or_default()
                {
                    Self::Let {
                        pat: name,
                        val,
                        body,
                    }
                } else {
                    Self::Let {
                        pat: name,
                        val: val.map(|e| e.substitute_term(from, to_term)).into(),
                        body: body.map(|e| e.substitute_term(from, to_term)).into(),
                    }
                }
            }
            Self::Seq(t1, t2) => Self::Seq(
                t1.map(|e| e.substitute_term(from, to_term)).into(),
                t2.map(|e| e.substitute_term(from, to_term)).into(),
            ),
            Self::BoolIf {
                cond,
                then_clause,
                else_clause,
            } => Self::BoolIf {
                cond: cond.map(|e| e.substitute_term(from, to_term)).into(),
                then_clause: then_clause.map(|e| e.substitute_term(from, to_term)).into(),
                else_clause: else_clause.map(|e| e.substitute_term(from, to_term)).into(),
            },
            Self::Match { scrutinee, arms } => Self::Match {
                scrutinee: scrutinee.map(|e| e.substitute_term(from, to_term)).into(),
                arms: arms
                    .into_iter()
                    .map(|e| e.map(|e| e.substitute_term(from, to_term)))
                    .collect(),
            },
        }
    }
}
impl SubstitutableWithType for Term {
    fn substitute_type(self, from: &Identifier, to_typ: &Spanned<Type>) -> Self {
        match self {
            Self::Var(var) => Self::Var(var),
            Self::AbsTerm { term_bind, body } => Self::AbsTerm {
                term_bind: term_bind.map(|e| e.substitute_type(from, to_typ)),
                body: body.map(|e| e.substitute_type(from, to_typ)).into(),
            },
            Self::All { term_bind, body } => Self::All {
                term_bind: term_bind.map(|e| e.substitute_type(from, to_typ)),
                body: body.map(|e| e.substitute_type(from, to_typ)).into(),
            },
            Self::Ex { term_bind, body } => Self::Ex {
                term_bind: term_bind.map(|e| e.substitute_type(from, to_typ)),
                body: body.map(|e| e.substitute_type(from, to_typ)).into(),
            },
            Self::Fix(t) => Self::Fix(t.map(|e| e.substitute_type(from, to_typ)).into()),
            Self::LambdaFix { term_bind, body } => Self::LambdaFix {
                term_bind: term_bind.map(|e| e.substitute_type(from, to_typ)),
                body: body.map(|e| e.substitute_type(from, to_typ)).into(),
            },
            Self::AbsType { type_bind, body } => {
                if type_bind.identifier() == Some(from) {
                    Self::AbsType { type_bind, body }
                } else {
                    Self::AbsType {
                        type_bind,
                        body: body.map(|e| e.substitute_type(from, to_typ)).into(),
                    }
                }
            }
            Self::ApplyTerm(t1, t2) => Self::ApplyTerm(
                t1.map(|e| e.substitute_type(from, to_typ)).into(),
                t2.map(|e| e.substitute_type(from, to_typ)).into(),
            ),
            Self::ApplyType(t1, t2) => Self::ApplyType(
                t1.map(|e| e.substitute_type(from, to_typ)).into(),
                t2.map(|e| e.substitute_type(from, to_typ)).into(),
            ),
            Self::Nat(..)
            | Self::Bool(..)
            | Self::NatSucc
            | Self::NatPred
            | Self::NatIsZero
            | Self::Unit
            | Self::LambdaFixCallee => self,
            Self::Let {
                pat: name,
                val,
                body,
            } => Self::Let {
                pat: name,
                val: val.map(|e| e.substitute_type(from, to_typ)).into(),
                body: body.map(|e| e.substitute_type(from, to_typ)).into(),
            },
            Self::Seq(t1, t2) => Self::Seq(
                t1.map(|e| e.substitute_type(from, to_typ)).into(),
                t2.map(|e| e.substitute_type(from, to_typ)).into(),
            ),
            Self::BoolIf {
                cond,
                then_clause,
                else_clause,
            } => Self::BoolIf {
                cond: cond.map(|e| e.substitute_type(from, to_typ)).into(),
                then_clause: then_clause.map(|e| e.substitute_type(from, to_typ)).into(),
                else_clause: else_clause.map(|e| e.substitute_type(from, to_typ)).into(),
            },
            Self::Match { scrutinee, arms } => Self::Match {
                scrutinee: scrutinee.map(|e| e.substitute_type(from, to_typ)).into(),
                arms: arms
                    .into_iter()
                    .map(|e| e.map(|e| e.substitute_type(from, to_typ)))
                    .collect(),
            },
        }
    }
}
impl Display for Term {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_code(&FormatContext::new_compact()))
    }
}

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub constructor: Spanned<Identifier>,
    pub args: Vec<Spanned<Pattern>>,
    pub body: Spanned<Term>,
}
impl MatchArm {
    // pub fn free_term_vars(&self) -> SpannedVars {
    //     let mut free_vars = self.body.free_term_vars();
    //     for arg in &self.args {
    //         if let Some(id) = arg.identifier() {
    //             free_vars.remove(id);
    //         }
    //     }
    //     free_vars
    // }
    pub fn free_type_vars(&self) -> SpannedVars {
        self.body.free_type_vars()
    }
}
impl GrammaticalSize for MatchArm {
    fn grammatical_size(&self) -> usize {
        self.args
            .iter()
            .map(|arg| arg.grammatical_size())
            .sum::<usize>()
            + self.body.grammatical_size()
            + 1
    }
}
impl ToCode for MatchArm {
    fn to_code(&self, fmt_ctx: &FormatContext) -> String {
        format!(
            "{} {} => {}",
            self.constructor.to_string(),
            self.args
                .iter()
                .map(|arg| arg.to_code(fmt_ctx))
                .collect::<Vec<_>>()
                .join(" "),
            self.body.to_code(fmt_ctx)
        )
    }
}
impl NumberTermVar for MatchArm {
    fn number_term_var(
        self,
        cs: &VariableLevelContextStack<Identifier>,
        number: &mut usize,
    ) -> Self {
        Self {
            constructor: self.constructor,
            args: self.args,
            body: self.body.map(|t| t.number_term_var(cs, number)),
        }
    }
}
impl SubstitutableWithTerm for MatchArm {
    fn substitute_term(self, from: VariableRef, to_term: &Spanned<Term>) -> Self {
        Self {
            constructor: self.constructor,
            args: self.args,
            body: self.body.map(|t| t.substitute_term(from, to_term)),
        }
    }
}
impl SubstitutableWithType for MatchArm {
    fn substitute_type(self, from: &Identifier, to_typ: &Spanned<Type>) -> Self {
        Self {
            constructor: self.constructor,
            args: self.args,
            body: self.body.map(|t| t.substitute_type(from, to_typ)),
        }
    }
}

pub fn number_var<T: NumberTermVar>(t: T) -> T {
    let mut ctx = VariableLevelContext::default();
    let cs = ctx.make_stack();
    let mut number = 1;
    t.number_term_var(&cs, &mut number)
}
