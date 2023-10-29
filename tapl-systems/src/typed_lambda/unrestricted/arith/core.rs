use crate::common::ast::identifier::{QualifiedIdentifier, QualifiedIdentifierPart};
use crate::common::ast::variable::{LocalVariable, Variable};
use crate::common::context::{VariableLevelContext, VariableLevelContextStack};
use crate::common::diagnostic::{merge_diags, Diagnostic, DiagnosticLevel, Spanned};
use crate::common::evaluator::small_step::*;
use crate::format::{ctx_for_err, FormatContext, ToCode};
use crate::typed_lambda::unrestricted::meta::ast::{
    number_var, Context, ContextStack, GlobalTermDefMap, Identifier, Kind as MetaKind, Pattern,
    Source as MetaSource, SubstitutableWithTerm, SubstitutableWithType, Term as MetaTerm, TermBind,
    TermVarDefRef, Type as MetaType, TypeBind,
};
use crate::typed_lambda::unrestricted::meta::parser::{
    BuiltInTermsConfig, BuiltInTypesConfig, ParseResult, Parser as MetaParser, ParserConfig,
};
use num_bigint::BigUint;
use num_traits::CheckedSub;
use num_traits::{One, Zero};
use std::marker::PhantomData;

type VariableLevelContextArith = VariableLevelContext<Identifier>;
type VariableLevelContextStackArith<'a> = VariableLevelContextStack<'a, Identifier>;

pub struct Parser<'a>(MetaParser<'a>);
impl<'a> Parser<'a> {
    pub fn new(code: &'a str) -> Self {
        Self(MetaParser::new(code, Self::parse_config()))
    }
    pub fn start_parse_source(&mut self) -> ParseResult<MetaSource> {
        self.0.start_parse_source()
    }
    pub fn parse_config() -> ParserConfig {
        ParserConfig {
            built_in_terms: BuiltInTermsConfig {
                unit: true,
                bool_true: true,
                bool_false: true,
                succ: true,
                pred: true,
                is_zero: true,
            },
            built_in_types: BuiltInTypesConfig {
                unit: true,
                bool: true,
                nat: true,
                ..BuiltInTypesConfig::default()
            },
        }
    }
}
#[derive(Clone, Debug)]
pub struct Source(MetaSource);
impl Source {
    pub fn try_new(source: Spanned<MetaSource>) -> Result<Spanned<Self>, Vec<Diagnostic>> {
        let diags = Source::check(&source);
        if !diags.is_empty() {
            return Err(diags);
        }
        Ok(Spanned::new(source.span, Self(source.value)))
    }
    pub fn into_term(mut self) -> Result<(Spanned<Term>, Context), Vec<Diagnostic>> {
        self.0 = number_var(self.0);
        self.0.check_closure()?;
        let (term, global_ctx) = self.0.into_term();
        Ok((term.map(Term), global_ctx))
    }
    fn check(source: &Spanned<MetaSource>) -> Vec<Diagnostic> {
        let mut diags = Vec::new();
        for def in source.value.spanned_defs().iter().map(|def| def.value()) {
            for subterm in def.subterms_term() {
                merge_diags(&mut diags, Term::check(subterm));
            }
            for subterm in def.subterms_type() {
                merge_diags(&mut diags, Type::check(subterm));
            }
        }
        merge_diags(&mut diags, Term::check(source.value.spanned_term()));
        diags
    }
}
impl ToCode for Source {
    fn to_code(&self, fmt_ctx: &FormatContext) -> String {
        self.0.to_code(fmt_ctx)
    }
}

#[derive(Clone, Debug)]
pub struct Term(MetaTerm);
impl Term {
    pub fn try_new(term: Spanned<MetaTerm>) -> Result<Self, Vec<Diagnostic>> {
        let diags = Term::check(&term);
        if !diags.is_empty() {
            return Err(diags);
        }
        Ok(Self(term.value))
    }
    fn check(term: &Spanned<MetaTerm>) -> Vec<Diagnostic> {
        let mut diags = Vec::new();
        match term.value() {
            MetaTerm::Var(var) => {
                if var.is_qualified() {
                    diags.push(Diagnostic::new(
                        DiagnosticLevel::Error,
                        format!("unsupported variable, only unqualified non-dollar decorated variables are supported: {}", var.to_code(&ctx_for_err())),
                        term.span,
                    ))
                }
            }
            MetaTerm::Unit
            | MetaTerm::Nat(..)
            | MetaTerm::NatSucc
            | MetaTerm::NatPred
            | MetaTerm::NatIsZero
            | MetaTerm::Bool(..) => {}
            MetaTerm::AbsTerm { term_bind, body } => {
                merge_diags(&mut diags, Term::check(body));
                merge_diags(&mut diags, Term::check_term_bind(term_bind));
            }
            MetaTerm::ApplyTerm(t1, t2) => {
                merge_diags(&mut diags, Term::check(t1));
                merge_diags(&mut diags, Term::check(t2));
            }
            MetaTerm::BoolIf {
                cond,
                then_clause,
                else_clause,
            } => {
                merge_diags(&mut diags, Term::check(cond));
                merge_diags(&mut diags, Term::check(then_clause));
                merge_diags(&mut diags, Term::check(else_clause));
            }
            MetaTerm::Let { val, body, .. } => {
                merge_diags(&mut diags, Term::check(val));
                merge_diags(&mut diags, Term::check(body));
            }
            MetaTerm::Seq(t1, t2) => {
                merge_diags(&mut diags, Term::check(t1));
                merge_diags(&mut diags, Term::check(t2));
            }
            MetaTerm::Fix(t) => merge_diags(&mut diags, Term::check(t)),
            MetaTerm::AbsType { body, .. } => {
                // merge(&mut diags, Type::check_type_bind(type_bind));
                merge_diags(&mut diags, Term::check(body));
            }
            MetaTerm::ApplyType(t1, t2) => {
                merge_diags(&mut diags, Term::check(t1));
                merge_diags(&mut diags, Type::check(t2));
            }
            _ => diags.push(Diagnostic::new(
                DiagnosticLevel::Error,
                format!("unsupported term: {}", term.to_code(&ctx_for_err())),
                term.span,
            )),
        }
        diags
    }
    fn check_term_bind(term_bind: &Spanned<TermBind>) -> Vec<Diagnostic> {
        let mut diags = Vec::new();
        let TermBind { ref typ, .. } = term_bind.value();
        merge_diags(&mut diags, Type::check(typ));
        diags
    }

    pub fn into_inner(self) -> MetaTerm {
        self.0
    }
    pub fn inner(&self) -> &MetaTerm {
        &self.0
    }
}

#[derive(Clone, Debug)]
pub struct Type(MetaType);
impl Type {
    pub fn try_new(typ: Spanned<MetaType>) -> Result<Self, Vec<Diagnostic>> {
        let diags = Type::check(&typ);
        if !diags.is_empty() {
            return Err(diags);
        }
        Ok(Self(typ.value))
    }
    fn check(typ: &Spanned<MetaType>) -> Vec<Diagnostic> {
        let mut diags = Vec::new();
        for subterm in typ.subterms_term() {
            merge_diags(&mut diags, Term::check(subterm));
        }
        for subterm in typ.subterms_type() {
            merge_diags(&mut diags, Type::check(subterm));
        }
        match typ.value() {
            MetaType::Var(..) => {}
            MetaType::Unit | MetaType::Nat | MetaType::Bool => {}
            MetaType::Pi { term_bind, .. } => {
                let TermBind { pat: name, .. } = term_bind.as_ref().value();
                match name.value() {
                    Pattern::Named(..) => diags.push(Diagnostic::new(
                        DiagnosticLevel::Error,
                        "pi type with named term bind is not supported".to_string(),
                        term_bind.span,
                    )),
                    Pattern::Wildcard => {}
                }
            }
            MetaType::Forall { body, .. } => {
                // merge(&mut diags, Type::check_type_bind(type_bind));
                merge_diags(&mut diags, Type::check(body));
            }
            MetaType::AbsType { body, .. } => {
                // merge(&mut diags, Type::check_type_bind(type_bind));
                merge_diags(&mut diags, Type::check(body));
            }
            MetaType::ApplyType(typ1, typ2) => {
                merge_diags(&mut diags, Type::check(typ1));
                merge_diags(&mut diags, Type::check(typ2));
            }
            _ => diags.push(Diagnostic::new(
                DiagnosticLevel::Error,
                format!("unsupported type: {}", typ.to_code(&ctx_for_err())),
                typ.span,
            )),
        }
        diags
    }
    fn syntactic_eq(t1: &Spanned<MetaType>, t2: &Spanned<MetaType>) -> Result<(), Diagnostic> {
        let mut idc1 = VariableLevelContextArith::default();
        let mut idc2 = VariableLevelContextArith::default();
        let ids1 = idc1.make_stack();
        let ids2 = idc2.make_stack();
        Type::syntactic_eq_internal(t1, t2, &ids1, &ids2, 0)
    }
    fn syntactic_eq_internal<'a>(
        t1: &'a Spanned<MetaType>,
        t2: &'a Spanned<MetaType>,
        ids1: &VariableLevelContextStackArith,
        ids2: &VariableLevelContextStackArith,
        level: usize,
    ) -> Result<(), Diagnostic> {
        let (s1, s2, span1) = match (t1.value(), t2.value()) {
            (MetaType::Var(id1), MetaType::Var(id2)) => {
                match (ids1.get(id1.value()), ids2.get(id2.value())) {
                    (Some((_, l1)), Some((_, l2))) if l1 == l2 => return Ok(()),
                    (None, None) if id1.value() == id2.value() => return Ok(()),
                    (s1, s2) => {
                        let s1 = s1
                            .map(|(_, level)| format!(" ({})", level))
                            .unwrap_or_default();
                        let s2 = s2
                            .map(|(_, level)| format!(" ({})", level))
                            .unwrap_or_default();
                        let s1 = format!("{}{}", id1.to_string(), s1);
                        let s2 = format!("{}{}", id2.to_string(), s2);
                        (s1, s2, id1.span)
                    }
                }
            }
            (MetaType::Unit, MetaType::Unit)
            | (MetaType::Nat, MetaType::Nat)
            | (MetaType::Bool, MetaType::Bool) => return Ok(()),
            (MetaType::Pi { body: t1, .. }, MetaType::Pi { body: t2, .. }) => {
                return Self::syntactic_eq_internal(t1, t2, ids1, ids2, level)
            }
            (
                MetaType::Forall {
                    type_bind: b1,
                    body: t1,
                },
                MetaType::Forall {
                    type_bind: b2,
                    body: t2,
                },
            ) => {
                let _scope1 = b1
                    .spanned_identifier()
                    .map(|lvar| ids1.push(lvar.cloned(), level));
                let _scope2 = b2
                    .spanned_identifier()
                    .map(|lvar| ids2.push(lvar.cloned(), level));
                let eq = Self::syntactic_eq_internal(t1, t2, ids1, ids2, level + 1);
                return eq;
            }
            _ => (
                t1.to_code(&ctx_for_err()),
                t2.to_code(&ctx_for_err()),
                t1.span,
            ),
        };
        Err(Diagnostic::new(
            DiagnosticLevel::Error,
            format!("type mismatch: {} != {}", s1, s2),
            span1,
        ))
    }
    pub fn into_inner(self) -> MetaType {
        self.0
    }
    pub fn inner(&self) -> &MetaType {
        &self.0
    }
}

#[derive(Clone, Debug)]
pub struct KindChecker(PhantomData<()>);
impl KindChecker {
    fn kinding_internal(
        cs: &ContextStack,
        typ: &Spanned<MetaType>,
    ) -> Result<Spanned<MetaKind>, Vec<Diagnostic>> {
        match typ.value() {
            MetaType::Var(id) => cs.type_vars().get(id.value()).map_or_else(
                || {
                    Err(vec![Diagnostic::new(
                        DiagnosticLevel::Error,
                        format!("unbound type variable: {}", id.to_string()),
                        id.span,
                    )])
                },
                |(_, kind, _)| Ok(kind.clone()),
            ),
            MetaType::Unit | MetaType::Nat | MetaType::Bool => {
                Ok(Spanned::new(None, MetaKind::ProperTypes))
            }
            MetaType::Pi { term_bind, body } => {
                let TermBind { typ, .. } = term_bind.as_ref().value();
                let kind1 = Self::kinding_internal(cs, typ)?;
                let kind2 = Self::kinding_internal(cs, body)?;
                if Self::kind_syntactic_eq(&kind1, &MetaKind::ProperTypes)
                    && Self::kind_syntactic_eq(&kind2, &MetaKind::ProperTypes)
                {
                    Ok(Spanned::new(None, MetaKind::ProperTypes))
                } else {
                    Err(vec![
                        Diagnostic::new(
                        DiagnosticLevel::Error,
                        format!(
                            "in arrow type, expected proper types in both domain and codomain types, but got {} in domain",
                            kind1.to_code(&ctx_for_err())
                        ),
                        typ.span,
                    ),
                        Diagnostic::new(
                        DiagnosticLevel::Error,
                        format!(
                            "in arrow type, expected proper types in both domain and codomain types, but got {} in codomain",
                            kind2.to_code(&ctx_for_err())
                        ),
                        body.span,
                    ),
                    ])
                }
            }
            MetaType::AbsType { type_bind, body } => {
                let TypeBind { kind, .. } = type_bind.value();
                let _scope = type_bind
                    .spanned_identifier()
                    .map(|id| cs.push_type_var(id.cloned(), kind.as_ref().clone()));
                let kind2 = Self::kinding_internal(cs, body)?;
                Ok(Spanned::new(
                    None,
                    MetaKind::Arrow(kind.clone(), Box::new(kind2)),
                ))
            }
            MetaType::ApplyType(typ1, typ2) => {
                let kind1 = Self::kinding_internal(cs, typ1)?;
                let kind2 = Self::kinding_internal(cs, typ2)?;
                match kind1.value() {
                    MetaKind::Arrow(kind1_1, kind1_2) => {
                        if Self::kind_syntactic_eq(kind1_1, &kind2) {
                            Ok(kind1_2.as_ref().clone())
                        } else {
                            Err(vec![Diagnostic::new(
                                DiagnosticLevel::Error,
                                format!(
                                    "in application type, expected {} in the first argument, but got {}",
                                    kind1_1.to_code(&ctx_for_err()),
                                    kind2.to_code(&ctx_for_err()),
                                ),
                                typ2.span,
                            )])
                        }
                    }
                    _ => Err(vec![Diagnostic::new(
                        DiagnosticLevel::Error,
                        format!(
                            "in application type, expected arrow kinded type (.. => ..), but got {}:: {}",
                            typ1.to_code(&ctx_for_err()),
                            kind1.to_code(&ctx_for_err()),
                        ),
                        typ1.span,
                    )]),
                }
            }
            MetaType::Forall { type_bind, body } => {
                let _scope = type_bind
                    .spanned_identifier()
                    .map(|id| cs.push_type_var(id.cloned(), type_bind.spanned_kind().clone()));
                let kind = Self::kinding_internal(cs, body.as_ref())?;
                Ok(kind)
            }
            _ => unreachable!("unsupported type: {}", typ.to_code(&ctx_for_err())),
        }
    }
    fn kind_syntactic_eq(a: &MetaKind, b: &MetaKind) -> bool {
        match (a, b) {
            (MetaKind::Arrow(k1_1, k1_2), MetaKind::Arrow(k2_1, k2_2)) => {
                Self::kind_syntactic_eq(k1_1, k2_1.value())
                    && Self::kind_syntactic_eq(k1_2, k2_2.value())
            }
            (MetaKind::ProperTypes, MetaKind::ProperTypes) => true,
            _ => false,
        }
    }
}

pub struct TypeEvaluator<'a> {
    cs: &'a ContextStack<'a>,
}
impl TypeEvaluator<'_> {
    pub fn new<'a>(cs: &'a ContextStack) -> TypeEvaluator<'a> {
        TypeEvaluator { cs }
    }
    fn eval1(cs: &ContextStack, typ: &Spanned<MetaType>) -> Option<Spanned<MetaType>> {
        match typ.value() {
            MetaType::Var(id) => cs
                .type_vars()
                .get(id.value())
                .and_then(|(_, _, typ)| typ.clone()),
            MetaType::ApplyType(typ1, typ2) => {
                match typ1.value() {
                    MetaType::AbsType { type_bind, body } => Some(body.as_ref().clone().map(
                        |body| match type_bind.identifier() {
                            Some(id) => body.substitute_type(id, typ2),
                            None => body,
                        },
                    )),
                    _ => Self::eval1(cs, typ1).map(|typ1| {
                        Spanned::new(typ.span, MetaType::ApplyType(typ1.into(), typ2.clone()))
                    }),
                }
            }
            MetaType::Pi { term_bind, body } => Self::eval1(cs, body)
                .map(|body| {
                    Spanned::new(
                        typ.span,
                        MetaType::Pi {
                            term_bind: term_bind.clone(),
                            body: body.into(),
                        },
                    )
                })
                .or_else(|| {
                    Self::eval1(cs, term_bind.spanned_type()).map(|typ| {
                        Spanned::new(
                            typ.span,
                            MetaType::Pi {
                                term_bind: Spanned::new(
                                    term_bind.span,
                                    TermBind {
                                        pat: term_bind.pat.clone(),
                                        typ: typ.into(),
                                    },
                                )
                                .into(),
                                body: body.clone(),
                            },
                        )
                    })
                }),
            MetaType::Forall { type_bind, body } => {
                let _scope = type_bind
                    .spanned_identifier()
                    .map(|id| cs.push_type_var(id.cloned(), type_bind.spanned_kind().clone()));
                Self::eval1(cs, body).map(|body| {
                    Spanned::new(
                        typ.span,
                        MetaType::Forall {
                            type_bind: type_bind.clone(),
                            body: body.into(),
                        },
                    )
                })
            }
            _ => None,
        }
    }
}
impl SmallStepEvaluator for TypeEvaluator<'_> {
    type Stmt = Spanned<MetaType>;
    fn eval1(&self, stmt: &Self::Stmt) -> Option<Self::Stmt> {
        Self::eval1(self.cs, stmt)
    }
}

#[derive(Clone, Debug)]
pub struct TypeChecker;
impl TypeChecker {
    pub fn eval1_type(
        ctx: &mut Context,
        typ: &Spanned<Type>,
    ) -> Result<Option<Spanned<Type>>, Vec<Diagnostic>> {
        let cs = ctx.make_stack();
        let (_scope, diags) = cs.init(TypeChecker::typing_internal, KindChecker::kinding_internal);
        if !diags.is_empty() {
            return Err(diags);
        }
        let typ = typ.clone().map(|e| e.into_inner());
        let type_evaluator = TypeEvaluator { cs: &cs };
        Ok(type_evaluator.eval1(&typ).map(|typ| typ.map(Type)))
    }
    pub fn eval_type(
        ctx: &mut Context,
        typ: &Spanned<Type>,
    ) -> Result<Spanned<Type>, Vec<Diagnostic>> {
        let cs = ctx.make_stack();
        let (_scope, diags) = cs.init(TypeChecker::typing_internal, KindChecker::kinding_internal);
        if !diags.is_empty() {
            return Err(diags);
        }
        let typ = typ.clone().map(|e| e.into_inner());
        let type_evaluator = TypeEvaluator { cs: &cs };
        let typ = type_evaluator.eval_iter(&typ).last().unwrap();
        let typ = typ.into_owned().map(Type);
        Ok(typ)
    }

    // fn get_term_by_qid<'a>(
    //     cs: &'a ContextStack,
    //     qid: &Spanned<QualifiedIdentifier>,
    // ) -> Option<TermVarDefRef<'a>> {
    //     match qid.value() {
    //         QualifiedIdentifier::Head(p) => match p.value() {
    //             QualifiedIdentifierPart::Normal(id) => cs.get_term(id),
    //             QualifiedIdentifierPart::Special(_) => None,
    //         },
    //         _ => None,
    //     }
    // }
    fn get_term_by_qid<'a>(
        cs: &'a ContextStack,
        qid: &QualifiedIdentifier,
    ) -> Option<TermVarDefRef<'a>> {
        match qid {
            QualifiedIdentifier::Head(p) => match p.value() {
                QualifiedIdentifierPart::Normal(id) => cs
                    .get_term(&LocalVariable::new(id.clone(), Default::default()))
                    .or_else(|| cs.get_global_term(id)),
                QualifiedIdentifierPart::Special(_) => None,
            },
            QualifiedIdentifier::Sub(rest, p) => rest.value().head_special().and_then(|sp| {
                p.normal().and_then(|id| {
                    if sp.as_str() == "this" {
                        cs.get_global_term(id)
                    } else {
                        None
                    }
                })
            }),
        }
    }
    fn get_term_by_var<'a>(cs: &'a ContextStack, var: &Variable) -> Option<TermVarDefRef<'a>> {
        match var {
            Variable::Local(id) => cs.get_term(id),
            Variable::Qualified(qid) => Self::get_term_by_qid(cs, qid),
        }
    }

    pub fn typing(
        ctx: &mut Context,
        term: Spanned<Term>,
    ) -> Result<Spanned<Type>, Vec<Diagnostic>> {
        let cs = ctx.make_stack();
        let (_scope, diags) = cs.init(TypeChecker::typing_internal, KindChecker::kinding_internal);
        if !diags.is_empty() {
            return Err(diags);
        }
        Self::typing_internal(&cs, &term.map(|e| e.0)).map(|typ| typ.map(Type))
    }
    fn typing_internal(
        cs: &ContextStack,
        term: &Spanned<MetaTerm>,
    ) -> Result<Spanned<MetaType>, Vec<Diagnostic>> {
        match term.value() {
            MetaTerm::Unit => Ok(Spanned::new(None, MetaType::Unit)),
            MetaTerm::Nat(..) => Ok(Spanned::new(None, MetaType::Nat)),
            MetaTerm::Bool(..) => Ok(Spanned::new(None, MetaType::Bool)),
            MetaTerm::Var(var) => match Self::get_term_by_var(cs, var) {
                Some(r) => {
                    let (_, var_type, _) = &*r;
                    Ok(var_type.clone())
                }
                None => {
                    let diag = Diagnostic::new(
                        DiagnosticLevel::Error,
                        format!("unbound variable: {}", var.to_code(&ctx_for_err())),
                        term.span,
                    );
                    Err(vec![diag])
                }
            },
            // MetaTerm::Var(qid) => match cs.get_term(qid) {
            //     Some(r) => {
            //         let (_, var_type, _) = &*r;
            //         Ok(var_type.clone())
            //     }
            //     None => Err(vec![Diagnostic::new(
            //         DiagnosticLevel::Error,
            //         format!("unbound variable: {}", qid.to_code()),
            //         term.span,
            //     )]),
            // },
            MetaTerm::AbsTerm { term_bind, body } => {
                let term_bind_type = term_bind.typ.as_ref();
                let term_bind_type_kind = KindChecker::kinding_internal(cs, term_bind_type)?;
                if !KindChecker::kind_syntactic_eq(
                    term_bind_type_kind.value(),
                    &MetaKind::ProperTypes,
                ) {
                    return Err(vec![Diagnostic::new(
                        DiagnosticLevel::Error,
                        format!(
                            "expected a proper type, but got {}:: {}",
                            term_bind_type.to_code(&ctx_for_err()),
                            term_bind_type_kind.to_code(&ctx_for_err()),
                        ),
                        term_bind_type.span,
                    )]);
                }
                let _scope = term_bind
                    .spanned_local_variable()
                    .map(|lvar| cs.push_term_var(lvar.clone(), term_bind.spanned_type().clone()));
                Ok(Spanned::new(
                    None,
                    MetaType::new_arrow(
                        term_bind_type.clone(),
                        Self::typing_internal(cs, body.as_ref())?,
                    ),
                ))
            }
            MetaTerm::AbsType { type_bind, body } => {
                let _scope = type_bind
                    .spanned_identifier()
                    .map(|id| cs.push_type_var(id.cloned(), type_bind.spanned_kind().clone()));
                Ok(Spanned::new(
                    None,
                    MetaType::Forall {
                        type_bind: type_bind.clone(),
                        body: Self::typing_internal(cs, body.as_ref())?.into(),
                    },
                ))
            }
            MetaTerm::ApplyTerm(t1, t2) => {
                let type_evaluator = TypeEvaluator { cs };
                let t1_type = Self::typing_internal(cs, t1)?;
                let t1_type = type_evaluator.eval_iter(&t1_type).last().unwrap();
                match t1_type.into_owned().value {
                    MetaType::Pi { term_bind, body } => {
                        let term_bind_type = term_bind.spanned_type();
                        let term_bind_type =
                            type_evaluator.eval_iter(term_bind_type).last().unwrap();
                        let t2_type = Self::typing_internal(cs, t2)?;
                        let t2_type = type_evaluator.eval_iter(&t2_type).last().unwrap();
                        Type::syntactic_eq(&term_bind_type, &t2_type)
                            .map(|()| Ok(*body))
                            .map_err(|err| {
                                let err = err.map_message(|msg| {
                                    format!(
                                        "lambda bind type and argument type do not match: {}",
                                        msg,
                                    )
                                });
                                vec![err]
                            })?
                    }
                    t1_type => Err({
                        let diag = Diagnostic::new(
                            DiagnosticLevel::Error,
                            format!(
                                "expected pi type for apply callee, but found {}",
                                t1_type.to_code(&ctx_for_err()),
                            ),
                            term.span,
                        );
                        vec![diag]
                    }),
                }
            }
            MetaTerm::ApplyType(t, typ) => {
                let type_evaluator = TypeEvaluator { cs };
                let t_type = Self::typing_internal(cs, t)?;
                let t_type = type_evaluator.eval_iter(&t_type).last().unwrap();
                let typ_kind = KindChecker::kinding_internal(cs, typ.as_ref())?;
                match t_type.into_owned().value {
                    MetaType::Forall { type_bind, body } => {
                        if KindChecker::kind_syntactic_eq(type_bind.kind(), typ_kind.value()) {
                            Ok(Spanned::new(
                                None,
                                match type_bind.identifier() {
                                    Some(id) => {
                                        body.value().clone().substitute_type(id, typ.as_ref())
                                    }
                                    None => body.value().clone(),
                                },
                            ))
                        } else {
                            Err({
                                let diag = Diagnostic::new(
                                    DiagnosticLevel::Error,
                                    format!(
                                        "kind mismatch: expected {}, found {}",
                                        type_bind.kind().to_code(&ctx_for_err()),
                                        typ_kind.to_code(&ctx_for_err()),
                                    ),
                                    term.span,
                                );
                                vec![diag]
                            })
                        }
                    }
                    t_type => Err(vec![Diagnostic::new(
                        DiagnosticLevel::Error,
                        format!(
                            "expected forall type, found {}",
                            t_type.to_code(&ctx_for_err())
                        ),
                        term.span,
                    )]),
                }
            }
            MetaTerm::NatSucc | MetaTerm::NatPred => Ok(Spanned::new(
                None,
                MetaType::new_arrow(
                    Spanned::new(None, MetaType::Nat),
                    Spanned::new(None, MetaType::Nat),
                ),
            )),
            MetaTerm::NatIsZero => Ok(Spanned::new(
                None,
                MetaType::new_arrow(
                    Spanned::new(None, MetaType::Nat),
                    Spanned::new(None, MetaType::Bool),
                ),
            )),
            MetaTerm::Fix(t) => {
                // t must be typed as T -> T, and then fix t is T
                let t_type = Self::typing_internal(cs, t)?;
                match t_type.value {
                    MetaType::Pi { term_bind, body } => {
                        let type_evaluator = TypeEvaluator { cs };
                        let term_bind_type = term_bind.spanned_type();
                        let term_bind_type_evaluated = type_evaluator.eval_iter(term_bind_type).last().unwrap();
                        let body_type = body.as_ref();
                        let body_type_evaluated = type_evaluator.eval_iter(body_type).last().unwrap();
                        match Type::syntactic_eq(&term_bind_type_evaluated, &body_type_evaluated) {
                            Ok(()) => Ok(*body),
                            Err(err) => Err({
                                let fix_diag = Diagnostic::new(
                                    DiagnosticLevel::Error,
                                    format!(
                                        "fix operand must be typed as T -> T for some type T, but domain and codomian is not the same type and found value of {}",
                                        MetaType::Pi {
                                            term_bind,
                                            body: body.clone(),
                                        }.to_code(&ctx_for_err()),
                                    ),
                                    t.span,
                                );
                                vec![fix_diag, err]
                            }),
                        }
                    }
                    _ => Err(vec![Diagnostic::new(
                        DiagnosticLevel::Error,
                        format!(
                            "fix operand must be typed as T -> T for some type T, but found value of {}",
                            t_type.to_code(&ctx_for_err()),
                        ),
                        term.span,
                    )]),
                }
            }
            MetaTerm::BoolIf {
                cond,
                then_clause,
                else_clause,
            } => {
                let type_evaluator = TypeEvaluator { cs };
                let mut diags = Vec::new();
                let cond_type = Self::typing_internal(cs, cond)?;
                let cond_type = type_evaluator.eval_iter(&cond_type).last().unwrap();
                if Type::syntactic_eq(&cond_type, &Spanned::new(None, MetaType::Bool)).is_err() {
                    diags.push(Diagnostic::new(
                        DiagnosticLevel::Error,
                        format!(
                            "expected {}, but got {}",
                            MetaType::Bool.to_code(&ctx_for_err()),
                            cond_type.to_code(&ctx_for_err()),
                        ),
                        cond.span,
                    ));
                }
                let then_type = Self::typing_internal(cs, then_clause)?;
                let then_type_evaluated = type_evaluator.eval_iter(&then_type).last().unwrap();
                let else_type = Self::typing_internal(cs, else_clause)?;
                let else_type_evaluated = type_evaluator.eval_iter(&else_type).last().unwrap();
                if Type::syntactic_eq(&then_type_evaluated, &else_type_evaluated).is_err() {
                    diags.push(Diagnostic::new(
                        DiagnosticLevel::Error,
                        format!(
                            "then clause and else clause in if expression must have same type, but got {} and {}",
                            then_type.to_code(&ctx_for_err()),
                            else_type.to_code(&ctx_for_err()),
                        ),
                        else_clause.span,
                    ));
                }
                if diags.is_empty() {
                    Ok(then_type)
                } else {
                    Err(diags)
                }
            }
            MetaTerm::Let { pat, val, body } => {
                let val_type = Self::typing_internal(cs, val)?;
                // let _scope = cs.push_term_var(name.clone(), val_type);
                let _scope = pat
                    .spanned_local_variable()
                    .map(|lvar| cs.push_term_var(lvar.clone(), val_type));
                let body_type = Self::typing_internal(cs, body)?;
                Ok(body_type)
            }
            MetaTerm::Seq(t1, t2) => {
                let type_evaluator = TypeEvaluator { cs };
                let t1_type = Self::typing_internal(cs, t1)?;
                let t2_type = Self::typing_internal(cs, t2)?;
                let t1_type_evaluated = type_evaluator.eval_iter(&t1_type).last().unwrap();
                if Type::syntactic_eq(&t1_type_evaluated, &Spanned::new(None, MetaType::Unit))
                    .is_err()
                {
                    Err(vec![Diagnostic::new(
                        DiagnosticLevel::Error,
                        format!(
                            "expected {}, but got {}",
                            MetaType::Unit.to_code(&ctx_for_err()),
                            t1_type.to_code(&ctx_for_err()),
                        ),
                        t1.span,
                    )])
                } else {
                    Ok(t2_type)
                }
            }
            _ => unreachable!("unsupported term: {:?}", term),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Evaluator {
    defs: GlobalTermDefMap,
}
impl Evaluator {
    pub fn new(defs: GlobalTermDefMap) -> Self {
        Self { defs }
    }
    fn get_term_by_qid<'a>(
        defs: &GlobalTermDefMap,
        qid: &QualifiedIdentifier,
    ) -> Option<Spanned<MetaTerm>> {
        match qid {
            QualifiedIdentifier::Head(p) => match p.value() {
                QualifiedIdentifierPart::Normal(id) => defs.get(id).map(|(_, term)| term.clone()),
                QualifiedIdentifierPart::Special(_) => None,
            },
            QualifiedIdentifier::Sub(rest, p) => rest.value().head_special().and_then(|sp| {
                p.normal().and_then(|id| {
                    if sp.as_str() == "this" {
                        defs.get(id).map(|(_, term)| term.clone())
                    } else {
                        None
                    }
                })
            }),
        }
    }
    fn get_term_by_var<'a>(defs: &GlobalTermDefMap, var: &Variable) -> Option<Spanned<MetaTerm>> {
        match var {
            Variable::Local(lvar) => defs.get(lvar.id()).map(|(_, term)| term.clone()),
            Variable::Qualified(qid) => Self::get_term_by_qid(defs, qid),
        }
    }

    pub fn is_value_meta_term(term: &MetaTerm) -> bool {
        matches!(
            term,
            MetaTerm::Unit
                | MetaTerm::Nat(..)
                | MetaTerm::Bool(..)
                | MetaTerm::NatSucc
                | MetaTerm::NatPred
                | MetaTerm::NatIsZero
                | MetaTerm::AbsTerm { .. }
                | MetaTerm::AbsType { .. }
        )
    }
    fn eval1(defs: &GlobalTermDefMap, term: &Term) -> Option<Spanned<Term>> {
        let term = Self::eval1_meta_term(defs, &term.0);
        term.map(|e| e.map(Term))
    }
    fn eval1_meta_term(defs: &GlobalTermDefMap, term: &MetaTerm) -> Option<Spanned<MetaTerm>> {
        match term {
            MetaTerm::Var(var) => {
                // let head = var.head_normal().unwrap_or_else(|| {
                //     unreachable!(
                //         "unbound supported variable: {}",
                //         var.to_code(&ctx_for_err())
                //     )
                // });
                //
                // defs.get(head.value()).map(|(_, term)| term.clone())
                Self::get_term_by_var(defs, var)
            }
            MetaTerm::ApplyTerm(t1, t2) => match t1.value() {
                MetaTerm::AbsTerm { term_bind, body } => {
                    let body = body.value();
                    term_bind
                        .pat
                        .local_variable()
                        .map_or_else(
                            || t2.as_ref().clone(),
                            |lvar| {
                                Spanned::new(
                                    None,
                                    body.clone().substitute_term(lvar.into(), t2.as_ref()),
                                )
                            },
                        )
                        .into()
                }
                MetaTerm::NatSucc => Self::eval1_meta_term(defs, t2.as_ref())
                    .map(|t2| Spanned::new(None, MetaTerm::ApplyTerm(t1.clone(), t2.into())))
                    .or_else(|| {
                        t2.as_ref()
                            .as_ref()
                            .nat()
                            .map(|n| Spanned::new(None, MetaTerm::Nat(n + BigUint::one())))
                    }),
                MetaTerm::NatPred => Self::eval1_meta_term(defs, t2.as_ref())
                    .map(|t2| Spanned::new(None, MetaTerm::ApplyTerm(t1.clone(), t2.into())))
                    .or_else(|| {
                        t2.nat().map(|n| {
                            Spanned::new(
                                None,
                                MetaTerm::Nat(
                                    n.checked_sub(&BigUint::one()).unwrap_or_else(BigUint::zero),
                                ),
                            )
                        })
                    }),
                MetaTerm::NatIsZero => {
                    if Self::is_value_meta_term(t2.as_ref()) {
                        t2.nat()
                            .map(|n| Spanned::new(None, MetaTerm::Bool(n.is_zero())))
                    } else {
                        Self::eval1_meta_term(defs, t2.as_ref()).map(|t2| {
                            Spanned::new(None, MetaTerm::ApplyTerm(t1.clone(), t2.into()))
                        })
                    }
                }
                _ => Self::eval1_meta_term(defs, t1)
                    .map(|t1| Spanned::new(None, MetaTerm::ApplyTerm(t1.into(), t2.clone()))),
            },
            MetaTerm::BoolIf {
                cond,
                then_clause,
                else_clause,
            } => match cond.value() {
                MetaTerm::Bool(true) => then_clause.as_ref().clone().into(),
                MetaTerm::Bool(false) => else_clause.as_ref().clone().into(),
                cond if !Self::is_value_meta_term(cond) => {
                    Self::eval1_meta_term(defs, cond).map(|cond| {
                        Spanned::new(
                            None,
                            MetaTerm::BoolIf {
                                cond: cond.into(),
                                then_clause: then_clause.clone(),
                                else_clause: else_clause.clone(),
                            },
                        )
                    })
                }
                _ => None,
            },
            MetaTerm::Let { pat, val, body, .. } => {
                if Self::is_value_meta_term(val.value()) {
                    let body = body.value();
                    Spanned::new(
                        None,
                        match pat.local_variable() {
                            Some(lvar) => body.clone().substitute_term(lvar.into(), val.as_ref()),
                            None => body.clone(),
                        },
                    )
                    .into()
                } else {
                    Self::eval1_meta_term(defs, val.as_ref()).map(|val| {
                        Spanned::new(
                            None,
                            MetaTerm::Let {
                                pat: pat.clone(),
                                val: val.into(),
                                body: body.clone(),
                            },
                        )
                    })
                }
            }
            MetaTerm::Fix(t) => {
                if Self::is_value_meta_term(t.as_ref()) {
                    match t.value() {
                        MetaTerm::AbsTerm { term_bind, body } => {
                            let body = body.value();
                            term_bind
                                .pat
                                .local_variable()
                                .map_or_else(
                                    || t.as_ref().clone(),
                                    |lvar| {
                                        Spanned::new(
                                            None,
                                            body.clone().substitute_term(
                                                lvar.into(),
                                                &Spanned::new(None, MetaTerm::Fix(t.clone())),
                                            ),
                                        )
                                    },
                                )
                                .into()
                        }
                        _ => None,
                    }
                } else {
                    Self::eval1_meta_term(defs, t.as_ref())
                        .map(|t| Spanned::new(None, MetaTerm::Fix(t.into())))
                }
            }
            MetaTerm::Seq(t1, t2) => {
                if Self::is_value_meta_term(t1.as_ref()) {
                    t2.as_ref().clone().into()
                } else {
                    Self::eval1_meta_term(defs, t1.as_ref())
                        .map(|t1| Spanned::new(None, MetaTerm::Seq(t1.into(), t2.clone())))
                }
            }
            MetaTerm::ApplyType(t, typ) => {
                if Self::is_value_meta_term(t.as_ref()) {
                    match t.value() {
                        MetaTerm::AbsType { type_bind, body } => {
                            let body = body.value();
                            type_bind
                                .identifier()
                                .map_or_else(
                                    || t.as_ref().clone(),
                                    |id| {
                                        Spanned::new(
                                            None,
                                            body.clone().substitute_type(id, typ.as_ref()),
                                        )
                                    },
                                )
                                .into()
                        }
                        _ => None,
                    }
                } else {
                    Self::eval1_meta_term(defs, t.as_ref())
                        .map(|t| Spanned::new(None, MetaTerm::ApplyType(t.into(), typ.clone())))
                }
            }
            _ => None,
        }
    }
}
impl SmallStepEvaluator for Evaluator {
    type Stmt = Spanned<Term>;
    fn eval1(&self, stmt: &Self::Stmt) -> Option<Self::Stmt> {
        Self::eval1(&self.defs, stmt)
    }
}

#[cfg(test)]
mod tests {
    use crate::common::diagnostic::{Spanned, SpannedWithDiagnostics};

    use super::*;

    fn simple(code: &str, expected_typ: &str, expected_term: &str) {
        println!("====================");
        println!("code: {}", code);
        let SpannedWithDiagnostics {
            value: source,
            span,
            diagnostics,
        } = Parser::new(code)
            .start_parse_source()
            .into_result()
            .unwrap_or_else(|e| {
                e.diagnostics.iter().for_each(|d| println!("{}", d));
                panic!("parse error");
            });
        if !diagnostics.is_empty() {
            diagnostics.iter().for_each(|d| println!("{}", d));
            panic!("parse error");
        }
        let source = Source::try_new(Spanned::new(span, source)).unwrap_or_else(|e| {
            e.iter().for_each(|d| println!("{}", d));
            panic!("Source parse error");
        });
        println!("source: {}", source.to_code(&ctx_for_err()));
        let Spanned {
            value: (term, mut ctx),
            ..
        } = source.map(|e| {
            e.into_term().unwrap_or_else(|e| {
                e.iter().for_each(|d| println!("{}", d));
                panic!("term parse error");
            })
        });
        let e = Evaluator::new(ctx.term_def_map());
        let typ = TypeChecker::typing(&mut ctx, term.clone()).unwrap_or_else(|e| {
            e.iter().for_each(|d| println!("{}", d));
            panic!("typing error");
        });
        let typ = TypeChecker::eval_type(&mut ctx, &typ).unwrap_or_else(|e| {
            e.iter().for_each(|d| println!("{}", d));
            panic!("type eval error");
        });
        assert_eq!(
            typ.value()
                .clone()
                .into_inner()
                .to_code(&FormatContext::new_compact()),
            expected_typ,
            "expected {} : {}",
            code,
            expected_typ
        );
        if expected_term != "SKIP" {
            let term = e
                .eval_iter(&term)
                .map_then_eval_with_limit(100, |e| {
                    e.map(|e| {
                        println!("STEP: {}", e.value.0.to_code(&FormatContext::new_compact()));
                        e
                    })
                })
                .expect("evaluation limit exceeded");
            assert_eq!(
                term.value()
                    .clone()
                    .into_inner()
                    .to_code(&FormatContext::new_compact()),
                expected_term,
                "expected {} ->* {}",
                code,
                expected_term,
            );
            let typ2 = TypeChecker::typing(&mut ctx, term.clone()).unwrap_or_else(|e| {
                e.iter().for_each(|d| println!("{}", d));
                panic!("typing error");
            });
            let typ2 = TypeChecker::eval_type(&mut ctx, &typ2).unwrap_or_else(|e| {
                e.iter().for_each(|d| println!("{}", d));
                panic!("type eval error");
            });
            assert_eq!(
                typ2.value
                    .into_inner()
                    .to_code(&FormatContext::new_compact()),
                expected_typ,
                "{:?} : {:?}",
                term.value()
                    .clone()
                    .into_inner()
                    .to_code(&FormatContext::new_compact()),
                expected_typ
            );
        }
    }

    fn error(code: &str, expected_error_step: &str, expected_error: Vec<&str>) {
        error_main(code).map_or_else(
            |(error_step, diagnostics)| {
                assert_eq!(error_step, expected_error_step);
                if expected_error != vec!["SKIP"] {
                    let got = diagnostics
                        .iter()
                        .map(|d| d.message().as_str())
                        .collect::<Vec<&str>>();
                    assert_eq!(got, expected_error,);
                }
            },
            |_| panic!("expected error"),
        );
    }

    fn error_main(code: &str) -> Result<(), (&'static str, Vec<Diagnostic>)> {
        println!("====================");
        println!("code: {}", code);
        let SpannedWithDiagnostics {
            value: source,
            span,
            diagnostics,
        } = Parser::new(code)
            .start_parse_source()
            .into_result()
            .unwrap_or_else(|e| {
                e.diagnostics.iter().for_each(|d| println!("{}", d));
                panic!("parse error");
            });
        if !diagnostics.is_empty() {
            diagnostics.iter().for_each(|d| println!("{}", d));
            panic!("parse error");
        }
        let source = Source::try_new(Spanned::new(span, source)).map_err(|e| ("SYNTAX", e))?;
        println!("source: {}", source.to_code(&ctx_for_err()));
        let (term, mut ctx) = source.value.into_term().map_err(|e| ("INTO_TERM", e))?;
        let typ = TypeChecker::typing(&mut ctx, term).map_err(|e| ("TYPING", e))?;
        TypeChecker::eval_type(&mut ctx, &typ).map_err(|e| ("TYPE_EVAL", e))?;
        Ok(())
    }

    #[test]
    fn test_unit() {
        simple("unit", "Unit", "unit");
    }

    #[test]
    fn test_nat() {
        simple("0", "Nat", "0");
        simple("1", "Nat", "1");
        simple("2", "Nat", "2");
        simple("123456789", "Nat", "123456789");
        simple("0xabcdef", "Nat", "11259375");
        simple("0b101010", "Nat", "42");
        simple("0o1234567", "Nat", "342391");
    }

    #[test]
    fn test_nat_succ() {
        simple("succ 0", "Nat", "1");
        simple("succ 1", "Nat", "2");
        simple("succ 2", "Nat", "3");
        simple("succ 123456789", "Nat", "123456790");
    }
    #[test]
    fn test_nat_pred() {
        simple("pred 0", "Nat", "0");
        simple("pred 1", "Nat", "0");
        simple("pred 2", "Nat", "1");
        simple("pred 123456789", "Nat", "123456788");
    }
    #[test]
    fn test_nat_is_zero() {
        simple("is_zero 0", "Bool", "true");
        simple("is_zero 1", "Bool", "false");
        simple("is_zero 2", "Bool", "false");
    }

    #[test]
    fn test_bool() {
        simple("true", "Bool", "true");
        simple("false", "Bool", "false");
    }

    #[test]
    fn test_if() {
        simple("if true { 1 } else { 2 }", "Nat", "1");
        simple("if false { 1 } else { 2 }", "Nat", "2");
        simple("if true { true } else { false }", "Bool", "true");
        simple("if false { true } else { false }", "Bool", "false");
        simple("if true { unit } else { unit }", "Unit", "unit");
    }

    #[test]
    fn test_if_2() {
        simple(
            "if true { if true { true } else { false } } else { false }",
            "Bool",
            "true",
        );
        simple(
            "if true { 1 } else { if true { 2 } else { 3 } }",
            "Nat",
            "1",
        );
        simple(
            "if if true { true } else { false } { true } else { false }",
            "Bool",
            "true",
        );
    }

    #[test]
    fn test_abs_1() {
        simple(r"\x: Nat. x", "Nat -> Nat", r"\x: Nat. x");
        simple(
            r"\f: Nat -> Nat. f",
            r"(Nat -> Nat) -> Nat -> Nat",
            r"\f: (Nat -> Nat). f",
        );
        simple(
            r"\x: Nat. \y: Nat. x",
            r"Nat -> Nat -> Nat",
            r"\x: Nat. \y: Nat. x",
        );
        simple(
            r"\x: Nat. \f: Nat -> Nat. (f(x))",
            r"Nat -> (Nat -> Nat) -> Nat",
            r"\x: Nat. \f: (Nat -> Nat). f x",
        );
    }
    #[test]
    fn test_abs_2() {
        simple(
            r#"
            def id_nat = \x: Nat. x;
            def app = \x: Nat. \f: Nat -> Nat. (f(x));
            def x = 1;
            app x id_nat
            "#,
            "Nat",
            "1",
        );
    }

    #[test]
    fn test_let() {
        simple(
            r#"
            let id_nat = \x: Nat. x;
            let app = \x: Nat. \f: Nat -> Nat. (f(x));
            let x = 1;
            app x id_nat
            "#,
            "Nat",
            "1",
        );
    }

    #[test]
    fn test_seq() {
        simple(
            r#"
            def! print = \_:Nat. unit;
            let id_nat = \x: Nat. (print(x); x);
            let app = \x: Nat. \f: Nat -> Nat. (
                let v = f(x);
                print(v);
                v
            );
            let x = 1;
            app(x)(id_nat)
            "#,
            "Nat",
            "1",
        );
    }

    #[test]
    fn test_fix() {
        simple(
            r#"
            let add = fix (\self: Nat -> Nat -> Nat. \x: Nat. \y: Nat. (
                if is_zero(y) {
                    x
                } else {
                    self(succ(x))(pred(y))
                }
            ));
            add 2 3
            "#,
            "Nat",
            "5",
        );
    }

    #[test]
    fn test_forall_id() {
        simple(
            r#"
            let id = \X:: *. \x: X. x;
            id[Nat] 1
            "#,
            "Nat",
            "1",
        );
    }

    #[test]
    fn test_forall_id_typing() {
        simple(
            r#"
            let id = \X:: *. \x: X. x;
            id
            "#,
            "forall X:: *. X -> X",
            r"\X:: *. \x: X. x",
        );
    }

    #[test]
    fn test_forall_app_id_1() {
        simple(
            r#"
            let id = \X:: *. \x: X. x;
            let app = \Y:: *. \f: forall X:: *. X -> X. \x: Y. f[Y] x;
            app[Nat] id 1
            "#,
            "Nat",
            "1",
        );
    }
    #[test]
    fn test_forall_app_id_2() {
        simple(
            r#"
            let id = \X:: *. \x: X. x;
            let app = \X:: *. \f: forall X:: *. X -> X. \x: X. f[X] x;
            app[Nat] id 1
            "#,
            "Nat",
            "1",
        );
    }
    #[test]
    fn test_forall_app_id_3() {
        simple(
            r#"
            let id = \X:: *. \x: X. x;
            let app2 = \f: forall X:: *. X -> X. \X:: *. \x: X. f[X] x;
            (app2 id)[Nat] 1
            "#,
            "Nat",
            "1",
        );
    }

    #[test]
    fn test_forall_composite_double() {
        simple(
            r#"
            let composite = \X:: *. \Y:: *. \Z:: *. \f: X -> Y. \g: Y -> Z. \x: X. g(f(x));
            let double = \X:: *. \f: X -> X. \x: X. composite[X][X][X] f f x;
            double[Nat] (composite[Nat][Nat][Nat] succ succ) 1
            "#,
            "Nat",
            "5",
        );
    }
    #[test]
    fn test_forall_composite_double_typing_1() {
        simple(
            r#"
            let composite = \X:: *. \Y:: *. \Z:: *. \f: X -> Y. \g: Y -> Z. \x: X. g(f(x));
            let double = \X:: *. \f: X -> X. \x: X. composite[X][X][X] f f x;
            double
            "#,
            "forall X:: *. (X -> X) -> X -> X",
            "SKIP",
        );
    }
    #[test]
    fn test_forall_composite_double_typing_2() {
        simple(
            r#"
            let composite = \X:: *. \Y:: *. \Z:: *. \f: X -> Y. \g: Y -> Z. \x: X. g(f(x));
            composite
            "#,
            "forall X:: *. forall Y:: *. forall Z:: *. (X -> Y) -> (Y -> Z) -> X -> Z",
            "SKIP",
        );
    }

    #[test]
    fn test_propositional_calculus_1() {
        simple(
            r#"
            let then1 = \P:: *. \Q:: *. \p: P. \q: Q. p;
            then1
            "#,
            "forall P:: *. forall Q:: *. P -> Q -> P",
            "SKIP",
        );
    }
    #[test]
    fn test_propositional_calculus_2() {
        simple(
            r#"
            let then2 = \P:: *. \Q:: *. \R:: *. \f: P -> Q -> R. \g: P -> Q. \p: P. f p (g p);
            then2
            "#,
            "forall P:: *. forall Q:: *. forall R:: *. (P -> Q -> R) -> (P -> Q) -> P -> R",
            "SKIP",
        );
    }

    #[test]
    fn test_forall_higher_order_ski() {
        simple(
            r#"
            // (* => * => *) => (* => *) => * => *
            type S = \X:: * => * => *. \Y:: * => *. \Z:: *. X[Z][Y[Z]];
            // * => * => *
            type K = \X:: *. \Y:: *. X;
            // * => *
            type I = \X:: *. X;
            type Id = S[K][I];
            type Nat2 = Id[Nat];
            (\x: Nat2. x) 1
            "#,
            "Nat",
            "1",
        );
    }
    #[test]
    fn test_forall_higher_order() {
        simple(
            r#"
            // (* => * => *) => (* => *) => * => *
            type S = \X:: * => * => *. \Y:: * => *. \Z:: *. X[Z][Y[Z]];
            // * => * => *
            type K = \X:: *. \Y:: *. X;
            // * => *
            type I = \X:: *. X;
            type Nat2 = S[K][I][Nat];
            type Id2 = \X:: * => *. X;
            type App = \X:: * => *. X[Nat];
            type IdFunc = \X:: *. X -> X;
            let id = \X:: *. \x: X. x;
            let app2 = \f: forall X:: *. IdFunc[X]. \X:: *. \x: X. f[X] x;
            (app2 id)[Nat] 1
            "#,
            "Nat",
            "1",
        );
    }
    #[test]
    fn test_higher_order_1() {
        simple(
            r#"
            type I = \X:: *. X;
            \x: I[Nat] -> I[I[Nat]]. x 0
            "#,
            "(Nat -> Nat) -> Nat",
            r"\x: (I[Nat] -> I[I[Nat]]). x 0",
        );
    }

    #[test]
    fn test_higher_order_fix_1() {
        simple(
            r#"
            type I = \X:: *. X;
            fix ((\f: Nat -> I[Nat]. f) (\x: Nat. x))
            "#,
            "Nat",
            "SKIP",
        );
    }
    #[test]
    fn test_higher_order_fix_2() {
        simple(
            r#"
            type I = \X:: *. X;
            fix ((\f: I[Nat] -> Nat. f) (\x: Nat. x))
            "#,
            "Nat",
            "SKIP",
        );
    }
    #[test]
    fn test_higher_order_fix_3() {
        simple(
            r#"
            type I = \X:: *. X;
            type I2 = \X:: * => *. X;
            fix ((\f: I[I2[I2[I]][I[Nat]]] -> Nat. f) (\x: I2[I][Nat]. x))
            "#,
            "Nat",
            "SKIP",
        );
    }
    #[test]
    fn test_higher_order_fix_4() {
        simple(
            r#"
            def diverge = \X:: *. fix (\x: X. x);
            type I = \X:: *. X;
            fix ((\f: I[Nat] -> I[I[I[Nat]]]. f) (\x: I[I[Nat]]. x))
            "#,
            "Nat",
            "SKIP",
        );
    }
    #[test]
    fn test_higher_order_fix_5() {
        simple(
            r#"
            def diverge = \X:: *. fix (\x: X. x);
            type I = \X:: *. X;
            fix diverge[I[Nat] -> Nat]
            "#,
            "Nat",
            "SKIP",
        );
    }

    #[test]
    fn test_higher_order_seq_1() {
        simple(
            r#"
            def diverge = \X:: *. fix (\x: X. x);
            type I = \X:: *. X;
            diverge[Unit];
            diverge[I[Unit]];
            0
            "#,
            "Nat",
            "SKIP",
        );
    }

    #[test]
    fn test_error_lambda() {
        error(
            r#"
            (\x: Nat. x) true
            "#,
            "TYPING",
            vec!["lambda bind type and argument type do not match: type mismatch: Nat != Bool"],
        );
    }

    #[test]
    fn test_error_fix_1() {
        error(
            r#"
            fix 0
            "#,
            "TYPING",
            vec!["fix operand must be typed as T -> T for some type T, but found value of Nat"],
        );
    }

    #[test]
    fn test_error_fix_2() {
        error(
            r#"
            fix (\x: Nat. unit)
            "#,
            "TYPING",
            vec!["fix operand must be typed as T -> T for some type T, but domain and codomian is not the same type and found value of Nat -> Unit", "type mismatch: Nat != Unit"],
        );
    }

    #[test]
    fn test_error_higher_order_1() {
        error(
            r#"
            type X = \X:: *. X;
            type Y = \X:: *. X;
            type Z = X[Y];
            0
            "#,
            "TYPING",
            vec!["in application type, expected * in the first argument, but got * => *"],
        );
    }

    #[test]
    fn test_error_higher_order_2() {
        error(
            r#"
            type X = \X:: *. X;
            type Y = \X:: *. X;
            (\x: X[Y][Nat]. x) 0
            "#,
            "TYPING",
            vec!["in application type, expected * in the first argument, but got * => *"],
        );
    }

    #[test]
    fn test_error_higher_order_3() {
        error(
            r#"
            type X = \X:: *. X;
            (\x: X. x) 0
            "#,
            "TYPING",
            vec!["expected a proper type, but got X:: * => *"],
        );
    }

    #[test]
    fn test_error_higher_order_4() {
        error(
            r#"
            type X = \X:: *. X[X];
            0
            "#,
            "TYPING",
            vec!["in application type, expected arrow kinded type (.. => ..), but got X:: *"],
        );
    }

    #[test]
    fn test_error_higher_order_5() {
        error(
            r#"
            type X = \X:: * => *. X;
            type Y = X[Nat];
            0
            "#,
            "TYPING",
            vec!["in application type, expected * => * in the first argument, but got *"],
        );
    }

    #[test]
    fn test_higher_order_if_1() {
        simple(
            r#"
            def diverge = \X:: *. fix (\x: X. x);
            type I = \X:: *. X;
            if diverge[I[Bool]] { 0 } else { 0 }
            "#,
            "Nat",
            "SKIP",
        );
    }
    #[test]
    fn test_higher_order_if_2() {
        simple(
            r#"
            def diverge = \X:: *. fix (\x: X. x);
            type I = \X:: *. X;
            if true { diverge[I[I[Nat]]] } else { diverge[I[Nat]] }
            "#,
            "Nat",
            "SKIP",
        );
    }
}
