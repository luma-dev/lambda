// Calculus of Constructions

use crate::common::ast::identifier::{QualifiedIdentifier, QualifiedIdentifierPart};
use crate::common::ast::variable::{LocalVariable, Variable};
use crate::common::context::{VariableLevelContext, VariableLevelContextStack};
use crate::common::diagnostic::{merge_diags, merge_span, Diagnostic, DiagnosticLevel, Spanned};
use crate::format::{ctx_for_err, FormatContext, ToCode};
use crate::typed_lambda::unrestricted::meta::ast::{
    number_var, Context, ContextStack, Kind as MetaKind, Pattern, Source as MetaSource,
    SubstitutableWithTerm, Term as MetaTerm, TermBind, TermVarDefRef, Type as MetaType,
};
use crate::typed_lambda::unrestricted::meta::parser::{
    BuiltInTypesConfig, ParseResult, Parser as MetaParser, ParserConfig,
};

type VariableLevelContextCc = VariableLevelContext<LocalVariable>;
type VariableLevelContextStackCc<'a> = VariableLevelContextStack<'a, LocalVariable>;

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
            built_in_types: BuiltInTypesConfig {
                prop: true,
                prf: true,
                ..BuiltInTypesConfig::default()
            },
            ..ParserConfig::default()
        }
    }
}

#[derive(Clone, Debug)]
pub struct Source(MetaSource);
impl Source {
    pub fn try_new(source: Spanned<MetaSource>) -> Result<Spanned<Self>, Vec<Diagnostic>> {
        let diags = Self::check(&source);
        if !diags.is_empty() {
            return Err(diags);
        }
        Ok(Spanned::new(source.span, Self(source.value)))
    }
    fn check(source: &Spanned<MetaSource>) -> Vec<Diagnostic> {
        let mut diags = Vec::new();
        for def in source.value.spanned_defs().iter().map(|def| def.value()) {
            for subterm in def.subterms_term() {
                merge_diags(&mut diags, Self::check_term(subterm));
            }
            for subterm in def.subterms_type() {
                merge_diags(&mut diags, Self::check_type(subterm));
            }
        }
        merge_diags(&mut diags, Self::check_term(source.value.spanned_term()));
        diags
    }
    fn check_term(term: &Spanned<MetaTerm>) -> Vec<Diagnostic> {
        let mut diags = Vec::new();
        match term.value() {
            MetaTerm::Var(..) => {}
            MetaTerm::AbsTerm { term_bind, body } => {
                merge_diags(&mut diags, Self::check_term(body));
                merge_diags(&mut diags, Self::check_term_bind(term_bind));
            }
            MetaTerm::ApplyTerm(t1, t2) => {
                merge_diags(&mut diags, Self::check_term(t1));
                merge_diags(&mut diags, Self::check_term(t2));
            }
            // MetaTerm::Let { val, body, .. } => {
            //     merge_diags(&mut diags, Self::check_term(val));
            //     merge_diags(&mut diags, Self::check_term(body));
            // }
            MetaTerm::All { term_bind, body } => {
                merge_diags(&mut diags, Self::check_term_bind(term_bind));
                merge_diags(&mut diags, Self::check_term(body));
            }
            _ => diags.push(Diagnostic::new(
                DiagnosticLevel::Error,
                format!(
                    "Term of {:?} is not supported for this system: {}",
                    term.term_kind_name(),
                    term.to_code(&ctx_for_err())
                ),
                term.span,
            )),
        }
        diags
    }
    fn check_term_bind(term_bind: &Spanned<TermBind>) -> Vec<Diagnostic> {
        let mut diags = Vec::new();
        merge_diags(&mut diags, Self::check_type(term_bind.spanned_type()));
        diags
    }
    fn check_type(typ: &Spanned<MetaType>) -> Vec<Diagnostic> {
        let mut diags = Vec::new();
        match typ.value() {
            MetaType::Var(..) => {}
            MetaType::ApplyTerm(t1, t2) => {
                merge_diags(&mut diags, Self::check_type(t1));
                merge_diags(&mut diags, Self::check_term(t2));
            }
            MetaType::Pi { term_bind, body } => {
                merge_diags(&mut diags, Self::check_term_bind(term_bind));
                merge_diags(&mut diags, Self::check_type(body));
            }
            MetaType::Prop => {}
            MetaType::Prf => {}
            _ => diags.push(Diagnostic::new(
                DiagnosticLevel::Error,
                format!(
                    "Type of {:?} is not supported for this system: {}",
                    typ.type_kind_name(),
                    typ.to_code(&FormatContext::new_compact())
                ),
                typ.span,
            )),
        }
        diags
    }
    pub fn into_term(mut self) -> Result<(Spanned<Term>, Context), Vec<Diagnostic>> {
        self.0 = number_var(self.0);
        self.0.check_closure()?;
        let (term, global_ctx) = self.0.into_term();
        Ok((term.map(Term), global_ctx))
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
        let diags = Source::check_term(&term);
        if !diags.is_empty() {
            return Err(diags);
        }
        Ok(Self(term.value))
    }

    pub fn into_inner(self) -> MetaTerm {
        self.0
    }
    pub fn inner(&self) -> &MetaTerm {
        &self.0
    }
}
impl ToCode for Term {
    fn to_code(&self, fmt_ctx: &FormatContext) -> String {
        self.0.to_code(fmt_ctx)
    }
}

#[derive(Clone, Debug)]
pub struct Type(MetaType);
impl Type {
    pub fn try_new(typ: Spanned<MetaType>) -> Result<Self, Vec<Diagnostic>> {
        let diags = Source::check_type(&typ);
        if !diags.is_empty() {
            return Err(diags);
        }
        Ok(Self(typ.value))
    }

    pub fn into_inner(self) -> MetaType {
        self.0
    }
    pub fn inner(&self) -> &MetaType {
        &self.0
    }
}
impl ToCode for Type {
    fn to_code(&self, fmt_ctx: &FormatContext) -> String {
        self.0.to_code(fmt_ctx)
    }
}

pub struct TypeChecker;
impl TypeChecker {
    fn get_term_by_qid<'a>(
        cs: &'a ContextStack,
        qid: &QualifiedIdentifier,
    ) -> Option<TermVarDefRef<'a>> {
        match qid {
            QualifiedIdentifier::Head(p) => match p.value() {
                QualifiedIdentifierPart::Normal(id) => cs.get_global_term(id),
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
            Variable::Local(lvar) => cs.get_term(lvar),
            Variable::Qualified(qid) => Self::get_term_by_qid(cs, qid),
        }
    }
    fn kinding_internal(
        cs: &ContextStack,
        typ: &Spanned<MetaType>,
    ) -> Result<Spanned<MetaKind>, Vec<Diagnostic>> {
        match typ.value() {
            MetaType::Var(id) => {
                let t = match cs.get_type(id.value()) {
                    Some(a) => a,
                    None => {
                        let diag = Diagnostic::new(
                            DiagnosticLevel::Error,
                            format!("unbound type variable: {}", id.as_str()),
                            id.span,
                        );
                        return Err(vec![diag]);
                    }
                };
                let (_, kind, _) = &*t;
                Ok(kind.clone())
            }
            // (KA-PI)
            // ctx |- T1 :: *
            // ctx, x: T1 |- T2 :: *
            // ============================
            // ctx |- (pi x: T1. T2) :: *
            MetaType::Pi { term_bind, body } => {
                let TermBind { pat, typ } = term_bind.as_ref().value();
                let _scope = pat
                    .spanned_local_variable()
                    .map(|lvar| cs.push_term_var(lvar.clone(), typ.as_ref().clone()));
                let typ_kind = Self::kinding_internal(cs, typ)?;
                if !typ_kind.is_proper_types() {
                    return Err(vec![Diagnostic::new(
                        DiagnosticLevel::Error,
                        format!(
                            "expected proper type, but got ({}) :: ({})",
                            typ.to_code(&ctx_for_err()),
                            typ_kind.to_code(&ctx_for_err()),
                        ),
                        typ.span,
                    )]);
                }
                let kind2 = Self::kinding_internal(cs, body)?;
                if !kind2.is_proper_types() {
                    return Err(vec![Diagnostic::new(
                        DiagnosticLevel::Error,
                        format!(
                            "expected proper type, but got ({}) :: ({})",
                            body.to_code(&ctx_for_err()),
                            kind2.to_code(&ctx_for_err()),
                        ),
                        body.span,
                    )]);
                }
                Ok(Spanned::new(typ.span, MetaKind::ProperTypes))
            }
            // (KA-APP)
            // ctx |-> S :: (pi x: T1. K)
            // ctx |-> t: T2
            // ctx |-> T1 === T2
            // ============================
            // ctx |- S t: [x |-> t]K
            // (KA-PRF)
            // ctx |- t: Prop
            // ============================
            // ctx |- Prf t :: *
            MetaType::ApplyTerm(typ2, t) => {
                let t_type = Self::typing_internal(cs, t)?;
                let t_type = Self::expand_head_type_vars_internal(cs, t_type)?;

                if typ2.is_prf() {
                    if t_type.is_prop() {
                        return Ok(Spanned::new(typ.span, MetaKind::ProperTypes));
                    } else {
                        return Err(vec![Diagnostic::new(
                            DiagnosticLevel::Error,
                            format!(
                                "expected Prop for argument of Prf, but got {}",
                                t_type.to_code(&ctx_for_err()),
                            ),
                            t.span,
                        )]);
                    }
                }
                let typ_kind = Self::kinding_internal(cs, typ2)?;
                match typ_kind.value {
                    MetaKind::TypeFamilies { term_bind, body } => {
                        let TermBind { pat, typ: typ1 } = term_bind.value();
                        if Self::type_equiv(cs, &t_type, typ1.as_ref()).is_err() {
                            return Err(vec![Diagnostic::new(
                                DiagnosticLevel::Error,
                                format!(
                                    "expected type {}, but got {}",
                                    typ2.to_code(&ctx_for_err()),
                                    t_type.to_code(&ctx_for_err()),
                                ),
                                t_type.span,
                            )]);
                        }
                        Ok(match pat.local_variable() {
                            Some(lvar) => {
                                body.map(|body| body.substitute_term(lvar.as_ref().into(), t))
                            }
                            None => *body,
                        })
                    }
                    _ => Err(vec![Diagnostic::new(
                        DiagnosticLevel::Error,
                        format!(
                            "expected type family, but got {}:: {}",
                            typ2.to_code(&ctx_for_err()),
                            typ_kind.to_code(&ctx_for_err()),
                        ),
                        typ2.span,
                    )]),
                }
            }
            // (KA-PROP)
            // ============================
            // ctx |- Prop :: *
            MetaType::Prop => Ok(Spanned::new(typ.span, MetaKind::ProperTypes)),
            _ => unreachable!("unsupported type: {}", typ.to_code(&ctx_for_err())),
        }
    }
    pub fn typing(
        ctx: &mut Context,
        term: &Spanned<Term>,
    ) -> Result<Spanned<Type>, Vec<Diagnostic>> {
        let cs = ctx.make_stack();
        let (_scope, diags) = cs.init(Self::typing_internal, Self::kinding_internal);
        if !diags.is_empty() {
            return Err(diags);
        }
        let typ = Self::typing_internal(&cs, &term.clone().map(|e| e.0))?;
        Ok(Self::eval_type_internal(&cs, &typ)?.map(Type))
        // Ok(Self::expand_head_type_vars_internal(&cs, typ)?.map(Type))
    }
    fn typing_internal(
        cs: &ContextStack,
        term: &Spanned<MetaTerm>,
    ) -> Result<Spanned<MetaType>, Vec<Diagnostic>> {
        match term.value() {
            // x: T is in ctx
            // =======================
            // ctx |- x: T
            MetaTerm::Var(var) => match Self::get_term_by_var(cs, var) {
                Some(typ) => {
                    let (_, typ, _) = &*typ;
                    Ok(typ.clone())
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
            // ctx, x: T |- t: T2
            // ctx |- T:: *
            // =======================
            // ctx |- (\x: T. t): (pi x: T. T2)
            MetaTerm::AbsTerm { term_bind, body } => {
                let _scope = term_bind
                    .spanned_local_variable()
                    .map(|lvar| cs.push_term_var(lvar.clone(), term_bind.spanned_type().clone()));
                let body_type = Self::typing_internal(cs, body)?;
                Ok(Spanned::new(
                    term.span,
                    MetaType::Pi {
                        term_bind: term_bind.clone().into(),
                        body: body_type.into(),
                    },
                ))
            }
            // ctx |- t1: (pi x: T1. T2)
            // ctx |- t2: T1
            // =========================
            // ctx |- t1 t2: [x |-> t2]T2
            MetaTerm::ApplyTerm(t1, t2) => {
                let t1_type = Self::typing_internal(cs, t1)?;
                let t1_type = Self::expand_head_type_vars_internal(cs, t1_type)?;
                // TODO: not sure
                // NOTE: ここは想像で補間して書いた
                let t1_type = match t1_type.value {
                    MetaType::ApplyTerm(typ1, t2) => {
                        // TODO: error message
                        Self::type_equiv(cs, typ1.as_ref(), &Spanned::new(None, MetaType::Prf))?;
                        let t2_whnf = Self::whnf(cs, &t2)?;
                        match t2_whnf.value {
                            MetaTerm::All { term_bind, body } => MetaType::Pi {
                                term_bind: term_bind.into(),
                                body: Spanned::new(
                                    body.span,
                                    MetaType::ApplyTerm(
                                        Spanned::new(body.span, MetaType::Prf).into(),
                                        *body,
                                    ),
                                )
                                .into(),
                            },
                            _ => {
                                return Err(vec![Diagnostic::new(
                                    DiagnosticLevel::Error,
                                    format!(
                                        "expected pi type, but got {}",
                                        t2_whnf.to_code(&ctx_for_err())
                                    ),
                                    t2_whnf.span,
                                )])
                            }
                        }
                    }
                    t1_type => t1_type,
                };
                match t1_type {
                    MetaType::Pi { term_bind, body } => {
                        let term_bind_type = term_bind.spanned_type();
                        let t2_type = Self::typing_internal(cs, t2)?;
                        let body = match term_bind.pat.local_variable() {
                            Some(lvar) => body.map(|body| body.substitute_term(lvar.into(), t2)),
                            None => *body,
                        };
                        // term_bind.pat.identifier().map_or_else(
                        //     || *body,
                        //     |id| body.map(|t| t.substitute_term(id, t2.as_ref())),
                        // );
                        Self::type_equiv(cs, term_bind_type, &t2_type)?;
                        Ok(body)
                        // .map_err(|err| {
                        //     let err = err.map_message(|msg| {
                        //         format!(
                        //             "lambda argument type and applied value do not match: {}",
                        //             msg,
                        //         )
                        //     });
                        //     vec![err]
                        // })?
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
            // (QT-ALL-E)
            // ctx |- T :: *
            // ctx, x: T |- t: Prop
            // =========================
            // ctx |- all x: T. t: Prop
            MetaTerm::All { term_bind, body } => {
                let term_bind_type = term_bind.spanned_type();
                let term_bind_type_kind = Self::kinding_internal(cs, term_bind_type)?;
                if !term_bind_type_kind.is_proper_types() {
                    let diag = Diagnostic::new(
                        DiagnosticLevel::Error,
                        format!(
                            "expected proper type for all type, but found {} :: {}",
                            term_bind_type.to_code(&ctx_for_err()),
                            term_bind_type_kind.to_code(&ctx_for_err()),
                        ),
                        term_bind_type.span,
                    );
                    return Err(vec![diag]);
                }
                let _scope = term_bind
                    .spanned_local_variable()
                    .map(|lvar| cs.push_term_var(lvar.clone(), term_bind.spanned_type().clone()));
                let body_type = Self::typing_internal(cs, body)?;
                let body_type = Self::expand_head_type_vars_internal(cs, body_type)?;
                if !body_type.is_prop() {
                    return {
                        let diag = Diagnostic::new(
                            DiagnosticLevel::Error,
                            format!(
                                "expected prop type for type of all bind, but found {}",
                                body_type.to_code(&ctx_for_err()),
                            ),
                            body_type.span,
                        );
                        Err(vec![diag])
                    };
                }
                Ok(Spanned::new(term.span, MetaType::Prop))
            }
            _ => unreachable!("unsupported term: {:?}", term),
        }
    }

    fn expand_head_type_vars_internal(
        cs: &ContextStack,
        typ: Spanned<MetaType>,
    ) -> Result<Spanned<MetaType>, Vec<Diagnostic>> {
        match typ.value() {
            MetaType::Var(id) => match cs.get_type(id) {
                Some(typ) => {
                    let (_, _, typ) = &*typ;
                    // In this system, there's no bound as local type variables
                    let typ = typ.as_ref().unwrap_or_else(|| {
                        unreachable!("In this system, there's no bound as local type variables")
                    });
                    Ok(Self::expand_head_type_vars_internal(cs, typ.clone())?)
                }
                None => Err({
                    let diag = Diagnostic::new(
                        DiagnosticLevel::Error,
                        format!("unbound type variable: {}", id.as_str()),
                        typ.span,
                    );
                    vec![diag]
                }),
            },
            _ => Ok(typ),
        }
    }
    fn eval_type_internal(
        cs: &ContextStack,
        typ: &Spanned<MetaType>,
    ) -> Result<Spanned<MetaType>, Vec<Diagnostic>> {
        match typ.value() {
            MetaType::Var(id) => match cs.get_type(id) {
                Some(typ) => {
                    let (_, _, typ) = &*typ;
                    // In this system, there's no bound as local type variables
                    let typ = typ.as_ref().unwrap_or_else(|| {
                        unreachable!("In this system, there's no bound as local type variables")
                    });
                    Ok(Self::eval_type_internal(cs, typ)?)
                }
                None => Err({
                    let diag = Diagnostic::new(
                        DiagnosticLevel::Error,
                        format!("unbound type variable: {}", id.as_str()),
                        typ.span,
                    );
                    vec![diag]
                }),
            },
            MetaType::Pi { term_bind, body } => {
                let term_bind = Spanned::new(
                    term_bind.span,
                    TermBind {
                        pat: term_bind.pat.clone(),
                        typ: Self::eval_type_internal(cs, term_bind.spanned_type())?.into(),
                    },
                );
                let _scope = term_bind
                    .spanned_local_variable()
                    .map(|lvar| cs.push_term_var(lvar.clone(), term_bind.spanned_type().clone()));
                let body = Self::eval_type_internal(cs, body)?;
                Ok(Spanned::new(
                    typ.span,
                    MetaType::Pi {
                        term_bind: term_bind.into(),
                        body: body.into(),
                    },
                ))
            }
            MetaType::ApplyTerm(typ1, t2) => {
                let typ1 = Self::eval_type_internal(cs, typ1)?;
                let t2 = Self::eval_term_internal(cs, t2)?;
                match (typ1.value(), t2.value()) {
                    (MetaType::Prf, MetaTerm::All { term_bind, body }) => {
                        let term_bind = Spanned::new(
                            term_bind.span,
                            TermBind {
                                pat: term_bind.pat.clone(),
                                typ: Self::eval_type_internal(cs, term_bind.spanned_type())?.into(),
                            },
                        );
                        let _scope = term_bind.spanned_local_variable().map(|lvar| {
                            cs.push_term_var(lvar.clone(), term_bind.spanned_type().clone())
                        });
                        let body = Spanned::new(
                            body.span,
                            MetaType::ApplyTerm(
                                Spanned::new(typ1.span, MetaType::Prf).into(),
                                Self::eval_term_internal(cs, body)?,
                            ),
                        );
                        let body = Self::eval_type_internal(cs, &body)?;
                        Ok(Spanned::new(
                            t2.span,
                            MetaType::Pi {
                                term_bind: term_bind.into(),
                                body: body.into(),
                            },
                        ))
                    }
                    _ => Ok(typ.clone()),
                }
            }
            _ => Ok(typ.clone()),
        }
    }

    pub fn eval_term(
        ctx: &mut Context,
        term: &Spanned<Term>,
    ) -> Result<Spanned<Term>, Vec<Diagnostic>> {
        let cs = ctx.make_stack();
        let (_scope, diags) = cs.init(Self::typing_internal, Self::kinding_internal);
        if !diags.is_empty() {
            return Err(diags);
        }
        Self::eval_term_internal(&cs, &term.clone().map(|e| e.0)).map(|e| e.map(Term))
    }
    fn eval_term_internal(
        cs: &ContextStack,
        term: &Spanned<MetaTerm>,
    ) -> Result<Spanned<MetaTerm>, Vec<Diagnostic>> {
        match term.value() {
            // MetaTerm::Var(..) => Ok(term.clone()),
            MetaTerm::Var(var) => {
                let v = {
                    let term_def = Self::get_term_by_var(cs, var.value()).ok_or_else(|| {
                        let diag = Diagnostic::new(
                            DiagnosticLevel::Error,
                            format!(
                                "eval_term: unbound term variable: {}",
                                var.to_code(&ctx_for_err())
                            ),
                            term.span,
                        );
                        vec![diag]
                    })?;
                    term_def.2.clone()
                };

                match v {
                    Some(v) => Self::eval_term_internal(cs, &v),
                    None => Ok(term.clone()),
                }
            }
            MetaTerm::AbsTerm { term_bind, body } => {
                let term_bind = Spanned::new(
                    term_bind.span,
                    TermBind {
                        pat: term_bind.pat.clone(),
                        typ: Self::eval_type_internal(cs, term_bind.spanned_type())?.into(),
                    },
                );
                let _scope = term_bind
                    .spanned_local_variable()
                    .map(|lvar| cs.push_term_var(lvar.clone(), term_bind.spanned_type().clone()));
                // TODO: To calculate full beta-reduction, all bind vars should be alpha-converted.
                let body = Self::eval_term_internal(cs, body.as_ref())?;
                Ok(Spanned::new(
                    term.span,
                    MetaTerm::AbsTerm {
                        term_bind,
                        body: body.into(),
                    },
                ))
            }
            MetaTerm::All { term_bind, body } => {
                let term_bind = Spanned::new(
                    term_bind.span,
                    TermBind {
                        pat: term_bind.pat.clone(),
                        typ: Self::eval_type_internal(cs, term_bind.spanned_type())?.into(),
                    },
                );
                let _scope = term_bind
                    .spanned_local_variable()
                    .map(|lvar| cs.push_term_var(lvar.clone(), term_bind.spanned_type().clone()));
                // TODO: To calculate full beta-reduction, all bind vars should be alpha-converted.
                let body = Self::eval_term_internal(cs, body.as_ref())?;
                Ok(Spanned::new(
                    term.span,
                    MetaTerm::All {
                        term_bind,
                        body: body.into(),
                    },
                ))
            }
            MetaTerm::ApplyTerm(t1, t2) => {
                let t1 = Self::eval_term_internal(cs, t1.as_ref())?;
                let t2 = Self::eval_term_internal(cs, t2.as_ref())?;
                match t1.value {
                    MetaTerm::AbsTerm { term_bind, body } => {
                        let t2_type = Self::typing_internal(cs, &t2)?;
                        Self::type_equiv(cs, term_bind.spanned_type(), &t2_type).map_or_else(
                            Err,
                            |_| {
                                // let body = term_bind.spanned_identifier().map_or_else(
                                //     || *body,
                                //     |id| body.map(|t| t.substitute_term(id, &t2)),
                                // );
                                let body = match term_bind.local_variable() {
                                    Some(lvar) => body.map(|t| t.substitute_term(lvar.into(), &t2)),
                                    None => *body,
                                };
                                Self::eval_term_internal(cs, &body)
                            },
                        )
                    }
                    _ => Ok(Spanned::new(
                        term.span,
                        MetaTerm::ApplyTerm(t1.into(), t2.into()),
                    )),
                }
            }
            _ => unreachable!("unsupported term: {:?}", term),
        }
    }

    fn type_equiv(
        cs: &ContextStack,
        typ1: &Spanned<MetaType>,
        typ2: &Spanned<MetaType>,
    ) -> Result<(), Vec<Diagnostic>> {
        let mut ctx2 = cs.fork();
        let cs2 = ctx2.make_stack();
        let mut idc1 = VariableLevelContextCc::default();
        let mut idc2 = VariableLevelContextCc::default();
        let ids1 = idc1.make_stack();
        let ids2 = idc2.make_stack();
        Self::type_equiv_internal(cs, &cs2, typ1, typ2, &ids1, &ids2, 0)
    }
    fn type_equiv_internal(
        cs1: &ContextStack,
        cs2: &ContextStack,
        typ1: &Spanned<MetaType>,
        typ2: &Spanned<MetaType>,
        ids1: &VariableLevelContextStackCc,
        ids2: &VariableLevelContextStackCc,
        level: usize,
    ) -> Result<(), Vec<Diagnostic>> {
        let span1 = typ1.span;
        match (typ1.value(), typ2.value()) {
            (MetaType::Var(id1), _) => {
                let typ1 = match cs1.get_type(id1) {
                    Some(typ1) => typ1,
                    None => {
                        let diag = Diagnostic::new(
                            DiagnosticLevel::Error,
                            format!("type_equiv: unbound type variable: {}", id1.as_str()),
                            span1,
                        );
                        return Err(vec![diag]);
                    }
                };
                let (_, _, typ1) = &*typ1;
                let typ1 = typ1.as_ref().unwrap();
                Self::type_equiv_internal(cs1, cs2, typ1, typ2, ids1, ids2, level)
            }
            (_, MetaType::Var(id2)) => {
                let typ2 = match cs2.get_type(id2) {
                    Some(typ2) => typ2,
                    None => {
                        let diag = Diagnostic::new(
                            DiagnosticLevel::Error,
                            format!("type_equiv: unbound type variable: {}", id2.as_str()),
                            span1,
                        );
                        return Err(vec![diag]);
                    }
                };
                let (_, _, typ2) = &*typ2;
                let typ2 = typ2.as_ref().unwrap();
                Self::type_equiv_internal(cs1, cs2, typ1, typ2, ids1, ids2, level)
            }
            // (QTA-PI)
            // T1 === T1'
            // x: T1 |- T2 === T2'
            // -----------------
            // pi x: T1. T2 === pi x: T1'. T2'
            (
                MetaType::Pi {
                    term_bind: term_bind1,
                    body: body1,
                },
                MetaType::Pi {
                    term_bind: term_bind2,
                    body: body2,
                },
            ) => {
                let TermBind {
                    pat: pat1,
                    typ: typ1,
                } = term_bind1.as_ref().value();
                let TermBind {
                    pat: pat2,
                    typ: typ2,
                } = term_bind2.as_ref().value();
                Self::type_equiv_internal(cs1, cs2, typ1, typ2, ids1, ids2, level)?;
                let _scope1 = pat1.spanned_local_variable().map(|lvar| {
                    (
                        cs1.push_term_var(lvar.clone(), typ1.as_ref().clone()),
                        ids1.push(lvar.clone(), level),
                    )
                });
                let _scope2 = pat2.spanned_local_variable().map(|lvar| {
                    (
                        cs2.push_term_var(lvar.clone(), typ2.as_ref().clone()),
                        ids2.push(lvar.clone(), level),
                    )
                });
                Self::type_equiv_internal(cs1, cs2, body1, body2, ids1, ids2, level + 1)
            }
            // (QTA-APP)
            // typ1 === typ2
            // t1 === t2
            // -----------------
            // typ1 t1 === typ2 t2
            (MetaType::ApplyTerm(typ1, t1), MetaType::ApplyTerm(typ2, t2)) => {
                Self::type_equiv_internal(cs1, cs2, typ1, typ2, ids1, ids2, level)?;
                Self::term_equiv_internal(cs1, cs2, t1, t2, ids1, ids2, level)
            }
            // (MetaType::ApplyTerm(typ1, t1), _) if typ1.is_prf() => {
            //     let t1_whnf = Self::whnf(cs1, t1)?;
            //     match t1_whnf.value {
            //         MetaTerm::All { term_bind, body } => {
            //             let typ1 = Spanned::new(
            //                 None,
            //                 MetaType::Pi {
            //                     term_bind: term_bind.into(),
            //                     body: Spanned::new(
            //                         None,
            //                         MetaType::ApplyTerm(
            //                             Spanned::new(None, MetaType::Prf).into(),
            //                             *body,
            //                         ),
            //                     )
            //                     .into(),
            //                 },
            //             );
            //             Self::type_equiv_internal(cs1, cs2, &typ1, typ2, ids1, ids2, level)
            //         }
            //         _ => {
            //             let diag = Diagnostic::new(
            //                 DiagnosticLevel::Error,
            //                 format!("expected proof, but got {:?}", t1_whnf.to_code()),
            //                 t1.span,
            //             );
            //             Err(vec![diag])
            //         }
            //     }
            // }
            // (_, MetaType::ApplyTerm(typ2, t2)) if typ2.is_prf() => {
            //     let t2_whnf = Self::whnf(cs2, t2)?;
            //     match t2_whnf.value {
            //         MetaTerm::All { term_bind, body } => {
            //             let typ2 = Spanned::new(
            //                 None,
            //                 MetaType::Pi {
            //                     term_bind: term_bind.into(),
            //                     body: Spanned::new(
            //                         None,
            //                         MetaType::ApplyTerm(
            //                             Spanned::new(None, MetaType::Prf).into(),
            //                             *body,
            //                         ),
            //                     )
            //                     .into(),
            //                 },
            //             );
            //             Self::type_equiv_internal(cs1, cs2, typ1, &typ2, ids1, ids2, level)
            //         }
            //         _ => {
            //             let diag = Diagnostic::new(
            //                 DiagnosticLevel::Error,
            //                 format!("expected proof, but got {:?}", t2_whnf.to_code()),
            //                 t2.span,
            //             );
            //             Err(vec![diag])
            //         }
            //     }
            // }
            // (QKA-PRF)
            // s === t
            // -----------------
            // Prf s === Prf t
            //
            // But this is redundant with (QTA-APP).
            //
            // -----------------
            // Prf === Prf
            //
            // is enough.
            (MetaType::Prf, MetaType::Prf) => Ok(()),
            // TODO
            (MetaType::Prop, MetaType::Prop) => Ok(()),
            // (QKA-PI-PRF)
            // ctx |- t -->wh all x: T1. t2
            // ctx |- S1 === S2
            // ctx, x: S1 |- S2 === Prf t2
            // --------------------------------
            // ctx |- pi x: S1. S2 === Prf t
            (
                MetaType::Pi {
                    term_bind: term_bind1,
                    body: body1,
                },
                MetaType::ApplyTerm(typ2, t),
            ) => match typ2.value() {
                MetaType::Prf => {
                    let t_whnf = Self::whnf(cs2, t)?;
                    match t_whnf.value() {
                        MetaTerm::All {
                            term_bind: term_bind2,
                            body: t2,
                        } => {
                            let TermBind {
                                pat: pat1,
                                typ: typ1,
                            } = term_bind1.value();
                            let TermBind {
                                pat: pat2,
                                typ: typ2,
                            } = term_bind2.value();
                            Self::type_equiv_internal(cs1, cs2, typ1, typ2, ids1, ids2, level + 1)?;
                            let _scope1 = pat1.spanned_local_variable().map(|lvar| {
                                (
                                    cs1.push_term_var(lvar.clone(), typ1.as_ref().clone()),
                                    ids1.push(lvar.clone(), level),
                                )
                            });
                            let _scope2 = pat2.spanned_local_variable().map(|lvar| {
                                (
                                    cs2.push_term_var(lvar.clone(), typ2.as_ref().clone()),
                                    ids2.push(lvar.clone(), level),
                                )
                            });
                            Self::type_equiv_internal(
                                cs1,
                                cs2,
                                body1,
                                &Spanned::new(
                                    t2.span,
                                    MetaType::ApplyTerm(
                                        Spanned::new(t2.span, MetaType::Prf).into(),
                                        t2.as_ref().clone(),
                                    ),
                                ),
                                ids1,
                                ids2,
                                level + 1,
                            )
                        }
                        _ => Err({
                            let diag = Diagnostic::new(
                                DiagnosticLevel::Error,
                                format!(
                                    "while checking pi .. === Prf <here>: expected all .., but found {}",
                                    t_whnf.to_code(&ctx_for_err()),
                                ),
                                t_whnf.span,
                            );
                            vec![diag]
                        }),
                    }
                }
                _ => Err({
                    let diag = Diagnostic::new(
                        DiagnosticLevel::Error,
                        format!(
                            "while checking pi .. === <here> ..: expected proof type, but found {}",
                            typ2.to_code(&ctx_for_err()),
                        ),
                        typ2.span,
                    );
                    vec![diag]
                }),
            },
            (MetaType::ApplyTerm(..), MetaType::Pi { .. }) => {
                Self::type_equiv_internal(cs2, cs1, typ2, typ1, ids2, ids1, level)
            }
            (typ1, typ2) => Err({
                let diag = Diagnostic::new(
                    DiagnosticLevel::Error,
                    format!(
                        "expected the same type, but got {} and {}",
                        typ1.to_code(&ctx_for_err()),
                        typ2.to_code(&ctx_for_err()),
                    ),
                    span1,
                );
                vec![diag]
            }),
        }
    }

    // fn term_equiv(
    //     cs: &ContextStack,
    //     t1: &Spanned<MetaTerm>,
    //     t2: &Spanned<MetaTerm>,
    // ) -> Result<(), Diagnostic> {
    //     let mut ctx2 = cs.fork();
    //     let cs2 = ctx2.make_stack();
    //     let mut idc1 = VariableLevelContext::default();
    //     let mut idc2 = VariableLevelContext::default();
    //     let ids1 = idc1.make_stack();
    //     let ids2 = idc2.make_stack();
    //     Self::term_equiv_internal(cs, &cs2, t1, t2, &ids1, &ids2, 0)
    // }
    // fn term_equiv_internal(
    //     cs1: &ContextStack,
    //     cs2: &ContextStack,
    //     t1: &Spanned<MetaTerm>,
    //     t2: &Spanned<MetaTerm>,
    //     ids1: &VariableLevelContextStack,
    //     ids2: &VariableLevelContextStack,
    //     level: usize,
    // ) -> Result<(), Diagnostic> {
    //      let span1 = t1.span;
    //      match (t1.value(), t2.value()) {
    //          // T1 === T2
    //          // x: T1 |- t1 === t2
    //          // ---------------------------------
    //          // \x: T1. t1 === \x: T2. t2
    //          (
    //              MetaTerm::AbsTerm {
    //                  term_bind: term_bind1,
    //                  body: body1,
    //              },
    //              MetaTerm::AbsTerm {
    //                  term_bind: term_bind2,
    //                  body: body2,
    //              },
    //          ) => {
    //              let TermBind {
    //                  pat: pat1,
    //                  typ: typ1,
    //              } = term_bind1.value();
    //              let TermBind {
    //                  pat: pat2,
    //                  typ: typ2,
    //              } = term_bind2.value();
    //              Self::type_equiv_internal(cs1, cs2, typ1, typ2, ids1, ids2, level + 1)?;
    //              let _scope1 = pat1.spanned_identifier().map(|id| {
    //                  cs2.push_term_var(id.clone(), typ1.as_ref().clone());
    //              });
    //              let _scope2 = pat2.spanned_identifier().map(|id| {
    //                  cs2.push_term_var(id.clone(), typ2.as_ref().clone());
    //              });
    //              Self::term_equiv_internal(cs1, cs2, body1, body2, ids1, ids2, level + 1)
    //          }
    //          // t1 === t2
    //          // s1 === s2
    //          // ---------------------------------
    //          // t1 s1 === t2 s2
    //          (MetaTerm::ApplyTerm(t1, s1), MetaTerm::ApplyTerm(t2, s2)) => {
    //             Self::term_equiv_internal(cs1, cs2, t1, t2, ids1, ids2, level)?;
    //             Self::term_equiv_internal(cs1, cs2, s1, s2, ids1, ids2, level)
    //          }
    //          // x: T1 |- t1 === t2 x
    //          // t2 not lambda
    //          // ---------------------------------
    //          // \x: T1. t1 ===wh t2
    //          (
    //              MetaTerm::AbsTerm {
    //                  term_bind: term_bind1,
    //                  body: body1,
    //              },
    //              _,
    //          ) => {
    //              let TermBind {
    //                  pat: pat1,
    //                  typ: typ1,
    //              } = term_bind1.value();
    //              let _scope1 = pat1.spanned_identifier().map(|id| {
    //                  cs2.push_term_var(id.clone(), typ1.as_ref().clone());
    //              });
    //              Self::term_equiv_internal(cs1, cs2, body1, t2)
    //          }
    //         // x: T2 |- t1 x === t2
    //            // t1 not lambda
    //            // ---------------------------------
    //            // t1 ===wh \x: T2. t2
    //      }
    // }

    pub fn term_equiv(
        cs: &ContextStack,
        t1: &Spanned<MetaTerm>,
        t2: &Spanned<MetaTerm>,
    ) -> Result<(), Vec<Diagnostic>> {
        let mut ctx2 = cs.fork();
        let cs2 = ctx2.make_stack();
        let mut idc1 = VariableLevelContextCc::default();
        let mut idc2 = VariableLevelContextCc::default();
        let ids1 = idc1.make_stack();
        let ids2 = idc2.make_stack();
        Self::term_equiv_internal(cs, &cs2, t1, t2, &ids1, &ids2, 0)
    }
    fn term_equiv_internal(
        cs1: &ContextStack,
        cs2: &ContextStack,
        t1: &Spanned<MetaTerm>,
        t2: &Spanned<MetaTerm>,
        ids1: &VariableLevelContextStackCc,
        ids2: &VariableLevelContextStackCc,
        level: usize,
    ) -> Result<(), Vec<Diagnostic>> {
        // Checks if the two terms are well-formed in simple type.
        // If not, calculation of whnf may diverge.
        // Self::simple_typing(cs1, t1)?;
        // Self::simple_typing(cs2, t2)?;

        // whnf(t1) ===wh whnf(t2)
        // -----------------
        // t1 === t2
        let t1 = Self::whnf(cs1, t1)?;
        let t2 = Self::whnf(cs2, t2)?;
        Self::term_equiv_wh_internal(cs1, cs2, &t1, &t2, ids1, ids2, level)
    }
    // weak head normal form
    // This halts on well-typed terms.
    fn whnf(
        cs: &ContextStack,
        t: &Spanned<MetaTerm>,
    ) -> Result<Spanned<MetaTerm>, Vec<Diagnostic>> {
        match t.value() {
            MetaTerm::Var(var) => {
                let v = {
                    let term_def = Self::get_term_by_var(cs, var).ok_or_else(|| {
                        let diag = Diagnostic::new(
                            DiagnosticLevel::Error,
                            format!(
                                "whnf: unbound term variable: {}",
                                var.to_code(&ctx_for_err())
                            ),
                            t.span,
                        );
                        vec![diag]
                    })?;
                    term_def.2.clone()
                };

                match v {
                    Some(v) => Self::whnf(cs, &v),
                    None => Ok(t.clone()),
                }
            }
            MetaTerm::ApplyTerm(t1, t2) => {
                let t1 = Self::whnf(cs, t1)?;
                match t1.value() {
                    MetaTerm::AbsTerm { term_bind, body } => {
                        let TermBind { pat, typ } = term_bind.value();

                        // TODO: I'm not sure this strategy halts totally.
                        let t2_type = Self::typing_internal(cs, t2)?;
                        if let Err(mut diags) = Self::type_equiv(cs, typ, &t2_type) {
                            return Err({
                                let diag = Diagnostic::new(
                                    DiagnosticLevel::Error,
                                    format!(
                                        "type mismatch: expected {}, found {}",
                                        typ.to_code(&ctx_for_err()),
                                        t2_type.to_code(&ctx_for_err()),
                                    ),
                                    t2.span,
                                );
                                diags.push(diag);
                                diags
                            });
                        }

                        // let _scope = pat
                        //     .spanned_identifier()
                        //     .map(|id| cs.push_term_var(id.clone(), typ.as_ref().clone()));

                        let body = match pat.value() {
                            Pattern::Named(name) => body
                                .as_ref()
                                .clone()
                                .map(|body| body.substitute_term(name.value().into(), t2)),
                            Pattern::Wildcard => body.as_ref().clone(),
                        };
                        Self::whnf(cs, &body)
                        // Self::whnf(
                        //     cs,
                        //     match pat.value() {
                        //         Pattern::Named(name) => &body
                        //             .as_ref()
                        //             .as_ref()
                        //             .map(|body| body.clone().substitute_term(&name, t2)),
                        //         Pattern::Wildcard => body.as_ref(),
                        //     },
                        // )
                        // match pat.value() {
                        //     Pattern::Named(name) => Self::whnf(
                        //         cs,
                        //         &body
                        //             .as_ref()
                        //             .as_ref()
                        //             .map(|body| body.clone().substitute_term(&name, t2)),
                        //     ),
                        //     Pattern::Wildcard => Self::whnf(cs, body.as_ref()),
                        // }
                    }
                    _ => Ok(Spanned::new(
                        t.span,
                        MetaTerm::ApplyTerm(t1.clone().into(), t2.clone()),
                    )),
                }
            }
            _ => Ok(t.clone()),
        }
    }
    // fn simple_typing(
    //     cs: &ContextStack,
    //     t: &Spanned<MetaTerm>,
    // ) -> Result<Spanned<MetaType>, Diagnostic> {
    //     let span = t.span;
    //     match t.value() {
    //         MetaTerm::Var(var) => {
    //             let term_def = cs.get_term(var.value()).ok_or_else(|| {
    //                 Diagnostic::new(
    //                     DiagnosticLevel::Error,
    //                     format!("unbound term variable: {}", var.to_code()),
    //                     span,
    //                 )
    //             })?;
    //             let typ = term_def.1;
    //             Ok(typ.clone())
    //         }
    //         MetaTerm::ApplyTerm(t1, t2) => {
    //             let typ1 = Self::simple_typing(cs, t1)?;
    //             let typ2 = Self::simple_typing(cs, t2)?;
    //             match typ1.value() {
    //                 MetaType::Pi {
    //                     term_bind: term_bind1,
    //                     body: body1,
    //                 } => {
    //                     let TermBind { typ: typ1, .. } = term_bind1.as_ref().value();
    //                     Self::type_equiv(cs, typ1.as_ref(), &typ2)?;
    //                     Ok(body1.as_ref().clone())
    //                 }
    //                 _ => Err(Diagnostic::new(
    //                     DiagnosticLevel::Error,
    //                     format!("expected a function type, but got {}", typ1.to_code()),
    //                     span,
    //                 )),
    //             }
    //         }
    //         MetaTerm::AbsTerm { term_bind, body } => {
    //             let TermBind { pat, typ, .. } = term_bind.value();
    //             let typ = typ.as_ref();
    //             let body_typ = Self::simple_typing(cs, body.as_ref())?;
    //             Ok(Spanned::new(
    //                 span,
    //                 MetaType::Pi {
    //                     term_bind: Spanned::new(
    //                         term_bind.span,
    //                         TermBind {
    //                             pat: Spanned::new(None, Pattern::Wildcard),
    //                             typ: Self::unwrap_apply_term(typ).into(),
    //                         },
    //                     )
    //                     .into(),
    //                     body: body_typ.into(),
    //                 },
    //             ))
    //         }
    //         MetaTerm::Let { pat, val, body } => {
    //             let val_typ = Self::simple_typing(cs, val.as_ref())?;
    //             let _scope = pat.spanned_identifier().map(|id| {
    //                 cs.push_term_var(id.clone(), val_typ.clone());
    //             });
    //             let body_typ = Self::simple_typing(cs, body.as_ref())?;
    //             Self::type_equiv(cs, &body_typ, &val_typ)?;
    //             Ok(body_typ)
    //         }
    //         MetaTerm::All { term_bind, body } => Ok(Spanned::new(None, MetaType::Prop)),
    //         _ => unreachable!("simple_typing: unsupported term: {}", t.to_code()),
    //     }
    // }
    // fn unwrap_apply_term(typ: &Spanned<MetaType>) -> Spanned<MetaType> {
    //     match typ.value() {
    //         MetaType::ApplyTerm(typ1, _) => Self::unwrap_apply_term(typ1.as_ref()),
    //         _ => typ.clone(),
    //     }
    // }
    fn term_equiv_wh_internal(
        cs1: &ContextStack,
        cs2: &ContextStack,
        t1: &Spanned<MetaTerm>,
        t2: &Spanned<MetaTerm>,
        ids1: &VariableLevelContextStackCc,
        ids2: &VariableLevelContextStackCc,
        level: usize,
    ) -> Result<(), Vec<Diagnostic>> {
        let span1 = t1.span;
        let span2 = t2.span;
        match (t1.value(), t2.value()) {
            (MetaTerm::Var(var1), MetaTerm::Var(var2)) => {
                let v1 = Self::get_term_by_var(cs1, var1).ok_or_else(|| {
                    let diag = Diagnostic::new(
                        DiagnosticLevel::Error,
                        format!(
                            "term_equiv_wh: unbound term variable: {}",
                            var1.to_code(&ctx_for_err())
                        ),
                        span1,
                    );
                    vec![diag]
                })?;
                let (_, _, v1) = &*v1;
                let v2 = Self::get_term_by_var(cs2, var2).ok_or_else(|| {
                    let diag = Diagnostic::new(
                        DiagnosticLevel::Error,
                        format!(
                            "term_equiv_wh: unbound term variable: {}",
                            var2.to_code(&ctx_for_err())
                        ),
                        span2,
                    );
                    vec![diag]
                })?;
                let (_, _, v2) = &*v2;
                match (v1, v2) {
                    (None, None) => {
                        let id1 = var1.local();
                        let id2 = var2.local();
                        match (id1, id2) {
                            (Some(id1), Some(id2)) => {
                                // both are local bound variables
                                let level1 = ids1.get(id1).map(|(_, level)| level);
                                let level2 = ids2.get(id2).map(|(_, level)| level);
                                match (level1, level2) {
                                    (Some(level1), Some(level2)) if level1 == level2 => Ok(()),
                                    // scope-global variables; the same name variables are guaranteed to be defiend in the same place
                                    (None, None) if id1 == id2 => Ok(()),
                                    _ => {
                                        let diag = Diagnostic::new(
                                            DiagnosticLevel::Error,
                                            format!(
                                                "variable {} and {} are not equivalent",
                                                var1.to_code(&ctx_for_err()),
                                                var2.to_code(&ctx_for_err())
                                            ),
                                            span1,
                                        );
                                        Err(vec![diag])
                                    }
                                }
                            }
                            (None, None) => {
                                // both are global axiom
                                if var1.value() == var2.value() {
                                    Ok(())
                                } else {
                                    let diag = Diagnostic::new(
                                        DiagnosticLevel::Error,
                                        format!(
                                            "variable {} and {} are not equivalent",
                                            var1.to_code(&ctx_for_err()),
                                            var2.to_code(&ctx_for_err())
                                        ),
                                        span1,
                                    );
                                    Err(vec![diag])
                                }
                            }
                            _ => {
                                let diag = Diagnostic::new(
                                    DiagnosticLevel::Error,
                                    format!(
                                        "term_equiv_wh: variable {} and {} are not equivalent",
                                        var1.to_code(&ctx_for_err()),
                                        var2.to_code(&ctx_for_err())
                                    ),
                                    span1,
                                );
                                let diags = vec![diag];
                                Err(diags)
                            }
                        }
                    }
                    // _ => Err(vec![Diagnostic::new(
                    //     DiagnosticLevel::Error,
                    //     format!(
                    //         "variable {} and {} are not equivalent",
                    //         qid1.to_code(),
                    //         qid2.to_code()
                    //     ),
                    //     span1,
                    // )]),
                    _ => Self::term_equiv_internal(
                        cs1,
                        cs2,
                        v1.as_ref().unwrap_or(t1),
                        v2.as_ref().unwrap_or(t2),
                        ids1,
                        ids2,
                        level + 1,
                    ),
                }
            }
            // (QA-ABS)
            // T1 === T2
            // x: T1 |- t1 === t2
            // ---------------------------------
            // \x: T1. t1 ===wh \x: T2. t2
            (
                MetaTerm::AbsTerm {
                    term_bind: term_bind1,
                    body: body1,
                },
                MetaTerm::AbsTerm {
                    term_bind: term_bind2,
                    body: body2,
                },
            ) => {
                let TermBind {
                    pat: pat1,
                    typ: typ1,
                } = term_bind1.value();
                let TermBind {
                    pat: pat2,
                    typ: typ2,
                } = term_bind2.value();
                Self::type_equiv_internal(cs1, cs2, typ1, typ2, ids1, ids2, level + 1)?;
                let _scope1 = pat1.spanned_local_variable().map(|lvar| {
                    (
                        cs1.push_term_var(lvar.clone(), typ1.as_ref().clone()),
                        ids1.push(lvar.clone(), level),
                    )
                });
                let _scope2 = pat2.spanned_local_variable().map(|lvar| {
                    (
                        cs2.push_term_var(lvar.clone(), typ2.as_ref().clone()),
                        ids2.push(lvar.clone(), level),
                    )
                });
                Self::term_equiv_internal(
                    cs1,
                    cs2,
                    body1.as_ref(),
                    body2.as_ref(),
                    ids1,
                    ids2,
                    level + 1,
                )
            }
            (
                MetaTerm::All {
                    term_bind: term_bind1,
                    body: body1,
                },
                MetaTerm::All {
                    term_bind: term_bind2,
                    body: body2,
                },
            ) => {
                let TermBind {
                    pat: pat1,
                    typ: typ1,
                } = term_bind1.value();
                let TermBind {
                    pat: pat2,
                    typ: typ2,
                } = term_bind2.value();
                Self::type_equiv_internal(cs1, cs2, typ1, typ2, ids1, ids2, level + 1)?;
                let _scope1 = pat1.spanned_local_variable().map(|lvar| {
                    (
                        cs1.push_term_var(lvar.clone(), typ1.as_ref().clone()),
                        ids1.push(lvar.clone(), level),
                    )
                });
                let _scope2 = pat2.spanned_local_variable().map(|lvar| {
                    (
                        cs2.push_term_var(lvar.clone(), typ2.as_ref().clone()),
                        ids2.push(lvar.clone(), level),
                    )
                });
                Self::term_equiv_internal(
                    cs1,
                    cs2,
                    body1.as_ref(),
                    body2.as_ref(),
                    ids1,
                    ids2,
                    level + 1,
                )
            }
            // (QA-APP)
            // t1 ===wh t2
            // s1 === s2
            // ---------------------------------
            // t1 s1 === t2 s2
            (MetaTerm::ApplyTerm(t1, s1), MetaTerm::ApplyTerm(t2, s2)) => {
                Self::term_equiv_wh_internal(cs1, cs2, t1, t2, ids1, ids2, level)?;
                Self::term_equiv_internal(cs1, cs2, s1, s2, ids1, ids2, level)
            }
            // (QA-NABS1)
            // ctx, x: T1 |- t1 === t2 x
            // t2 not lambda
            // ---------------------------------
            // ctx |- \x: T1. t1 ===wh t2
            // [Fixed version]
            // (QA-NABS1')
            // ctx |- t2: pi x: T21. T22
            // ctx, x: T1 |- t1 === t2 x
            // ctx |- T1 === T21
            // t2 not lambda
            // ---------------------------------
            // ctx |- \x: T1. t1 ===wh t2
            (
                MetaTerm::AbsTerm {
                    term_bind: term_bind1,
                    body: body1,
                },
                _,
            ) => {
                let t2_type = Self::typing_internal(cs2, t2)?;
                // let t2_type = Self::expand_head_type_vars_internal(cs2, t2_type)?;
                // TODO: heavy process. typing internalでやってるみたいに，pi までの展開をすべき
                let t2_type = Self::eval_type_internal(cs2, &t2_type)?;
                match t2_type.value() {
                    MetaType::Pi {
                        term_bind: term_bind2,
                        ..
                    } => {
                        let TermBind {
                            pat: pat1,
                            typ: typ1,
                        } = term_bind1.value();
                        let TermBind {
                            pat: pat2,
                            typ: typ2,
                        } = term_bind2.value();
                        Self::type_equiv_internal(cs1, cs2, typ1, typ2, ids1, ids2, level + 1)?;
                        let _scope1 = pat1.spanned_local_variable().map(|lvar| {
                            (
                                cs1.push_term_var(lvar.clone(), typ1.as_ref().clone()),
                                ids1.push(lvar.clone(), level),
                            )
                        });
                        let _scope2 = pat2.spanned_local_variable().map(|lvar| {
                            (
                                cs2.push_term_var(lvar.clone(), typ2.as_ref().clone()),
                                ids2.push(lvar.clone(), level),
                            )
                        });
                        let t2 = Spanned::new(
                            merge_span(t2.span, pat2.span),
                            MetaTerm::ApplyTerm(
                                t2.clone().into(),
                                Spanned::new(
                                    pat2.span,
                                    pat2.spanned_local_variable().map_or_else(
                                        || unimplemented!(),
                                        |lvar| {
                                            MetaTerm::Var(
                                                lvar.as_ref()
                                                    .map(|lvar| Variable::Local(lvar.clone())),
                                            )
                                        },
                                    ),
                                )
                                .into(),
                            ),
                        );
                        Self::term_equiv_internal(cs1, cs2, body1, &t2, ids1, ids2, level)
                    }
                    _ => {
                        let diag = Diagnostic::new(
                            DiagnosticLevel::Error,
                            format!(
                                "term {} is not typed as a pi type: actually, {}",
                                t2.to_code(&ctx_for_err()),
                                t2_type.to_code(&ctx_for_err())
                            ),
                            t2.span,
                        );
                        Err(vec![diag])
                    }
                }
            }
            // (QA-NABS2)
            // x: T2 |- t1 x === t2
            // t1 not lambda
            // ---------------------------------
            // t1 ===wh \x: T2. t2
            (_, MetaTerm::AbsTerm { .. }) => {
                Self::term_equiv_internal(cs2, cs1, t2, t1, ids2, ids1, level)
            }
            _ => {
                let err1 = Diagnostic::new(
                    DiagnosticLevel::Error,
                    format!(
                        "term ({}) is not equivalent to ({})",
                        t1.value(),
                        t2.value(),
                    ),
                    span1,
                );
                Err(vec![err1])
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::common::diagnostic::{Spanned, SpannedWithDiagnostics};

    use super::*;

    fn simple(code: &str, expected_typ: &str, _expected_term: &str) {
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
        let (term, mut ctx) = source.value.into_term().unwrap_or_else(|e| {
            e.iter().for_each(|d| println!("{}", d));
            panic!("term parse error");
        });
        println!(
            "INTO_TERM: {}\n\n{}",
            ctx.to_code(&ctx_for_err()),
            term.to_code(&ctx_for_err()),
        );
        // let tc = TypeChecker::new();
        // let e = Evaluator::new(ctx.term_def_map());
        let typ = TypeChecker::typing(&mut ctx, &term).unwrap_or_else(|e| {
            e.iter().for_each(|d| println!("{}", d));
            panic!("typing error");
        });
        // let typ = tc.eval_type(&mut ctx, &typ).unwrap_or_else(|e| {
        //     e.iter().for_each(|d| println!("{}", d));
        //     panic!("type eval error");
        // });
        if expected_typ != "SKIP" {
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
        }
        // if expected_term != "SKIP" {
        //     let term = e
        //         .eval_iter(&term)
        //         .map_then_eval_with_limit(100, |e| {
        //             e.map(|e| {
        //                 println!("STEP: {}", e.value.0.to_code());
        //                 e
        //             })
        //         })
        //         .expect("evaluation limit exceeded");
        //     assert_eq!(
        //         term.value().clone().into_inner().to_code(),
        //         expected_term,
        //         "expected {} ->* {}",
        //         code,
        //         expected_term,
        //     );
        //     let typ2 = tc.typing(&mut ctx, term.clone()).unwrap_or_else(|e| {
        //         e.iter().for_each(|d| println!("{}", d));
        //         panic!("typing error");
        //     });
        //     assert_eq!(
        //         typ2.value.into_meta_type().to_code(),
        //         expected_typ,
        //         "{:?} : {:?}",
        //         term.value().clone().into_inner().to_code(),
        //         expected_typ
        //     );
        // }
    }
    fn err(code: &str, want_err_kind: &str, want_err_matcher: &str) {
        let (err_kind, diags) = err_internal(code);
        assert_eq!(err_kind, want_err_kind);
        // TODO: easy implementation
        let summary = diags
            .iter()
            .map(|d| d.to_string())
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            summary.contains(want_err_matcher),
            "{:?} is not contained in {:?}",
            want_err_matcher,
            summary
        );
    }
    fn err_internal(code: &str) -> (&'static str, Vec<Diagnostic>) {
        println!("====================");
        println!("code: {}", code);
        let SpannedWithDiagnostics {
            value: source,
            span,
            diagnostics,
        } = {
            let r = Parser::new(code).start_parse_source().into_result();
            match r {
                Ok(r) => r,
                Err(e) => {
                    return ("PARSE_ERROR", e.diagnostics);
                }
            }
        };
        if !diagnostics.is_empty() {
            diagnostics.iter().for_each(|d| println!("{}", d));
            return ("PARSE_WARN", diagnostics);
        }
        let source = {
            let r = Source::try_new(Spanned::new(span, source));
            match r {
                Ok(r) => r,
                Err(e) => {
                    e.iter().for_each(|d| println!("{}", d));
                    return ("SOURCE_TRY_NEW_ERROR", e);
                }
            }
        };
        println!("source: {}", source.to_code(&ctx_for_err()));
        let (term, mut ctx) = {
            let r = source.value.into_term();
            match r {
                Ok(r) => r,
                Err(e) => {
                    e.iter().for_each(|d| println!("{}", d));
                    return ("INTO_TERM_ERROR", e);
                }
            }
        };
        // let typ = TypeChecker::typing(&mut ctx, &term).unwrap_or_else(|e| {
        //     e.iter().for_each(|d| println!("{}", d));
        //     panic!("typing error");
        // });
        let _typ = {
            let r = TypeChecker::typing(&mut ctx, &term);
            match r {
                Ok(r) => r,
                Err(e) => {
                    e.iter().for_each(|d| println!("{}", d));
                    return ("TYPING_ERROR", e);
                }
            }
        };
        ("OK", Default::default())
    }

    fn nat_template() -> &'static str {
        r#"
        def nat = all X: Prop. all z: Prf X. all s: Prf X -> Prf X. X;
        def O = \X: Prop. \z: Prf X. \s: Prf X -> Prf X. z;
        def S = \n: Prf nat. \X: Prop. \z: Prf X. \s: Prf X -> Prf X. s (n X z s);
        def plus = \m: Prf nat. \n: Prf nat. \X: Prop. \z: Prf X. \s: Prf X -> Prf X. m X (n X z s) s;
        "#
    }
    fn vec_template() -> &'static str {
        r#"
        def Vec = (
          \T: Prop. \n2: Prf nat.
            all X: Prf nat -> Prop.
            all cons: (pi n3: Prf nat. Prf T -> Prf (X n3) -> Prf (X (S n3))).
            all nil: Prf (X O).
            X n2
        );
        def cons = (
          \T: Prop. \n0: Prf nat. \x: Prf T. \xs: Prf (Vec T n0).
            \X: Prf nat -> Prop.
            \cons: (pi n1: Prf nat. Prf T -> Prf (X n1) -> Prf (X (S n1))).
            \nil: Prf (X O).
            cons n0 x (xs X cons nil)
        );
        def nil = (
          \T: Prop.
            \X: Prf nat -> Prop.
            \cons: (pi n1: Prf nat. Prf T -> Prf (X n1) -> Prf (X (S n1))).
            \nil: Prf (X O).
            nil
        );
        "#
    }

    #[test]
    fn test1() {
        simple(
            r#"
            def a = all x: Prop. x;
            a
            "#,
            "Prop",
            "",
        );
    }

    #[test]
    fn test4() {
        simple(
            r#"
            def a = all x: Prop. x;
            def f = \x: Prop. \y: Prf x. y;
            f a
            "#,
            "pi y: (pi x: Prop. Prf x). pi x: Prop. Prf x",
            // "pi y: Prf(a). Prf(a)",
            "",
        );
    }
    #[test]
    fn test5() {
        simple(
            r#"
            def! a = all x: Prop. x;
            def f = \x: Prop. \y: Prf x. y;
            f a
            "#,
            "pi y: (pi x: Prop. Prf x). pi x: Prop. Prf x",
            "",
        );
    }

    #[test]
    fn test3() {
        simple(
            r#"
            def True = all x: Prop. all I: Prf x. x;
            def I = \x: Prop. \I: Prf x. I;
            def f = \x: Prop. \y: Prf x. y;
            f True
            "#,
            "pi y: (pi x: Prop. pi I: (Prf x). Prf x). pi x: Prop. pi I: (Prf x). Prf x",
            "",
        );
    }

    #[test]
    fn test11() {
        simple(
            r#"
            def nat = all X: Prop. all z: Prf X. all s: Prf X -> Prf X. X;
            nat
            "#,
            "Prop",
            "",
        );
    }
    #[test]
    fn test10() {
        let nat_template = nat_template();
        simple(
            format!(
                r#"
                {nat_template}

                def List = \T: Prop. all X: Prop. all cons: Prf T -> Prf X -> Prf X. all nil: Prf X. X;

                nat
                "#,
            ).as_str(),
            "Prop",
            "",
        );
    }

    #[test]
    fn test02() {
        let nat_template = nat_template();
        let vec_template = vec_template();
        simple(
            format!(
                r#"
                {nat_template}
                {vec_template}

                // nat equality check through Vec
                def lis1 = cons nat O (S (S O)) (nil nat);
                def lis2 = cons nat (S O) O lis1;
                def lis3 = cons nat (S (S O)) O lis2;
                // prove of 1 = 1 + 0
                def lis2_1 = cons nat (plus (S O) O) O lis1;
                // def lis2_1_err = cons nat (plus (S O) (S O)) O lis1;
                // prove of 2 = 1 + 1
                def lis3_1 = cons nat (plus (S O) (S O)) O lis2;

                lis3_1
                "#,
            )
            .as_str(),
            // Too complex to write
            "SKIP",
            "",
        );
    }

    #[test]
    fn test2_2() {
        let nat_template = nat_template();
        let vec_template = vec_template();
        err(
            format!(
                r#"
                {nat_template}
                {vec_template}

                // nat equality check through Vec
                // prove of 1 != 1 + 1
                def lis1 = cons nat O (S (S O)) (nil nat);
                def lis2_1_err = cons nat (plus (S O) (S O)) O lis1;

                lis2_1_err
                "#,
            )
            .as_str(),
            "TYPING_ERROR",
            "is not equivalent to",
        );
    }

    #[test]
    fn test6() {
        let nat_template = nat_template();
        err(
            format!(
                r#"
                {nat_template}

                def eq_not_good = \n: Prf nat. \m: Prf nat. \F: (pi n: Prf nat. Prop). \z: Prf (F m). (\x: Prf (F n). x) z;
                eq_not_good (S O) (O)
                "#,
            ).as_str(),
            "TYPING_ERROR",
            "variable n and m are not equivalent",
        );
    }

    #[test]
    fn test7() {
        let nat_template = nat_template();
        simple(
            format!(
                r#"
                {nat_template}

                def v1 = S O;
                def v2 = S O;

                // Right way of checking equality
                axiom F: pi n: Prf nat. Prop;
                axiom z: Prf (F v1);

                (\x: Prf (F v2). x) z
                "#,
            )
            .as_str(),
            "SKIP",
            "",
        );
    }

    #[test]
    fn test8() {
        let nat_template = nat_template();
        simple(
            format!(
                r#"
                {nat_template}

                // 1 + 2 = 3
                def v1 = plus (S O) (S (S O));
                def v2 = S (S (S O));

                // Right way of checking equality
                axiom F: pi n: Prf nat. Prop;
                axiom z: Prf (F v1);

                (\x: Prf (F v2). x) z
                "#,
            )
            .as_str(),
            "SKIP",
            "",
        );
    }

    #[test]
    fn test9() {
        let nat_template = nat_template();
        err(
            format!(
                r#"
                {nat_template}

                // 2 + 2 = 2 ??
                def v1 = plus (S (S O)) (S (S O));
                def v2 = S (S O);

                // Right way of checking equality
                axiom F: pi n: Prf nat. Prop;
                axiom z: Prf (F v1);

                (\x: Prf (F v2). x) z
                "#,
            )
            .as_str(),
            "TYPING_ERROR",
            "is not equivalent to",
        );
    }

    #[test]
    fn test12() {
        err(
            r#"
            (\x: u. x)
            "#,
            "TYPING_ERROR",
            "unbound type variable: u",
        );
    }

    #[test]
    fn test13() {
        simple(
            r#"
            type MyProp = Prop;
            (\x: MyProp. x)
            "#,
            "pi x: Prop. Prop",
            "",
        );
    }

    #[test]
    fn test14() {
        simple(
            r#"
            type MyProp = Prop;
            (\x: MyProp. x) (all x: Prop. x)
            "#,
            "Prop",
            "",
        );
    }

    #[test]
    fn test15() {
        simple(
            r#"
            type Nat = Prop;
            (\x: Nat. x) (all x: Prop. x)
            "#,
            "Prop",
            "",
        );
    }

    #[test]
    fn test16() {
        simple(
            r#"
            axiom nat: Prop;
            type Nat = Prf nat;
            all n: Nat. nat
            "#,
            "Prop",
            "",
        );
    }

    #[test]
    fn test17() {
        simple(
            r#"
            type MyProp = Prop;
            all n: MyProp. n
            "#,
            "Prop",
            "",
        );
    }

    #[test]
    fn test18() {
        err(
            r#"
            type A = A;
            all x: Prop. x
            "#,
            "TYPING_ERROR",
            "unbound type variable: A",
        );
    }

    #[test]
    fn test19() {
        err(
            r#"
            def f = f;
            all x: Prop. x
            "#,
            "TYPING_ERROR",
            "unbound variable: $this::f",
        );
    }

    #[test]
    fn test20() {
        err(
            r#"
            def f = $this::f;
            all x: Prop. x
            "#,
            "TYPING_ERROR",
            "unbound variable: $this::f",
        );
    }

    #[test]
    fn test21() {
        err(
            r#"
            axiom a: Prf a;
            all x: Prop. x
            "#,
            "TYPING_ERROR",
            "unbound variable: $this::a",
        );
    }

    #[test]
    fn test22() {
        err(
            r#"
            axiom a: Prf $this::a;
            all x: Prop. x
            "#,
            "TYPING_ERROR",
            "unbound variable: $this::a",
        );
    }

    #[test]
    fn test23() {
        // implies (implies P Q) P
        // implies (all _: Prf P. Q) P
        // (\Q. (all _: (pi _: Prf P. Q<upward>). Q<local>)) P
        // all _: (pi _: Prf P. P). P
        // all _: (pi _: Prf P. Q). P
        simple(
            r#"
            def implies = \P: Prop. \Q: Prop. all _: Prf P. Q;
            \P: Prop. \Q: Prop.
                \x: Prf implies (implies P Q) P. x
            "#,
            "pi P: Prop. pi Q: Prop. pi x: ((Prf P -> Prf Q) -> Prf P). (Prf P -> Prf Q) -> Prf P",
            "",
        );
    }

    // #[test]
    // fn test24() {
    //     simple(
    //         r#"
    //         // Leibniz equality
    //         def eq = \a: Prop. \x: Prf a. \y: Prf a. all p: Prf a -> Prop. all h: Prf (p x). p y;
    //         def intro_refl = \a: Prop. \x: Prf a. \p: Prf a -> Prop. \h: Prf (p x). h;
    //
    //
    //         def eq_sym = all a: Prop. all x: Prf a. all y: Prf a. all _: Prf eq a x y. eq a y x;
    //         def eq_sym_proof = (\proof: Prf eq_sym. proof) (
    //            \a: Prop. \x: Prf a. \y: Prf a.
    //               \H1: Prf eq a x y.
    //                 (H1 (\REPLACE: Prf a. eq a REPLACE x)) (intro_refl a x)
    //         );
    //
    //         eq_sym_proof
    //         "#,
    //         "",
    //         "",
    //     );
    // }

    #[test]
    fn test25() {
        simple(
            r#"
            def nat = (
              all a: Prop.
                // constructors
                // - zero
                all z: Prf a.
                // - succ
                all s: Prf a -> Prf a.
                a
            );
            type Nat = Prf nat;
            def O = (
              // value of Nat
              \a: Prop. \z: Prf a. \s: Prf a -> Prf a. z
            );
            def S = (
              // argument
              \n: Nat.
              // value of Nat
              \a: Prop. \z: Prf a. \s: Prf a -> Prf a. s (n a z s)
            );
            def plus = (
              // arguments
              \m: Nat. \n: Nat.
              \a: Prop. \z: Prf a. \s: Prf a -> Prf a.
                // Little difficult, but you can read as follows:
                // s(m') + n = s(m' + n)
                // do induction on m
                m a (
                  // if m = 0
                  n a z s
                ) (
                  // if m = S(m'), then S(itself with m is replaced with m')
                  s
                )
                // Or you can understand as usual plus implementation of Church Numerals: \z. \s. m (n z s) s
            );

            // Leibniz equality
            def eq = \a: Prop. \x: Prf a. \y: Prf a. all p: Prf a -> Prop. all h: Prf (p x). p y;
            def intro_refl = \a: Prop. \x: Prf a. \p: Prf a -> Prop. \h: Prf (p x). h;

            def nat_0_plus_n_eq_n = all n: Nat. eq nat (plus O n) n;
            def nat_0_plus_n_eq_n_proof = (\proof: Prf nat_0_plus_n_eq_n. proof) (
              // Proof:
              // ∀ n ∈ Nat
              \n: Nat.
              // reflexivity of equality
              intro_refl nat n
              // Q.E.D.
            );

            nat_0_plus_n_eq_n_proof
            "#,
            "SKIP",
            "",
        );
    }
}
