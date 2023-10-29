use super::ast::{
    Def, Identifier, InductionConstructor, Kind, MatchArm, Pattern, Source, SpecialIdentifier,
    Term, TermBind, Type, TypeBind,
};
use super::tokenizer::{Token, Tokenizer};
use crate::common::ast::identifier::{QualifiedIdentifier, QualifiedIdentifierPart};
use crate::common::ast::variable::{LocalVariable, Variable};
use crate::common::diagnostic::{
    merge_span, Diagnostic, DiagnosticLevel, Span, Spanned, SpannedWithDiagnostics,
};
use core::marker::PhantomData;
use num_bigint::BigUint;
use std::iter;

#[derive(Clone, Debug)]
enum Bind {
    TermBind(TermBind),
    TypeBind(TypeBind),
}

pub type ParseResultInner<T> =
    Result<SpannedWithDiagnostics<T>, SpannedWithDiagnostics<PhantomData<()>>>;
#[derive(Clone, Debug)]
pub struct ParseResult<T>(ParseResultInner<T>);
impl<T> ParseResult<T> {
    pub fn new_ok(value: T, span: Option<Span>) -> Self {
        Self(Ok(SpannedWithDiagnostics {
            value,
            span,
            diagnostics: Vec::new(),
        }))
    }
    pub fn new_err(span: Option<Span>, diagnostics: Vec<Diagnostic>) -> Self {
        Self(Err(SpannedWithDiagnostics {
            value: PhantomData,
            span,
            diagnostics,
        }))
    }
    pub fn new(value: Option<T>, span: Option<Span>, diagnostics: Vec<Diagnostic>) -> Self {
        match value {
            Some(value) => Self(Ok(SpannedWithDiagnostics {
                value,
                span,
                diagnostics,
            })),
            None => Self(Err(SpannedWithDiagnostics {
                value: PhantomData,
                span,
                diagnostics,
            })),
        }
    }
    pub fn into_spanned_option_with_diagnostics(self) -> SpannedWithDiagnostics<Option<T>> {
        match self.0 {
            Ok(spanned) => SpannedWithDiagnostics {
                value: Some(spanned.value),
                span: spanned.span,
                diagnostics: spanned.diagnostics,
            },
            Err(spanned) => SpannedWithDiagnostics {
                value: None,
                span: spanned.span,
                diagnostics: spanned.diagnostics,
            },
        }
    }
    pub fn value(&self) -> Option<&T> {
        match &self.0 {
            Ok(spanned) => Some(&spanned.value),
            Err(..) => None,
        }
    }
    pub fn span(&self) -> Option<&Span> {
        match &self.0 {
            Ok(spanned) => spanned.span.as_ref(),
            Err(spanned) => spanned.span.as_ref(),
        }
    }
    pub fn diagnostics(&self) -> &Vec<Diagnostic> {
        match &self.0 {
            Ok(spanned) => &spanned.diagnostics,
            Err(spanned) => &spanned.diagnostics,
        }
    }
    pub fn push_diagnostic(&mut self, diagnostic: Diagnostic) {
        match &mut self.0 {
            Ok(spanned) => spanned.diagnostics.push(diagnostic),
            Err(spanned) => spanned.diagnostics.push(diagnostic),
        }
    }
    pub fn into_result(self) -> ParseResultInner<T> {
        self.0
    }
    pub fn from_result(result: ParseResultInner<T>) -> Self {
        Self(result)
    }
}

fn w<F, T>(f: F) -> ParseResult<T>
where
    F: FnOnce(&mut ParseInfoBuilder) -> Option<T>,
{
    let mut b = ParseInfoBuilder::new();
    let value = f(&mut b);
    b.build(value)
}

struct ParseInfoBuilder {
    span: Option<Span>,
    diagnostics: Vec<Diagnostic>,
}
impl ParseInfoBuilder {
    fn new() -> Self {
        Self {
            span: None,
            diagnostics: Vec::new(),
        }
    }
    fn push<T>(&mut self, parse_info: ParseResult<T>) -> Option<Spanned<T>> {
        let SpannedWithDiagnostics {
            value,
            span,
            diagnostics,
        } = parse_info.into_spanned_option_with_diagnostics();
        self.span = merge_span(self.span, span);
        self.diagnostics.extend(diagnostics);
        value.map(|value| Spanned { value, span })
    }
    fn push_diagnostic<T>(&mut self, diagnostic: Diagnostic) -> Option<T> {
        self.span = merge_span(self.span, diagnostic.span().cloned());
        self.diagnostics.push(diagnostic);
        None
    }
    fn build<T>(self, value: Option<T>) -> ParseResult<T> {
        let Self { span, diagnostics } = self;
        ParseResult::new(value, span, diagnostics)
    }
}

macro_rules! term_start_token_alone {
    () => {
        Token::Identifier(..)
            | Token::Dollar
            | Token::NumberDec(..)
            | Token::NumberHex(..)
            | Token::NumberOct(..)
            | Token::NumberBin(..)
            | Token::If
            | Token::Fix
            | Token::Percent
            | Token::LParen
    };
}

#[derive(Clone, Debug, Default)]
pub struct BuiltInTermsConfig {
    pub unit: bool,
    pub bool_true: bool,
    pub bool_false: bool,
    pub succ: bool,
    pub pred: bool,
    pub is_zero: bool,
}
#[derive(Clone, Debug, Default)]
pub struct BuiltInTypesConfig {
    pub unit: bool,
    pub bool: bool,
    pub nat: bool,
    pub prop: bool,
    pub prf: bool,
}
#[derive(Clone, Debug, Default)]
pub struct ParserConfig {
    pub built_in_terms: BuiltInTermsConfig,
    pub built_in_types: BuiltInTypesConfig,
}
pub struct Parser<'a> {
    token_iter: iter::Peekable<Box<dyn Iterator<Item = Spanned<Token>> + 'a>>,
    config: ParserConfig,
}
impl<'a> Parser<'a> {
    pub fn new(input: &'a str, config: ParserConfig) -> Self {
        Self {
            token_iter: (Box::new(
                Tokenizer::new(input).filter(|e| !matches!(e.value(), Token::Comment(..))),
            ) as Box<dyn Iterator<Item = Spanned<Token>> + 'a>)
                .peekable(),
            config,
        }
    }
    fn expect_eof(&mut self) -> ParseResult<()> {
        self.must_parse_token(Token::EOF)
    }
    pub fn start_parse_source(&mut self) -> ParseResult<Source> {
        w(|b| {
            let source = b.push(self.parse_source())?.value;
            b.push(self.expect_eof())?;
            source.into()
        })
    }
    fn parse_source(&mut self) -> ParseResult<Source> {
        w(|b| {
            let mut defs = Vec::new();
            while let Some(token) = self.token_iter.peek().map(|t| t.value()) {
                match token {
                    Token::Def => {
                        defs.push(b.push(self.parse_def_term())?);
                    }
                    Token::Type => {
                        defs.push(b.push(self.parse_def_type())?);
                    }
                    Token::Axiom => {
                        defs.push(b.push(self.parse_def_axiom())?);
                    }
                    Token::Ind => {
                        defs.push(b.push(self.parse_def_ind())?);
                    }
                    _ => break,
                }
            }
            let term = b.push(self.parse_term(false, false))?;
            Source { defs, term }.into()
        })
    }
    fn parse_def_term(&mut self) -> ParseResult<Def> {
        w(|b| {
            b.push(self.parse_token(Token::Def))?;
            let inline = if self.token_iter.peek().unwrap().value == Token::Exclamation {
                b.push(self.parse_token(Token::Exclamation))?;
                true
            } else {
                false
            };
            let name = b.push(self.parse_identifier())?;
            b.push(self.parse_token(Token::Equal))?;
            let term = b.push(self.parse_term(true, false))?;
            b.push(self.parse_token(Token::Semicolon))?;
            Def::TermDef { name, term, inline }.into()
        })
    }
    fn parse_def_type(&mut self) -> ParseResult<Def> {
        w(|b| {
            b.push(self.parse_token(Token::Type))?;
            let inline = if self.token_iter.peek().unwrap().value == Token::Exclamation {
                b.push(self.parse_token(Token::Exclamation))?;
                true
            } else {
                false
            };
            let name = b.push(self.parse_identifier())?;
            b.push(self.parse_token(Token::Equal))?;
            let typ = b.push(self.parse_type())?;
            b.push(self.parse_token(Token::Semicolon))?;
            Def::TypeDef { name, typ, inline }.into()
        })
    }
    fn parse_def_axiom(&mut self) -> ParseResult<Def> {
        w(|b| {
            b.push(self.parse_token(Token::Axiom))?;
            let name = b.push(self.parse_identifier())?;
            b.push(self.parse_token(Token::Colon))?;
            let typ = b.push(self.parse_type())?;
            b.push(self.parse_token(Token::Semicolon))?;
            Def::AxiomDef { name, typ }.into()
        })
    }
    fn parse_def_ind(&mut self) -> ParseResult<Def> {
        w(|b| {
            b.push(self.parse_token(Token::Ind))?;
            let name = b.push(self.parse_identifier())?;
            b.push(self.parse_token(Token::LBrace))?;
            let mut constructors = Vec::new();
            while self.token_iter.peek().map(|t| t.value()) != Some(&Token::RBrace) {
                constructors.push(b.push(self.parse_def_ind_constructor())?);
            }
            b.push(self.parse_token(Token::RBrace))?;
            Def::InductionDef { name, constructors }.into()
        })
    }
    fn parse_def_ind_constructor(&mut self) -> ParseResult<InductionConstructor> {
        w(|b| {
            let name = b.push(self.parse_identifier())?;
            b.push(self.parse_token(Token::Colon))?;
            let typ = b.push(self.parse_type())?;
            b.push(self.parse_token(Token::Comma))?;
            InductionConstructor { name, typ }.into()
        })
    }

    fn parse_identifier(&mut self) -> ParseResult<Identifier> {
        let Spanned { value: token, span } = self.token_iter.next().unwrap();
        match token {
            Token::Identifier(id) => ParseResult::new_ok(Identifier::new(id), span),
            _ => ParseResult::new(
                None,
                span,
                vec![Diagnostic::new(
                    DiagnosticLevel::Error,
                    format!("expected identifier, but found {:?}", token),
                    span,
                )],
            ),
        }
    }
    fn parse_special_identifier(&mut self) -> ParseResult<SpecialIdentifier> {
        w(|b| {
            b.push(self.parse_token(Token::Dollar))?;
            let id = b.push(self.parse_identifier())?;
            SpecialIdentifier::from_identifier(id.value).into()
        })
    }

    fn parse_token(&mut self, token: Token) -> ParseResult<()> {
        let Spanned { value: next, span } = self.token_iter.next().unwrap();
        if next != token {
            let diag = Diagnostic::new(
                DiagnosticLevel::Error,
                format!("expected {:?}, but found {:?}", token, next),
                span,
            );
            ParseResult::new(Some(()), span, vec![diag])
        } else {
            ParseResult::new_ok((), span)
        }
    }

    fn must_parse_token(&mut self, token: Token) -> ParseResult<()> {
        let Spanned { value: next, span } = self.token_iter.next().unwrap();
        if next != token {
            let diag = Diagnostic::new(
                DiagnosticLevel::Error,
                format!("expected {:?}, but found {:?}", token, next),
                span,
            );
            ParseResult::new_err(span, vec![diag])
        } else {
            ParseResult::new_ok((), span)
        }
    }

    fn parse_token_or_hint<F>(&mut self, token: Token, hinter: F) -> ParseResult<()>
    where
        F: Fn(&Token) -> Option<String>,
    {
        let Spanned { value: next, span } = self.token_iter.next().unwrap();
        if next != token {
            let mut diags = vec![Diagnostic::new(
                DiagnosticLevel::Error,
                format!("expected {:?}, but found {:?}", token, next),
                span,
            )];
            let hint = hinter(&next);
            if let Some(hint) = hint {
                diags.push(Diagnostic::new(DiagnosticLevel::Hint, hint, span));
            }
            ParseResult::new(Some(()), span, diags)
        } else {
            ParseResult::new_ok((), span)
        }
    }

    fn parse_term(&mut self, inline: bool, no_app: bool) -> ParseResult<Term> {
        w(|b| {
            let mut term = {
                let Spanned { value: token, .. } = self.token_iter.peek().unwrap();
                match token {
                    Token::Dollar => b.push(self.parse_term_var()),
                    Token::Backslash => b.push(self.parse_term_abs()),
                    Token::All => b.push(self.parse_term_all()),
                    Token::Ex => b.push(self.parse_term_ex()),

                    Token::Let => b.push(self.parse_term_let(inline)),
                    Token::Match => b.push(self.parse_term_match()),
                    Token::Identifier(id) => match id.as_str() {
                        "unit" if self.config.built_in_terms.unit => {
                            let id = b.push(self.parse_identifier())?;
                            Some(Spanned::new(id.span, Term::Unit))
                        }
                        "succ" if self.config.built_in_terms.succ => {
                            let id = b.push(self.parse_identifier())?;
                            Some(Spanned::new(id.span, Term::NatSucc))
                        }
                        "pred" if self.config.built_in_terms.pred => {
                            let id = b.push(self.parse_identifier())?;
                            Some(Spanned::new(id.span, Term::NatPred))
                        }
                        "is_zero" if self.config.built_in_terms.is_zero => {
                            let id = b.push(self.parse_identifier())?;
                            Some(Spanned::new(id.span, Term::NatIsZero))
                        }
                        "true" if self.config.built_in_terms.bool_true => {
                            let id = b.push(self.parse_identifier())?;
                            Some(Spanned::new(id.span, Term::Bool(true)))
                        }
                        "false" if self.config.built_in_terms.bool_false => {
                            let id = b.push(self.parse_identifier())?;
                            Some(Spanned::new(id.span, Term::Bool(false)))
                        }
                        _ => b.push(self.parse_term_var()),
                    },

                    Token::If => b.push(self.parse_term_if()),
                    Token::Fix => b.push(self.parse_term_fix()),

                    Token::NumberDec(..)
                    | Token::NumberHex(..)
                    | Token::NumberOct(..)
                    | Token::NumberBin(..) => b.push(self.parse_term_nat_value()),

                    Token::LParen => b.push(self.parse_term_parenthesized()),
                    _ => {
                        let Spanned { value: token, span } = self.token_iter.next().unwrap();
                        b.push_diagnostic(Diagnostic::new(
                            DiagnosticLevel::Error,
                            format!("while parsing term, found unexpected token {:?}", token),
                            span,
                        ))
                    }
                }?
            };
            loop {
                let Spanned { value: token, .. } = self.token_iter.peek().unwrap();
                match token {
                    term_start_token_alone!() if !no_app => {
                        let term2 = b.push(self.parse_term(true, true))?;
                        term = Spanned::new(
                            merge_span(term.span, term2.span),
                            Term::ApplyTerm(term.into(), term2.into()),
                        );
                    }
                    Token::LBracket => {
                        self.token_iter.next();
                        let typ = b.push(self.parse_type())?;
                        let last = b.push(self.parse_token(Token::RBracket))?;
                        term = Spanned::new(
                            merge_span(term.span, last.span),
                            Term::ApplyType(term.into(), typ.into()),
                        );
                    }
                    Token::Semicolon if !inline => {
                        self.token_iter.next();
                        let term2 = b.push(self.parse_term(inline, no_app))?;
                        term = Spanned::new(
                            merge_span(term.span, term2.span),
                            Term::Seq(term.into(), term2.into()),
                        );
                    }
                    _ => break,
                }
            }
            term.value.into()
        })
    }
    fn parse_term_parenthesized(&mut self) -> ParseResult<Term> {
        w(|b| {
            b.push(self.parse_token(Token::LParen))?;
            let term = b.push(self.parse_term(false, false))?;
            b.push(self.parse_token_or_hint(Token::RParen, |token| {
                match token {
                    Token::Backslash => "to apply a lambda, should always parenthesize"
                        .to_string()
                        .into(),
                    _ => None,
                }
            }))?;
            term.value.into()
        })
    }

    fn parse_term_var(&mut self) -> ParseResult<Term> {
        w(|b| {
            // let qid = b.push(self.parse_term_qid( ))?;
            let var = b.push(self.parse_variable())?;
            Term::Var(var).into()
        })
    }

    fn parse_variable(&mut self) -> ParseResult<Variable> {
        w(|b| {
            let Spanned { value: token, .. } = self.token_iter.peek().unwrap();
            if token == &Token::Dollar {
                let sid = b.push(self.parse_special_identifier())?;
                let qid = Spanned::new(sid.span, QualifiedIdentifier::new_head_special(sid));
                self.parse_variable_loop_from_qid(b, qid)
            } else {
                let id = b.push(self.parse_identifier())?;
                self.parse_variable_loop_from_id(b, id)
            }
        })
    }
    fn parse_variable_loop_from_id(
        &mut self,
        b: &mut ParseInfoBuilder,
        id: Spanned<Identifier>,
    ) -> Option<Variable> {
        let Spanned { value: token, .. } = self.token_iter.peek().unwrap();
        if !matches!(token, Token::ColonColon) {
            return Some(Variable::new_local(id.value, Default::default()));
        }

        let qid = Spanned::new(
            id.span,
            QualifiedIdentifier::Head(id.map(QualifiedIdentifierPart::Normal)),
        );
        self.parse_variable_loop_from_qid(b, qid)
    }
    fn parse_variable_loop_from_qid(
        &mut self,
        b: &mut ParseInfoBuilder,
        mut qid: Spanned<QualifiedIdentifier>,
    ) -> Option<Variable> {
        loop {
            let Spanned { value: token, .. } = self.token_iter.peek().unwrap();
            match token {
                Token::ColonColon => {
                    self.token_iter.next().unwrap();
                    // let id = b.push(self.parse_identifier( ))?;
                    let part = b.push(self.parse_term_qid_part())?;
                    qid = Spanned::new(
                        merge_span(qid.span, part.span),
                        QualifiedIdentifier::Sub(qid.into(), part),
                    );
                }
                _ => break,
            }
        }
        Variable::new_qualified(qid.value).into()
    }
    fn parse_term_qid_part(&mut self) -> ParseResult<QualifiedIdentifierPart> {
        w(|b| {
            let Spanned { value: token, .. } = self.token_iter.peek().unwrap();
            match token {
                Token::Dollar => {
                    self.token_iter.next().unwrap();
                    let id = b.push(self.parse_identifier())?;
                    QualifiedIdentifierPart::Special(SpecialIdentifier::from_identifier(id.value))
                        .into()
                }
                _ => {
                    let id = b.push(self.parse_identifier())?;
                    QualifiedIdentifierPart::Normal(id.value).into()
                }
            }
        })
    }

    fn parse_term_abs(&mut self) -> ParseResult<Term> {
        w(|b| {
            b.push(self.parse_token(Token::Backslash))?;

            let Spanned { value: bind, span } = b.push(self.parse_bind())?;
            match bind {
                Bind::TermBind(term_bind) => {
                    b.push(self.parse_token(Token::Dot))?;
                    let term = b.push(self.parse_term(true, false))?;
                    Term::AbsTerm {
                        term_bind: Spanned::new(span, term_bind),
                        body: term.into(),
                    }
                    .into()
                }
                Bind::TypeBind(type_bind) => {
                    b.push(self.parse_token(Token::Dot))?;
                    let term = b.push(self.parse_term(true, false))?;
                    Term::AbsType {
                        type_bind: Spanned::new(span, type_bind),
                        body: term.into(),
                    }
                    .into()
                }
            }
        })
    }
    fn parse_term_all(&mut self) -> ParseResult<Term> {
        w(|b| {
            b.push(self.parse_token(Token::All))?;
            let term_bind = b.push(self.parse_term_bind())?;
            b.push(self.parse_token(Token::Dot))?;
            let term = b.push(self.parse_term(true, false))?;
            Term::All {
                term_bind,
                body: term.into(),
            }
            .into()
        })
    }
    fn parse_term_ex(&mut self) -> ParseResult<Term> {
        w(|b| {
            b.push(self.parse_token(Token::Ex))?;
            let term_bind = b.push(self.parse_term_bind())?;
            b.push(self.parse_token(Token::Dot))?;
            let term = b.push(self.parse_term(true, false))?;
            Term::Ex {
                term_bind,
                body: term.into(),
            }
            .into()
        })
    }
    fn parse_term_match(&mut self) -> ParseResult<Term> {
        w(|b| {
            b.push(self.parse_token(Token::Match))?;
            let term = b.push(self.parse_term(false, false))?;
            b.push(self.parse_token(Token::LBracket))?;
            let mut arms = Vec::new();
            while self.token_iter.peek().map(|s| s.value()) != Some(&Token::RBracket) {
                let arm = b.push(self.parse_term_match_arm())?;
                arms.push(arm);
            }
            b.push(self.parse_token(Token::RBracket))?;
            Term::Match {
                scrutinee: term.into(),
                arms,
            }
            .into()
        })
    }
    fn parse_term_match_arm(&mut self) -> ParseResult<MatchArm> {
        w(|b| {
            let constructor = b.push(self.parse_identifier())?;
            let mut binds = Vec::new();
            while self.token_iter.peek().map(|s| s.value()) == Some(&Token::RightArrowDouble) {
                let bind = b.push(self.parse_pattern())?;
                binds.push(bind);
            }

            b.push(self.parse_token(Token::RightArrowDouble))?;

            let term = b.push(self.parse_term(false, false))?;

            b.push(self.parse_token(Token::Comma))?;

            MatchArm {
                constructor,
                args: binds,
                body: term,
            }
            .into()
        })
    }
    fn parse_term_nat_value(&mut self) -> ParseResult<Term> {
        let Spanned { value: token, span } = self.token_iter.next().unwrap();
        match token {
            Token::NumberDec(dec) => ParseResult::new_ok(
                {
                    let bytes = dec.as_bytes();
                    if bytes.is_empty() {
                        return ParseResult::new_err(span, {
                            let diag = Diagnostic::new(
                                DiagnosticLevel::Error,
                                "zero length decimal nat literal token is not allowed".to_string(),
                                span,
                            );
                            vec![diag]
                        });
                    }
                    Term::Nat(BigUint::parse_bytes(bytes, 10).unwrap())
                },
                span,
            ),
            Token::NumberHex(hex) => ParseResult::new_ok(
                {
                    let bytes = hex.as_bytes();
                    if bytes.is_empty() {
                        return ParseResult::new_err(span, {
                            let diag = Diagnostic::new(
                                DiagnosticLevel::Error,
                                "zero length hexical nat literal token (0x) is not allowed"
                                    .to_string(),
                                span,
                            );
                            vec![diag]
                        });
                    }
                    Term::Nat(BigUint::parse_bytes(bytes, 16).unwrap())
                },
                span,
            ),
            Token::NumberOct(oct) => ParseResult::new_ok(
                {
                    let bytes = oct.as_bytes();
                    if bytes.is_empty() {
                        return ParseResult::new_err(span, {
                            let diag = Diagnostic::new(
                                DiagnosticLevel::Error,
                                "zero length octonary nat literal token (0o) is not allowed"
                                    .to_string(),
                                span,
                            );
                            vec![diag]
                        });
                    }
                    Term::Nat(BigUint::parse_bytes(bytes, 8).unwrap())
                },
                span,
            ),
            Token::NumberBin(bin) => ParseResult::new_ok(
                Term::Nat({
                    let bytes = bin.as_bytes();
                    if bytes.is_empty() {
                        return ParseResult::new_err(span, {
                            let diag = Diagnostic::new(
                                DiagnosticLevel::Error,
                                "zero length binary nat literal token (0b) is not allowed"
                                    .to_string(),
                                span,
                            );
                            vec![diag]
                        });
                    }
                    BigUint::parse_bytes(bytes, 2).unwrap()
                }),
                span,
            ),
            _ => ParseResult::new(
                None,
                span,
                vec![Diagnostic::new(
                    DiagnosticLevel::Error,
                    format!("expected nat value, found {:?}", token),
                    span,
                )],
            ),
        }
    }
    fn parse_term_if(&mut self) -> ParseResult<Term> {
        w(|b| {
            b.push(self.parse_token(Token::If))?;
            let cond = b.push(self.parse_term(false, false))?;
            b.push(self.parse_token(Token::LBrace))?;
            let then_clause = b.push(self.parse_term(false, false))?;
            b.push(self.parse_token(Token::RBrace))?;
            b.push(self.parse_token(Token::Else))?;
            b.push(self.parse_token(Token::LBrace))?;
            let else_clause = b.push(self.parse_term(false, false))?;
            b.push(self.parse_token(Token::RBrace))?;
            Term::BoolIf {
                cond: cond.into(),
                then_clause: then_clause.into(),
                else_clause: else_clause.into(),
            }
            .into()
        })
    }

    fn parse_term_fix(&mut self) -> ParseResult<Term> {
        w(|b| {
            b.push(self.parse_token(Token::Fix))?;
            let Spanned { value: token, span } = self.token_iter.peek().unwrap();
            match token {
                term_start_token_alone!() => {
                    let term = b.push(self.parse_term(true, true))?;
                    Term::Fix(term.into()).into()
                }
                _ => {
                    b.push_diagnostic::<()>(Diagnostic::new(
                        DiagnosticLevel::Error,
                        format!("expected term, but found {:?}", token),
                        *span,
                    ));
                    let hint = match token {
                        Token::Backslash => Some("to fix a lambda, should always parenthesize"),
                        _ => None,
                    };
                    if let Some(hint) = hint {
                        b.push_diagnostic::<()>(Diagnostic::new(
                            DiagnosticLevel::Hint,
                            hint.to_string(),
                            *span,
                        ));
                    }
                    None
                }
            }
        })
    }

    fn parse_bind(&mut self) -> ParseResult<Bind> {
        w(|b| {
            let pat = b.push(self.parse_pattern())?;
            match self.token_iter.peek().map(|s| s.value()) {
                Some(Token::Colon) => {
                    b.push(self.parse_token(Token::Colon))?;
                    let typ = b.push(self.parse_type())?;
                    Bind::TermBind(TermBind {
                        pat,
                        typ: typ.into(),
                    })
                    .into()
                }
                Some(Token::ColonColon) => {
                    b.push(self.parse_token(Token::ColonColon))?;
                    let kind = b.push(self.parse_kind())?;
                    Bind::TypeBind(TypeBind {
                        pat,
                        kind: kind.into(),
                    })
                    .into()
                }
                _ => {
                    b.push_diagnostic::<()>(Diagnostic::new(
                        DiagnosticLevel::Error,
                        "expected token `:` or `::` after pattern, but found none".to_string(),
                        pat.span,
                    ));
                    None
                }
            }
        })
    }
    fn parse_term_bind(&mut self) -> ParseResult<TermBind> {
        w(|b| {
            let pat = b.push(self.parse_pattern())?;
            b.push(self.parse_token_or_hint(Token::Colon, |token| match token {
                Token::ColonColon => {
                    Some("Here, type binding is not allowed. Use term binding instead.".to_string())
                }
                _ => None,
            }))?;
            let typ = b.push(self.parse_type())?;
            TermBind {
                pat,
                typ: typ.into(),
            }
            .into()
        })
    }
    fn parse_type_bind(&mut self) -> ParseResult<TypeBind> {
        w(|b| {
            let pat = b.push(self.parse_pattern())?;
            b.push(
                self.parse_token_or_hint(Token::ColonColon, |token| match token {
                    Token::Colon => Some(
                        "Here, term binding is not allowed. Use type binding instead.".to_string(),
                    ),
                    _ => None,
                }),
            )?;
            let kind = b.push(self.parse_kind())?;
            TypeBind {
                pat,
                kind: kind.into(),
            }
            .into()
        })
    }
    fn parse_pattern(&mut self) -> ParseResult<Pattern> {
        let Spanned { value: token, span } = self.token_iter.next().unwrap();
        match token {
            Token::Identifier(id) => {
                let id = Identifier::new(id);
                let name = Spanned::new(span, LocalVariable::new(id, Default::default()));
                ParseResult::new_ok(Pattern::Named(name), span)
            }
            Token::Wildcard => ParseResult::new_ok(Pattern::Wildcard, span),
            _ => ParseResult::new_err(span, {
                let diag = Diagnostic::new(
                    DiagnosticLevel::Error,
                    format!(
                        "while parsing term binding, expected token identifier or wildcard, but found token {:?}",
                        token
                    ),
                    span,
                );
                vec![diag]
            }),
        }
    }
    fn parse_term_let(&mut self, inline: bool) -> ParseResult<Term> {
        w(|b| {
            b.push(self.parse_token(Token::Let))?;
            let pat = b.push(self.parse_pattern())?;
            b.push(self.parse_token(Token::Equal))?;
            let term = b.push(self.parse_term(true, false))?;
            b.push(self.parse_token(Token::Semicolon))?;
            let body = b.push(self.parse_term(inline, false))?;
            Term::Let {
                pat,
                val: term.into(),
                body: body.into(),
            }
            .into()
        })
    }

    pub fn start_parse_type(&mut self) -> ParseResult<Type> {
        w(|b| {
            let typ = b.push(self.parse_type())?.value;
            b.push(self.expect_eof())?;
            typ.into()
        })
    }
    fn parse_type(&mut self) -> ParseResult<Type> {
        w(|b| {
            let mut typ1 = {
                let Spanned { value: token, .. } = self.token_iter.peek().unwrap();
                match token {
                    Token::LParen => b.push(self.parse_type_parenthesized()),
                    Token::Identifier(id) => match id.as_str() {
                        "Unit" if self.config.built_in_types.unit => b.push(self.parse_type_unit()),
                        "Nat" if self.config.built_in_types.nat => b.push(self.parse_type_nat()),
                        "Bool" if self.config.built_in_types.bool => b.push(self.parse_type_bool()),
                        "Prop" if self.config.built_in_types.prop => b.push(self.parse_type_prop()),
                        "Prf" if self.config.built_in_types.prf => b.push(self.parse_type_prf()),
                        _ => b.push(self.parse_type_var()),
                    },
                    Token::Pi => b.push(self.parse_type_pi()),
                    Token::Forall => b.push(self.parse_type_forall()),
                    Token::Backslash => b.push(self.parse_type_abs_type()),
                    _ => {
                        let Spanned { value: token, span } = self.token_iter.next().unwrap();
                        b.push_diagnostic(Diagnostic::new(
                            DiagnosticLevel::Error,
                            format!("while parsing type, found unexpected token {:?}", token),
                            span,
                        ))
                    }
                }?
            };
            loop {
                let Spanned { value: token, .. } = self.token_iter.peek().unwrap();
                match token {
                    term_start_token_alone!() => {
                        let t = b.push(self.parse_term(true, false))?;
                        typ1 = Spanned::new(
                            merge_span(typ1.span, t.span),
                            Type::ApplyTerm(typ1.into(), t),
                        );
                    }
                    Token::LBracket => {
                        self.token_iter.next();
                        let typ2 = b.push(self.parse_type())?;
                        let last = b.push(self.parse_token(Token::RBracket))?;
                        typ1 = Spanned::new(
                            merge_span(typ1.span, last.span),
                            Type::ApplyType(typ1.into(), typ2.into()),
                        );
                    }
                    _ => break,
                }
            }

            let Spanned { value: token, .. } = self.token_iter.peek().unwrap();
            if token == &Token::RightArrowSingle {
                self.token_iter.next();
                let typ2 = b.push(self.parse_type())?;
                Type::new_arrow(typ1, typ2)
            } else {
                typ1.value
            }
            .into()
        })
    }
    fn parse_type_parenthesized(&mut self) -> ParseResult<Type> {
        w(|b| {
            b.push(self.parse_token(Token::LParen))?;
            let typ = b.push(self.parse_type())?;
            b.push(self.parse_token(Token::RParen))?;
            typ.value.into()
        })
    }
    fn parse_type_var(&mut self) -> ParseResult<Type> {
        w(|b| {
            let id = b.push(self.parse_identifier())?;
            Type::Var(id).into()
        })
    }
    fn parse_type_abs_type(&mut self) -> ParseResult<Type> {
        w(|b| {
            b.push(self.parse_token(Token::Backslash))?;
            let type_bind = b.push(self.parse_type_bind())?;
            b.push(self.parse_token(Token::Dot))?;
            let body = b.push(self.parse_type())?;
            Type::AbsType {
                type_bind,
                body: body.into(),
            }
            .into()
        })
    }
    fn parse_type_nat(&mut self) -> ParseResult<Type> {
        w(|b| {
            b.push(self.parse_identifier())?;
            Type::Nat.into()
        })
    }
    fn parse_type_bool(&mut self) -> ParseResult<Type> {
        w(|b| {
            b.push(self.parse_identifier())?;
            Type::Bool.into()
        })
    }
    fn parse_type_unit(&mut self) -> ParseResult<Type> {
        w(|b| {
            b.push(self.parse_identifier())?;
            Type::Unit.into()
        })
    }
    fn parse_type_prop(&mut self) -> ParseResult<Type> {
        w(|b| {
            b.push(self.parse_identifier())?;
            Type::Prop.into()
        })
    }
    fn parse_type_prf(&mut self) -> ParseResult<Type> {
        w(|b| {
            b.push(self.parse_identifier())?;
            Type::Prf.into()
        })
    }
    fn parse_type_pi(&mut self) -> ParseResult<Type> {
        w(|b| {
            b.push(self.parse_token(Token::Pi))?;
            let term_bind = b.push(self.parse_term_bind())?;
            b.push(self.parse_token(Token::Dot))?;
            let typ = b.push(self.parse_type())?;

            Type::Pi {
                term_bind: term_bind.into(),
                body: typ.into(),
            }
            .into()
        })
    }
    fn parse_type_forall(&mut self) -> ParseResult<Type> {
        w(|b| {
            b.push(self.parse_token(Token::Forall))?;
            let type_bind = b.push(self.parse_type_bind())?;
            b.push(self.parse_token(Token::Dot))?;
            let typ = b.push(self.parse_type())?;

            Type::Forall {
                type_bind,
                body: typ.into(),
            }
            .into()
        })
    }

    fn parse_kind(&mut self) -> ParseResult<Kind> {
        w(|b| {
            let kind1 = {
                let Spanned { value: token, .. } = self.token_iter.peek().unwrap();
                match token {
                    Token::Asterisk => b.push(self.parse_kind_asterisk())?,
                    Token::LParen => b.push(self.parse_kind_parenthesized())?,
                    _ => {
                        let Spanned { value: token, span } = self.token_iter.next().unwrap();
                        b.push_diagnostic(Diagnostic::new(
                            DiagnosticLevel::Error,
                            format!("while parsing kind, found unexpected token {:?}", token),
                            span,
                        ))?
                    }
                }
            };
            let Spanned { value: token, .. } = self.token_iter.peek().unwrap();
            if token == &Token::RightArrowDouble {
                b.push(self.parse_token(Token::RightArrowDouble))?;
                let kind2 = b.push(self.parse_kind())?;
                Kind::Arrow(kind1.into(), kind2.into())
            } else {
                kind1.value
            }
            .into()
        })
    }
    fn parse_kind_asterisk(&mut self) -> ParseResult<Kind> {
        w(|b| {
            b.push(self.parse_token(Token::Asterisk))?;
            Kind::ProperTypes.into()
        })
    }
    fn parse_kind_parenthesized(&mut self) -> ParseResult<Kind> {
        w(|b| {
            b.push(self.parse_token(Token::LParen))?;
            let kind = b.push(self.parse_kind())?;
            b.push(self.parse_token(Token::RParen))?;
            kind.value.into()
        })
    }
}

#[cfg(test)]
mod tests_source {
    use super::*;
    // use crate::format::{FormatContext, ToCode};

    fn full_parser_config() -> ParserConfig {
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
                prop: true,
                prf: true,
            },
        }
    }

    fn fp(s: &str) -> ParseResultInner<Source> {
        let mut parser = Parser::new(s, full_parser_config());
        parser.start_parse_source().into_result()
    }
    // fn p(s: &str) -> SpannedWithDiagnostics<Source> {
    //     let mut parser = Parser::new(s);
    //     parser.parse_source().into_result().unwrap()
    // }
    fn f(s: &str) {
        let r = fp(s);
        assert!(r.is_err());
    }
    // fn e(s: &str, s2: &str) {
    //     let SpannedWithDiagnostics {
    //         value: typ,
    //         diagnostics,
    //         ..
    //     } = p(s);
    //     assert_eq!(diagnostics.len(), 0, "{:?}", diagnostics);
    //     assert_eq!(typ.to_code(&FormatContext::new_compact()), s2);
    // }
    // fn s(s: &str) {
    //     e(s, s);
    // }

    #[test]
    fn test_fail_1() {
        f("0b");
        f("0x");
        f("0b");

        f(r"\x");
    }
}
#[cfg(test)]
mod tests_type {
    use super::*;
    use crate::format::{FormatContext, ToCode};

    fn full_parser_config() -> ParserConfig {
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
                prop: true,
                prf: true,
            },
        }
    }

    fn p(s: &str) -> SpannedWithDiagnostics<Type> {
        let mut parser = Parser::new(s, full_parser_config());
        parser.start_parse_type().into_result().unwrap()
    }
    fn e(s: &str, s2: &str) {
        let SpannedWithDiagnostics {
            value: typ,
            diagnostics,
            ..
        } = p(s);
        // let free_term_vars = typ.free_term_vars();
        let free_type_vars = typ.free_type_vars();
        // assert_eq!(
        //     free_term_vars.len(),
        //     0,
        //     "expected no free term vars: but got {:?}",
        //     free_term_vars
        // );
        assert_eq!(
            free_type_vars.len(),
            0,
            "expected no free type vars: but got {:?}",
            free_type_vars
        );
        assert_eq!(diagnostics.len(), 0, "{:?}", diagnostics);
        assert_eq!(typ.to_code(&FormatContext::new_compact()), s2);
    }
    fn s(s: &str) {
        e(s, s);
    }

    #[test]
    fn test_nat_nat_nat_1() {
        s("Nat -> Nat -> Nat");
    }
    #[test]
    fn test_nat_nat_nat_2() {
        e("Nat -> (Nat -> Nat)", "Nat -> Nat -> Nat");
    }
    #[test]
    fn test_nat_nat_nat_3() {
        s("(Nat -> Nat) -> Nat");
    }

    #[test]
    fn test_app_type_term_1() {
        e("Nat[Nat](pred(0))", "Nat[Nat] (pred 0)");
    }
    #[test]
    fn test_app_type_term_2() {
        s("Nat[Nat] (pred 0)");
    }
    #[test]
    fn test_pi_1() {
        s("pi n: Nat. Nat (pred n)");
    }
    #[test]
    fn test_pi_2() {
        e("pi _: Nat. Nat", "Nat -> Nat");
    }

    #[test]
    fn test_forall_1() {
        s(r"forall X:: *. X");
    }
    #[test]
    fn test_forall_2() {
        s(r"forall X:: * => *. X");
    }
    #[test]
    fn test_forall_3() {
        s(r"forall X:: * => * => * => *. X");
    }
    #[test]
    fn test_forall_4() {
        s(r"forall X:: * => (* => *) => ((* => *) => *) => *. X");
    }
    #[test]
    fn test_forall_5() {
        e(r"forall X:: * => (* => *). X", r"forall X:: * => * => *. X");
    }
}
