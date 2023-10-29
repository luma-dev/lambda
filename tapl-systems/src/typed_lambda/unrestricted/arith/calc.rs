use super::core as lang_core;
use crate::common::diagnostic::{Diagnostic, Spanned, SpannedWithDiagnostics};
use crate::common::evaluator::small_step::*;
use crate::format::{ctx_for_err, ToCode};
use crate::typed_lambda::unrestricted::meta::ast::Context;
use crate::wasm::WasmDiagnostics;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
#[derive(Clone)]
pub struct UnrestrictedArithCalc {
    result: Result<(Spanned<lang_core::Term>, Context, Vec<Diagnostic>), Vec<Diagnostic>>,
    current_term: Option<Spanned<lang_core::Term>>,
    evaluator: Option<lang_core::Evaluator>,
    typed: Option<Result<Spanned<lang_core::Type>, Vec<Diagnostic>>>,
    current_type: Option<Spanned<lang_core::Type>>,
    next_type: Option<Spanned<lang_core::Type>>,
}

#[wasm_bindgen]
impl UnrestrictedArithCalc {
    fn parse_inner(
        code: String,
    ) -> Result<(Spanned<lang_core::Term>, Context, Vec<Diagnostic>), Vec<Diagnostic>> {
        let SpannedWithDiagnostics {
            value: source,
            span,
            diagnostics,
        } = lang_core::Parser::new(&code)
            .start_parse_source()
            .into_result()
            .map_err(|err| err.diagnostics)?;
        let source = lang_core::Source::try_new(Spanned::new(span, source))?;
        let (term, ctx) = source.value.into_term()?;
        Ok((term, ctx, diagnostics))
    }
    pub fn parse(code: String) -> UnrestrictedArithCalc {
        let result = Self::parse_inner(code);
        let evaluator = result
            .as_ref()
            .ok()
            .map(|(_, ctx, _)| lang_core::Evaluator::new(ctx.term_def_map()));
        let current_term = result.as_ref().ok().map(|(term, _, _)| term.clone());
        UnrestrictedArithCalc {
            result,
            current_term,
            evaluator,
            typed: None,
            current_type: None,
            next_type: None,
        }
    }
    pub fn is_parse_ok(&self) -> bool {
        self.result.is_ok()
    }

    pub fn formatted_code(&self) -> String {
        self.current_term
            .as_ref()
            .map(|term| term.inner().to_code(&ctx_for_err()))
            .unwrap_or_default()
    }
    pub fn formatted_type(&self) -> String {
        self.current_type
            .as_ref()
            .map(|term| term.inner().to_code(&ctx_for_err()))
            .unwrap_or_default()
    }

    pub fn typing(&mut self) {
        if self.typed.is_some() {
            return;
        }
        if let (Ok((_, ctx, _)), Some(term)) = (&mut self.result, &self.current_term) {
            let typed = lang_core::TypeChecker::typing(ctx, term.clone());
            if let Ok(typed) = &typed {
                self.current_type = Some(typed.clone());
                self.next_type = lang_core::TypeChecker::eval1_type(ctx, typed)
                    .ok()
                    .flatten()
                    .map(|t| t.map(|t| t));
            }
            self.typed = Some(typed);
        }
    }
    pub fn is_typed(&self) -> bool {
        self.typed.is_some()
    }
    pub fn is_typing_ok(&self) -> bool {
        self.typed
            .as_ref()
            .map(|typed| typed.is_ok())
            .unwrap_or_default()
    }

    pub fn eval_type_next(&mut self) {
        if let (Ok((_, ctx, _)), Some(_), Some(_)) =
            (&mut self.result, &self.current_type, &self.next_type)
        {
            std::mem::swap(&mut self.current_type, &mut self.next_type);
            self.next_type = self.current_type.as_ref().and_then(|t| {
                lang_core::TypeChecker::eval1_type(ctx, t)
                    .ok()
                    .flatten()
                    .map(|t| t.map(|t| t))
            });
        }
    }
    pub fn eval_next(&mut self) {
        if let (Some(term), Some(e)) = (&self.current_term, &self.evaluator) {
            self.typed = None;
            self.current_term = e.eval1(term);
            self.current_type = None;
            self.next_type = None;
        }
    }

    pub fn is_normal_form(&self) -> bool {
        match (&self.current_term, &self.evaluator) {
            (Some(term), Some(evaluator)) => evaluator.is_normal_form(term),
            _ => false,
        }
    }
    pub fn type_is_normal_form(&mut self) -> bool {
        self.current_type.is_some() && self.next_type.is_none()
    }

    pub fn parse_result_diagnostics(&self) -> WasmDiagnostics {
        WasmDiagnostics::new(self.result.as_ref().err().cloned().unwrap_or_default())
    }
    pub fn typing_diagnostics(&self) -> WasmDiagnostics {
        WasmDiagnostics::new(
            self.typed
                .as_ref()
                .and_then(|typed| typed.as_ref().err().cloned())
                .unwrap_or_default(),
        )
    }

    pub fn wasm_clone(&self) -> Self {
        Clone::clone(self)
    }
}
