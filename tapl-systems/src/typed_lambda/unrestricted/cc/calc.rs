use super::core as lang_core;
use crate::common::diagnostic::{Diagnostic, Spanned, SpannedWithDiagnostics};
use crate::format::{ctx_for_err, ToCode};
use crate::typed_lambda::unrestricted::meta::ast::{Context, GrammaticalSize};
use crate::wasm::WasmDiagnostics;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
#[derive(Clone)]
pub struct LambdaCcCalc {
    result: Result<(Spanned<lang_core::Term>, Context, Vec<Diagnostic>), Vec<Diagnostic>>,
    evaled: Option<Result<Spanned<lang_core::Term>, Vec<Diagnostic>>>,
    typed: Option<Result<Spanned<lang_core::Type>, Vec<Diagnostic>>>,
}

#[wasm_bindgen]
impl LambdaCcCalc {
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
    pub fn parse(code: String) -> LambdaCcCalc {
        LambdaCcCalc {
            result: Self::parse_inner(code),
            evaled: None,
            typed: None,
        }
    }
    pub fn to_code(&self) -> String {
        self.result
            .as_ref()
            .ok()
            .map(|(term, ctx, _)| {
                let ctx_code = ctx.to_code(&ctx_for_err());
                let term_code = term.to_code(&ctx_for_err());
                format!("{ctx_code}\n\n{term_code}")
            })
            .unwrap_or_default()
    }
    pub fn evaled_to_code(&self) -> String {
        self.evaled
            .as_ref()
            .and_then(|t| t.as_ref().ok().map(|t| t.to_code(&ctx_for_err())))
            .unwrap_or_default()
    }
    pub fn typed_to_code(&self) -> String {
        self.typed
            .as_ref()
            .and_then(|t| t.as_ref().ok().map(|t| t.to_code(&ctx_for_err())))
            .unwrap_or_default()
    }
    pub fn eval(&mut self) {
        if let Ok((term, ctx, _)) = &mut self.result {
            if self.evaled.is_some() {
                return;
            }
            self.evaled = Some(lang_core::TypeChecker::eval_term(ctx, term));
        }
    }
    pub fn typing(&mut self) {
        if let Ok((term, ctx, _)) = &mut self.result {
            if self.typed.is_some() {
                return;
            }
            self.typed = Some(lang_core::TypeChecker::typing(ctx, term));
        }
    }
    pub fn is_parse_ok(&self) -> bool {
        self.result.is_ok()
    }
    pub fn is_evaled(&self) -> bool {
        self.evaled.is_some()
    }
    pub fn is_typed(&self) -> bool {
        self.typed.is_some()
    }
    pub fn is_evaled_ok(&self) -> bool {
        matches!(&self.evaled, Some(Ok(_)))
    }
    pub fn is_typed_ok(&self) -> bool {
        matches!(&self.typed, Some(Ok(_)))
    }
    pub fn grammatical_size(&self) -> usize {
        self.result
            .as_ref()
            .ok()
            .map(|t| t.0.inner().grammatical_size())
            .unwrap_or_default()
    }
    pub fn parse_result_diagnostics(&self) -> WasmDiagnostics {
        WasmDiagnostics::new(self.result.as_ref().err().cloned().unwrap_or_default())
    }
    pub fn typing_diagnostics(&self) -> WasmDiagnostics {
        WasmDiagnostics::new(
            self.typed
                .as_ref()
                .and_then(|t| t.as_ref().err().cloned())
                .unwrap_or_default(),
        )
    }
    pub fn eval_diagnostics(&self) -> WasmDiagnostics {
        WasmDiagnostics::new(
            self.evaled
                .as_ref()
                .and_then(|t| t.as_ref().err().cloned())
                .unwrap_or_default(),
        )
    }
}
