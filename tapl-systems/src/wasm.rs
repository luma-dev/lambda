use crate::common::diagnostic::{Diagnostic, DiagnosticLevel};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
#[derive(Clone)]
pub struct WasmDiagnostics {
    diagnostics: Vec<Diagnostic>,
}
impl WasmDiagnostics {
    pub fn new(diagnostics: Vec<Diagnostic>) -> Self {
        Self { diagnostics }
    }
}
#[wasm_bindgen]
impl WasmDiagnostics {
    pub fn len(&self) -> usize {
        self.diagnostics.len()
    }
    pub fn is_empty(&self) -> bool {
        self.diagnostics.is_empty()
    }
    pub fn is_error(&self) -> bool {
        self.diagnostics
            .iter()
            .any(|d| matches!(d.level(), DiagnosticLevel::Error))
    }
    pub fn to_summary(&self) -> String {
        self.diagnostics
            .iter()
            .map(|d| d.to_string())
            .collect::<Vec<_>>()
            .join("\n")
    }
}
