#[macro_use]
extern crate lalrpop_util;

pub mod common;
pub mod format;
pub mod typed_lambda;
pub mod untyped_arith;
pub mod untyped_lambda;
mod utils;
pub mod wasm;

use wasm_bindgen::prelude::*;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
extern "C" {
    fn alert(s: &str);
}

#[wasm_bindgen]
pub fn parse(input: &str) -> String {
    format!(
        "{:#?}",
        untyped_arith::syntax::TermParser::new().parse(input)
    )
}
