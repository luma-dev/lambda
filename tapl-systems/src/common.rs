pub mod ast;
pub mod context;
pub mod diagnostic;
pub mod evaluator;

pub trait ToLatex {
    fn to_latex(&self) -> String;
}
