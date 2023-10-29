pub trait SyntacticEq {
    fn syntactic_eq(&self, other: &Self) -> bool;
}
