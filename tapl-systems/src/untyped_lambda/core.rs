use crate::common::ToLatex;
use std::fmt::Display;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ChurchValue {
    // Church bool
    BoolTrue,
    // false is represented as 0

    // Church natural
    Nat(usize),

    // Church pair
    Pair(Box<ChurchValue>, Box<ChurchValue>),

    EvalStepLimitExceeded,
    EvalDepthLimitExceeded,

    Term,
}

impl Display for ChurchValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BoolTrue => write!(f, "true"),
            Self::Nat(n) => write!(f, "{}", n),
            Self::Pair(p1, p2) => write!(f, "({}, {})", p1, p2),
            Self::EvalStepLimitExceeded => write!(f, "?"),
            Self::EvalDepthLimitExceeded => write!(f, "..."),
            Self::Term => write!(f, "<term>"),
        }
    }
}

impl ToLatex for ChurchValue {
    fn to_latex(&self) -> String {
        match self {
            Self::BoolTrue => r"\mathrm{true}".to_string(),
            Self::Nat(n) => n.to_string(),
            Self::Pair(p1, p2) => format!(r"({},{})", p1.to_latex(), p2.to_latex()),
            Self::EvalStepLimitExceeded => r"?".to_string(),
            Self::EvalDepthLimitExceeded => r"\mathrm{<...>}".to_string(),
            Self::Term => r"\mathrm{<term>}".to_string(),
        }
    }
}
