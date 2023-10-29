#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Term {
    True,
    False,
    If {
        cond: Box<Term>,
        then_clause: Box<Term>,
        else_clause: Box<Term>,
    },
    Zero,
    Succ(Box<Term>),
    Pred(Box<Term>),
    Iszero(Box<Term>),
}

impl Term {
    pub fn to_latex(&self) -> String {
        match self {
            Term::True => r"\mathrm{true}".to_string(),
            Term::False => r"\mathrm{false}".to_string(),
            Term::If {
                cond,
                then_clause,
                else_clause,
            } => format!(
                r"\mathrm{{if}} \medspace ({}) \medspace \mathrm{{then}} \medspace ({}) \medspace \mathrm{{else}} \medspace ({})",
                cond.to_latex(),
                then_clause.to_latex(),
                else_clause.to_latex()
            ),
            Term::Zero => "0".to_string(),
            Term::Succ(t) => format!(r"\mathrm{{succ}} ({})", t.to_latex()),
            Term::Pred(t) => format!(r"\mathrm{{pred}} ({})", t.to_latex()),
            Term::Iszero(t) => format!(r"\mathrm{{iszero}} ({})", t.to_latex()),
        }
    }
    pub fn to_code(&self, indent: &str, line_break: &str) -> String {
        self.internal_to_code(indent, line_break, 0) + line_break
    }
    fn internal_to_code(&self, indent: &str, line_break: &str, nest: usize) -> String {
        match self {
            Term::True => r"true".to_string(),
            Term::False => r"false".to_string(),
            Term::If {
                cond,
                then_clause,
                else_clause,
            } => format!(
                r"if {cond}{line_break}{indent}then {then_clause}{line_break}{indent}else {else_clause}",
                cond = cond.internal_to_code(indent, line_break, nest + 1),
                line_break = line_break,
                indent = indent.repeat(nest + 1),
                then_clause = then_clause.internal_to_code(indent, line_break, nest + 1),
                else_clause = else_clause.internal_to_code(indent, line_break, nest + 1),
            ),
            Term::Zero => "0".to_string(),
            Term::Succ(t) => {
                format!(r"succ {}", t.internal_to_code(indent, line_break, nest + 1))
            }
            Term::Pred(t) => {
                format!(r"pred {}", t.internal_to_code(indent, line_break, nest + 1))
            }
            Term::Iszero(t) => format!(
                r"iszero {}",
                t.internal_to_code(indent, line_break, nest + 1)
            ),
        }
    }
}
