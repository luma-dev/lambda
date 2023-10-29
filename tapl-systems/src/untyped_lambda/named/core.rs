use super::ast::{Bind, Identifier, NamedTerm};
use crate::common::ToLatex;

impl NamedTerm {
    pub fn is_value(&self, var_is_val: bool) -> bool {
        match self {
            Self::Abs { .. } => true,
            Self::Var(_) => var_is_val,
            Self::Apply(t1, t2) => {
                var_is_val
                    && match t1.as_ref() {
                        Self::Abs { .. } => false,
                        _ => t1.is_value(var_is_val) && t2.is_value(var_is_val),
                    }
            }
        }
    }

    pub fn substitute(&self, bind: &Bind, to: &NamedTerm) -> NamedTerm {
        match bind {
            Bind::Identifier(id) => self.substitute_identifier(id, to),
            Bind::Wildcard => self.clone(),
        }
    }
    pub fn substitute_identifier(&self, id: &Identifier, to: &NamedTerm) -> NamedTerm {
        match self {
            Self::Var(v2) => {
                if id == v2 {
                    to.clone()
                } else {
                    self.clone()
                }
            }
            Self::Abs { binds, body } => {
                if binds
                    .iter()
                    .filter_map(|bind| match bind {
                        Bind::Identifier(id) => Some(id),
                        _ => None,
                    })
                    .any(|e| e == id)
                {
                    self.clone()
                } else {
                    Self::Abs {
                        binds: binds.clone(),
                        body: body.substitute_identifier(id, to).into(),
                    }
                }
            }
            Self::Apply(t1, t2) => Self::Apply(
                t1.substitute_identifier(id, to).into(),
                t2.substitute_identifier(id, to).into(),
            ),
        }
    }
}

impl ToLatex for NamedTerm {
    fn to_latex(&self) -> String {
        match self {
            Self::Var(v) => v.to_string(),
            Self::Abs { binds, body } => {
                let mut s = String::new();
                for bind in binds.iter().rev() {
                    s.push_str(r"\lambda ");
                    s.push_str(bind.as_str());
                    s.push_str(r" . \medspace ");
                }
                s.push_str(&body.to_latex());
                s
            }
            Self::Apply(t1, t2) => {
                let mut s = String::new();
                match &**t1 {
                    Self::Abs { .. } => {
                        s.push_str(r"\left(");
                        s.push_str(&t1.to_latex());
                        s.push_str(r"\right)");
                    }
                    _ => {
                        s.push_str(&t1.to_latex());
                    }
                }
                s.push_str(r"\medspace ");

                match &**t2 {
                    Self::Abs { .. } | Self::Apply(_, _) => {
                        s.push_str(r"\left(");
                        s.push_str(&t2.to_latex());
                        s.push_str(r"\right)");
                    }
                    _ => {
                        s.push_str(&t2.to_latex());
                    }
                }
                s
            }
        }
    }
}
