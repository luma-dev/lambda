pub enum FormatContext {
    Pretty {
        new_line: String,
        indent: String,
        current_indent: String,
    },
    Compact,
}
impl FormatContext {
    pub fn new_pretty(new_line: &str, indent: &str) -> Self {
        Self::Pretty {
            new_line: new_line.to_string(),
            indent: indent.to_string(),
            current_indent: String::new(),
        }
    }
    pub fn new_compact() -> Self {
        Self::Compact
    }
    pub fn indented(&self) -> Self {
        match self {
            Self::Pretty {
                new_line,
                indent,
                current_indent,
            } => Self::Pretty {
                new_line: new_line.clone(),
                indent: indent.clone(),
                current_indent: current_indent.clone() + indent,
            },
            Self::Compact => Self::Compact,
        }
    }
    pub fn is_pretty(&self) -> bool {
        matches!(self, Self::Pretty { .. })
    }
    pub fn is_compact(&self) -> bool {
        matches!(self, Self::Compact)
    }

    pub fn new_line(&self) -> &str {
        match self {
            Self::Pretty { new_line, .. } => new_line,
            Self::Compact => " ",
        }
    }
    pub fn current_indent(&self) -> &str {
        match self {
            Self::Pretty { current_indent, .. } => current_indent,
            Self::Compact => "",
        }
    }
    pub fn is_debug(&self) -> bool {
        false
    }
}
pub trait ToCode {
    fn to_code(&self, fmt_ctx: &FormatContext) -> String;
}
pub fn ctx_for_err() -> FormatContext {
    FormatContext::new_pretty("\n", "  ")
}
