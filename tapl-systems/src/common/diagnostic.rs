use core::ops;
use std::fmt::Display;

pub fn merge_span(a: Option<Span>, b: Option<Span>) -> Option<Span> {
    match (a, b) {
        (Some(a), Some(b)) => Some(a.merge(b)),
        (Some(a), None) => Some(a),
        (None, Some(b)) => Some(b),
        (None, None) => None,
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Location {
    line: usize,
    col: usize,
}
impl Location {
    pub fn new(line: usize, col: usize) -> Self {
        Self { line, col }
    }
    pub fn line(&self) -> usize {
        self.line
    }
    pub fn col(&self) -> usize {
        self.col
    }
}
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    start: Location,
    end: Location,
}
impl Span {
    pub fn new(start: Location, end: Location) -> Self {
        Self { start, end }
    }
    pub fn start(&self) -> &Location {
        &self.start
    }
    pub fn end(&self) -> &Location {
        &self.end
    }

    // span(min(a.start, b.start), max(a.end, b.end))
    pub fn merge(&self, other: Span) -> Span {
        let start = Location::new(
            std::cmp::min(self.start.line(), other.start.line()),
            std::cmp::min(self.start.col(), other.start.col()),
        );
        let end = Location::new(
            std::cmp::max(self.end.line(), other.end.line()),
            std::cmp::max(self.end.col(), other.end.col()),
        );
        Span::new(start, end)
    }
}
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Spanned<T> {
    pub span: Option<Span>,
    pub value: T,
}
impl<T> Spanned<T> {
    pub fn new(span: Option<Span>, value: T) -> Self {
        Self { span, value }
    }
    pub fn span(&self) -> Option<&Span> {
        self.span.as_ref()
    }
    pub fn value(&self) -> &T {
        &self.value
    }

    pub fn as_ref(&self) -> Spanned<&T> {
        Spanned {
            span: self.span,
            value: &self.value,
        }
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Spanned<U> {
        Spanned {
            span: self.span,
            value: f(self.value),
        }
    }
}
impl<T: Copy> Copy for Spanned<T> {}
// cloned
impl<T: Clone> Spanned<&T> {
    pub fn cloned(&self) -> Spanned<T> {
        Spanned {
            span: self.span,
            value: self.value.clone(),
        }
    }
}
// impl<T> AsRef<T> for Spanned<T> {
//     fn as_ref(&self) -> &T {
//         &self.value
//     }
// }
// impl<T> AsRef<T> for Spanned<T> {
//     fn as_ref(&self) -> &T {
//         &self.value
//     }
// }
impl<T> ops::Deref for Spanned<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

pub struct FileInfo {
    pub uri: String,
    pub loc: Location,
}

#[derive(Clone, Debug)]
pub enum DiagnosticLevel {
    Error,
    Warning,
    Info,
    Hint,
}

#[derive(Clone, Debug)]
pub struct Diagnostic {
    level: DiagnosticLevel,
    message: String,
    span: Option<Span>,
}
impl Diagnostic {
    pub fn new(level: DiagnosticLevel, message: String, span: Option<Span>) -> Self {
        Self {
            level,
            message,
            span,
        }
    }
    pub fn level(&self) -> &DiagnosticLevel {
        &self.level
    }
    pub fn message(&self) -> &String {
        &self.message
    }
    pub fn span(&self) -> Option<&Span> {
        self.span.as_ref()
    }
    pub fn map_message<F: FnOnce(String) -> String>(self, f: F) -> Self {
        Self {
            level: self.level,
            message: f(self.message),
            span: self.span,
        }
    }
}
impl Display for Diagnostic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let level = match self.level {
            DiagnosticLevel::Error => "error",
            DiagnosticLevel::Warning => "warning",
            DiagnosticLevel::Info => "info",
            DiagnosticLevel::Hint => "hint",
        };
        let span = match self.span {
            Some(span) => format!(
                " at {}:{}-{}:{}",
                span.start().line(),
                span.start().col(),
                span.end().line(),
                span.end().col()
            ),
            None => " [no loc]".to_string(),
        };
        write!(f, "{}{}: {}", level, span, self.message)
    }
}

#[derive(Clone, Debug)]
pub struct SpannedWithDiagnostics<T> {
    pub value: T,
    pub span: Option<Span>,
    pub diagnostics: Vec<Diagnostic>,
}

pub fn merge_diags<T>(v: &mut Vec<T>, mut w: Vec<T>) {
    // Swap the two vectors to make computation time to be O(n log n).
    if v.len() < w.len() {
        std::mem::swap(v, &mut w);
    }
    for x in w {
        v.push(x);
    }
}
