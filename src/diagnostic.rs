use crate::build::Span;
use std::sync::Arc;

#[derive(Debug, Clone)]
pub struct Diagnostic {
    message: String,
    span: Option<Arc<Span>>,
    level: Level,
}

impl Diagnostic {
    pub fn error<S: Into<String>>(message: S) -> Diagnostic {
        Diagnostic {
            message: message.into(),
            span: None,
            level: Level::Error,
        }
    }

    pub fn set_span(&mut self, span: Option<Arc<Span>>) {
        self.span = span;
    }
}

impl PartialEq for Diagnostic {
    fn eq(&self, other: &Self) -> bool {
        self.message == other.message && self.level == other.level
    }
}

// Implemented for logos lexing errors
// Not for program use
impl Default for Diagnostic {
    fn default() -> Self {
        Diagnostic {
            message: "unrecognized token".into(),
            span: None,
            level: Level::Error,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Level {
    Error,
    Warn,
    Help,
    Note,
    Debug,
}
