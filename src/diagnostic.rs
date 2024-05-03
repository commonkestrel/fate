use crate::span::Span;
use std::io;
use std::io::IsTerminal;

use async_std::{io::WriteExt, sync::RwLock};
use colored::{Color, ColoredString, Colorize};

use std::sync::Arc;

const BUG_MESSAGE: &str = "This is a bug. Please report it at `https://github.com/commonkestrel/fate/issues`";

#[derive(Debug, Clone)]
pub struct Diagnostic {
    level: Level,
    message: String,
    note: Option<String>,
    span: Option<Arc<Span>>,
    
}

impl Diagnostic {
    pub fn error<S: Into<String>>(message: S) -> Self {
        Diagnostic {
            level: Level::Error,
            message: message.into(),
            note: None,
            span: None,
        }
    }

    pub fn spanned_error<M: Into<String>, S: Into<Arc<Span>>>(span: S, message: M) -> Self {
        Diagnostic {
            level: Level::Error,
            message: message.into(),
            note: None,
            span: Some(span.into()),
        }
    }

    pub fn warn<S: Into<String>>(message: S) -> Self {
        Diagnostic {
            level: Level::Warn,
            message: message.into(),
            note: None,
            span: None,
        }
    }

    pub fn spanned_warn<M: Into<String>, S: Into<Arc<Span>>>(span: S, message: M) -> Self {
        Diagnostic {
            level: Level::Warn,
            message: message.into(),
            note: None,
            span: Some(span.into()),
        }
    }

    pub fn debug<S: Into<String>>(message: S) -> Self {
        Diagnostic {
            level: Level::Debug,
            message: message.into(),
            note: None,
            span: None,
        }
    }

    pub fn spanned_debug<M: Into<String>, S: Into<Arc<Span>>>(span: S, message: M) -> Self {
        Diagnostic {
            level: Level::Debug,
            message: message.into(),
            note: None,
            span: Some(span.into()),
        }
    }

    pub fn set_span<S: Into<Arc<Span>>>(&mut self, span: Option<S>) {
        self.span = span.map(|s| s.into());
    }

    #[inline]
    pub fn with_span<S: Into<Arc<Span>>>(mut self, span: Option<S>) -> Self {
        self.set_span(span);
        self
    }

    pub fn set_message<S: Into<String>>(&mut self, message: S) {
        self.message = message.into();
    }

    #[inline]
    pub fn with_message<S: Into<String>>(mut self, message: S) -> Self {
        self.set_message(message);
        self
    }

    pub fn set_note<S: Into<String>>(&mut self, note: S) {
        self.note = Some(note.into());
    }

    #[inline]
    pub fn with_note<S: Into<String>>(mut self, note: S) -> Self {
        self.set_note(note);
        self
    }

    pub fn clear_note(&mut self) {
        self.note = None;
    }

    pub fn without_note(mut self) -> Self {
        self.clear_note();
        self
    }

    #[inline]
    pub fn as_bug(self) -> Self {
        self.with_note(BUG_MESSAGE)
    }

    fn format_message(&self) -> ColoredString {
        let title = self.level.title();
        let color = self.level.color();

        format!("{}: {}", title.color(color), self.message).bold()
    }

    pub async fn emit(self) {
        if io::stdout().is_terminal() {
            self.emit_fancy().await;
        } else {
            self.raw_emit().await;
        }
    }

    pub fn sync_emit(self) {
        if io::stdout().is_terminal() {
            async_std::task::block_on(self.emit_fancy());
        } else {
            async_std::task::block_on(self.raw_emit());
        }
    }

    async fn raw_emit(self) {
        let title = match self.level {
            Level::Error => "error",
            Level::Warn => "warn",
            Level::Help => "help",
            Level::Info => "info",
            Level::Debug => "debug",
        };

        writeln!(async_std::io::stdout(), "{title}: {}", self.message)
            .await
            .unwrap();
    }

    async fn emit_fancy(self) {
        let mut note_offset = self.level.title().len() + 1;
        let message = self.format_message();
        writeln!(async_std::io::stdout(), "{message}")
            .await
            .unwrap();

        if let Some(span) = self.span {
            let (pointer, offset) = span.pointer(self.level.color());
            note_offset = offset + 1;
            writeln!(async_std::io::stdout(), "{pointer}")
                .await
                .unwrap();
        }

        if let Some(note) = self.note {
            writeln!(async_std::io::stdout(), "{:>note_offset$} {}: {note}", "=".bright_blue().bold(), "note".bold())
                .await
                .unwrap()
        }
    }
}

impl PartialEq for Diagnostic {
    fn eq(&self, other: &Self) -> bool {
        self.message == other.message && self.level == other.level
    }
}

// Implemented for `logos` lexing errors
// Not for program use
impl Default for Diagnostic {
    fn default() -> Self {
        Diagnostic {
            level: Level::Error,
            message: String::new(),
            note: None,
            span: None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Level {
    Error,
    Warn,
    Help,
    Info,
    Debug,
}

impl Level {
    pub fn title(&self) -> &'static str {
        match self {
            Level::Error => "error",
            Level::Warn => "warn",
            Level::Help => "help",
            Level::Info => "info",
            Level::Debug => "debug",
        }
    }

    pub fn color(&self) -> Color {
        match self {
            Level::Error => Color::Red,
            Level::Warn => Color::Yellow,
            Level::Help => Color::Cyan,
            Level::Info => Color::White,
            Level::Debug => Color::BrightMagenta,
        }
    }
}

#[derive(Clone)]
pub struct Reporter {
    diagnostics: Arc<RwLock<Vec<Diagnostic>>>,
}

impl Reporter {
    pub fn new() -> Self {
        Reporter {
            diagnostics: Arc::new(RwLock::new(Vec::new())),
        }
    }

    pub async fn report(&self, diag: Diagnostic) {
        let mut diagnostics = self.diagnostics.write().await;
        diagnostics.push(diag);
    }

    pub fn report_sync(&self, diag: Diagnostic) {
        let mut diagnostics = async_std::task::block_on(self.diagnostics.write());
        diagnostics.push(diag);
    }

    pub async fn emit_all(&self) {
        let mut diagnostics = self.diagnostics.write().await;
        for diagnostic in diagnostics.drain(..) {
            diagnostic.emit().await;
        }
    }

    pub fn has_errors(&self) -> bool {
        let diagnostics = async_std::task::block_on(self.diagnostics.read());
        return diagnostics.iter().any(|diag| diag.level == Level::Error);
    }
}

impl Default for Reporter {
    fn default() -> Self {
        Self::new()
    }
}

#[macro_export]
macro_rules! error {
    ($($arg:tt)*) => ($crate::diagnostic::Diagnostic::error(::std::format!($($arg)*)))
}

#[macro_export]
macro_rules! spanned_error {
    ($span:expr, $($arg:tt)*) => ($crate::diagnostic::Diagnostic::spanned_error($span, ::std::format!($($arg)*)))
}

#[macro_export]
macro_rules! warn {
    ($($arg:tt)*) => ($crate::diagnostic::Diagnostic::warn(::std::format!($($arg)*)))
}

#[macro_export]
macro_rules! spanned_warn {
    ($span:expr, $($arg:tt)*) => ($crate::diagnostic::Diagnostic::spanned_warn($span, ::std::format!($($arg)*)))
}

#[macro_export]
macro_rules! debug {
    ($($arg:tt)*) => ($crate::diagnostic::Diagnostic::debug(::std::format!($($arg)*)))
}

#[macro_export]
macro_rules! spanned_debug {
    ($span:expr, $($arg:tt)*) => ($crate::diagnostic::Diagnostic::spanned_debug($span, ::std::format!($($arg)*)))
}
