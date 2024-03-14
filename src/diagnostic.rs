use crate::span::Span;
use std::io;
use std::io::IsTerminal;

use async_std::io::WriteExt;
use colored::{Color, ColoredString, Colorize};

use std::sync::Arc;

#[derive(Debug, Clone)]
pub struct Diagnostic {
    message: String,
    span: Option<Arc<Span>>,
    level: Level,
}

impl Diagnostic {
    pub fn error<S: Into<String>>(message: S) -> Self {
        Diagnostic {
            message: message.into(),
            span: None,
            level: Level::Error,
        }
    }

    pub fn spanned_error<M: Into<String>, S: Into<Arc<Span>>>(span: S, message: M) -> Self {
        Diagnostic {
            message: message.into(),
            span: Some(span.into()),
            level: Level::Error,
        }
    }

    pub fn debug<S: Into<String>>(message: S) -> Self {
        Diagnostic {
            message: message.into(),
            span: None,
            level: Level::Debug,
        }
    }

    pub fn spanned_debug<M: Into<String>, S: Into<Arc<Span>>>(span: S, message: M) -> Self {
        Diagnostic {
            message: message.into(),
            span: Some(span.into()),
            level: Level::Debug,
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
        let message = self.format_message();
        writeln!(async_std::io::stdout(), "{message}")
            .await
            .unwrap();
        if let Some(span) = self.span {
            let pointer = span.pointer(self.level.color());
            writeln!(async_std::io::stdout(), "{pointer}")
                .await
                .unwrap();
        }
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
            message: String::new(),
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

#[macro_export]
macro_rules! error {
    ($($arg:tt)*) => ($crate::diagnostic::Diagnostic::error(::std::format!($($arg)*)))
}

#[macro_export]
macro_rules! spanned_error {
    ($span:expr, $($arg:tt)*) => ($crate::diagnostic::Diagnostic::spanned_error($span, ::std::format!($($arg)*)))
}

#[macro_export]
macro_rules! debug {
    ($($arg:tt)*) => ($crate::diagnostic::Diagnostic::debug(::std::format!($($arg)*)))
}

#[macro_export]
macro_rules! spanned_debug {
    ($span:expr, $($arg:tt)*) => ($crate::diagnostic::Diagnostic::spanned_debug($span, ::std::format!($($arg)*)))
}
