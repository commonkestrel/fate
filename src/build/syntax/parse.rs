use std::{
    collections::HashMap,
    ops::{Deref, Range, RangeInclusive},
    sync::Arc,
};

use async_std::path::PathBuf;
use indexmap::IndexMap;

use super::{
    ast::Expr,
    lex::{self, Delimeter, Punctuation, Token, TokenStream},
    token::{
        Break, CloseBrace, CloseBracket, CloseParen, Comma, Continue, DoubleColon, Eq, For, Gt,
        Ident, Let, Lt, Mut, OpenBrace, OpenBracket, OpenParen, Return, Semicolon,
    },
};
use crate::{
    diagnostic::Diagnostic,
    error,
    span::{Span, Spanned},
    spanned_error,
};

pub fn parse<L>(stream: TokenStream) {}

pub struct Cursor<'a> {
    stream: &'a mut [Spanned<Token>],
    position: usize,
}

impl<'a> Cursor<'a> {
    #[inline]
    pub fn new(stream: &'a mut [Spanned<Token>]) -> Self {
        Self {
            stream,
            position: 0,
        }
    }

    #[inline]
    pub fn parse<T: Parsable>(&mut self) -> Result<T, Diagnostic> {
        T::parse(self)
    }

    #[inline]
    pub fn at_end(&self) -> bool {
        self.position >= self.stream.len()
    }

    #[inline]
    pub fn peek(&self) -> Option<&Spanned<Token>> {
        self.stream.get(self.position)
    }

    #[inline]
    pub fn peek2(&self) -> Option<&Spanned<Token>> {
        self.stream.get(self.position + 1)
    }

    pub fn check(&self, other: &Token) -> bool {
        self.stream
            .get(self.position)
            .map(|next| next.inner() == other)
            .unwrap_or(false)
    }

    #[inline]
    pub fn peek_offset(&self, offset: usize) -> Option<&Spanned<Token>> {
        self.stream.get(self.position + offset)
    }

    #[inline]
    pub fn step(&mut self) {
        self.position += 1;
    }

    #[inline]
    pub fn step_back(&mut self) {
        if self.position != 0 {
            self.position -= 1;
        }
    }
}

impl<'a> Iterator for Cursor<'a> {
    type Item = Spanned<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        let ret = self.stream.get(self.position);
        self.position += 1;
        ret.cloned()
    }
}

pub trait Parsable: Sized {
    /// Parses the item from a stream of raw tokens.
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic>;
    /// Static description of the item.
    /// Used for error messages.
    fn description(&self) -> &'static str;
}

impl Parsable for TokenStream {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        Ok(cursor.collect())
    }

    fn description(&self) -> &'static str {
        "tokens"
    }
}

macro_rules! delimeterized {
    ($name:literal, $ident:ident, $open:ident, $close:ident, $open_inner:pat, $close_inner:pat, $error:literal) => {
        #[derive(Debug, Clone, PartialEq)]
        pub struct $ident<T> {
            open: $open,
            inner: T,
            close: $close,
        }

        impl<T: Parsable> Parsable for Spanned<$ident<T>> {
            fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
                let open: Spanned<$open> = cursor.parse()?;

                let mut depth = 0;
                let start = cursor.position;

                for (i, tok) in (&cursor.stream[start..]).into_iter().enumerate() {
                    cursor.position = start + i;
                    match tok.inner() {
                        $open_inner => depth += 1,
                        $close_inner => {
                            if depth == 0 {
                                let close: Spanned<$close> = cursor.parse()?;
                                let span = open.span().to(close.span());
                                return Ok(Spanned::new(
                                    $ident {
                                        open: open.into_inner(),
                                        inner: T::parse(&mut Cursor::new(
                                            &mut cursor.stream[start..i],
                                        ))?,
                                        close: close.into_inner(),
                                    },
                                    span,
                                ));
                            }

                            depth -= 1;
                        }
                        _ => {}
                    }
                }

                Err(spanned_error!(
                    open.into_span(),
                    concat!("unmatched opening ", $name)
                ))
            }

            fn description(&self) -> &'static str {
                concat!($name, " expression")
            }
        }

        impl<T: Parsable> Parsable for $ident<T> {
            fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
                let open: Spanned<$open> = cursor.parse()?;

                let mut depth = 0;
                let start = cursor.position;

                for (i, tok) in (&cursor.stream[start..]).into_iter().enumerate() {
                    cursor.position = start + i;
                    match tok.inner() {
                        $open_inner => depth += 1,
                        $close_inner => {
                            if depth == 0 {
                                let close: Spanned<$close> = cursor.parse()?;
                                let span = open.span().to(close.span());
                                return Ok($ident {
                                    open: open.into_inner(),
                                    inner: T::parse(&mut Cursor::new(
                                        &mut cursor.stream[start..i],
                                    ))?,
                                    close: close.into_inner(),
                                });
                            }

                            depth -= 1;
                        }
                        _ => {}
                    }
                }

                Err(spanned_error!(
                    open.into_span(),
                    concat!("unmatched opening ", $name)
                ))
            }

            fn description(&self) -> &'static str {
                concat!($name, " expression")
            }
        }
    };
}

delimeterized!(
    "parenthesized",
    Parenthesized,
    OpenParen,
    CloseParen,
    Token::Delimeter(Delimeter::OpenParen),
    Token::Delimeter(Delimeter::CloseParen),
    "parenthesis"
);
delimeterized!(
    "bracketed",
    Bracketed,
    OpenBracket,
    CloseBracket,
    Token::Delimeter(Delimeter::OpenBracket),
    Token::Delimeter(Delimeter::CloseBracket),
    "bracket"
);
delimeterized!(
    "braced",
    Braced,
    OpenBrace,
    CloseBrace,
    Token::Delimeter(Delimeter::OpenBrace),
    Token::Delimeter(Delimeter::CloseBrace),
    "brace"
);
delimeterized!(
    "arrowed",
    Arrowed,
    Lt,
    Gt,
    Token::Punctuation(Punctuation::Lt),
    Token::Punctuation(Punctuation::Gt),
    "arrow"
);

#[derive(Debug, Clone, PartialEq)]
pub struct Punctuated<T, S> {
    inner: Vec<(T, S)>,
    last: Box<Option<T>>,
}

impl<T, S> Punctuated<T, S> {
    pub fn new(inner: Vec<(T, S)>, last: Option<T>) -> Self {
        Punctuated {
            inner,
            last: Box::new(last),
        }
    }

    pub fn len(&self) -> usize {
        self.inner.len() + if self.last.is_some() { 1 } else { 0 }
    }

    pub fn last(&self) -> Option<&T>
    where
        T: Deref<Target = T>,
    {
        self.last
            .as_deref()
            .or_else(|| self.inner.last().map(|both| &both.0))
    }
}

impl<T: Parsable, S: Parsable> Parsable for Punctuated<T, S> {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        let mut inner = Vec::new();
        let mut last = None;

        while !cursor.at_end() {
            let next = cursor.parse()?;
            if cursor.at_end() {
                last = Some(next);
                break;
            }
            let sep = cursor.parse()?;

            inner.push((next, sep));
        }

        Ok(Self {
            inner,
            last: Box::new(last),
        })
    }

    fn description(&self) -> &'static str {
        "punctuated expression"
    }
}

#[macro_export]
macro_rules! punctuated {
    ($cursor:expr, $content:pat, $seperator:pat) => {{
        let mut inner = Vec::new();
        let mut last = None;

        while let Some(tok) = $cursor.peek() {
            match tok.inner() {
                $content => last = Some($cursor.parse()?),
                $seperator => match last.take() {
                    Some(l) => inner.push((l, $cursor.parse()?)),
                    None => {
                        return Err($crate::spanned_error!(
                            tok.span().clone(),
                            "unexpected duplicate seperator"
                        ))
                    }
                },
                _ => break,
            }
        }

        Ok(Punctuated::new(inner, last))
    }};
    ($cursor:expr, !$end:pat, $seperator:pat) => {{
        let mut inner = Vec::new();
        let mut last = None;

        while let Some(tok) = $cursor.peek() {
            match tok.inner() {
                $end => break,
                $seperator => match last.take() {
                    Some(l) => inner.push((l, $cursor.parse()?)),
                    None => {
                        return Err($crate::spanned_error!(
                            tok.span().clone(),
                            "unexpected duplicate seperator"
                        ))
                    }
                },
                _ => last = Some($cursor.parse()?),
            }
        }

        Ok(Punctuated::new(inner, last))
    }};
}

struct Namespace {
    home: PathBuf,
    // constants: HashMap<String, Constant>,
}

enum Instruction {}
