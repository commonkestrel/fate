use std::{
    collections::HashMap,
    ops::{Deref, Range, RangeInclusive},
    sync::Arc,
};

use async_std::path::PathBuf;
use indexmap::IndexMap;

use crate::{
    build::lexer::{Delimeter, TokenInner}, diagnostic::Diagnostic, error, span::{Span, Spanned}, spanned_error
};

use super::{
    lexer::{self, Punctuation, TokenStream},
    token::{
        Break, CloseBrace, CloseBracket, CloseParen, Comma, Continue, DoubleColon, Eq, For, Gt, Ident, Let, Lt, Mut, OpenBrace, OpenBracket, OpenParen, Return, Semicolon
    },
};

pub fn parse<L>(stream: TokenStream) {}

pub struct Cursor<'a> {
    stream: &'a mut [Spanned<TokenInner>],
    position: usize,
}

impl<'a> Cursor<'a> {
    #[inline]
    fn new(stream: &'a mut [Spanned<TokenInner>]) -> Self {
        Self {
            stream,
            position: 0,
        }
    }

    #[inline]
    fn parse<T: Parsable>(&mut self) -> Result<T, Diagnostic> {
        T::parse(self)
    }

    #[inline]
    fn at_end(&self) -> bool {
        self.position >= self.stream.len()
    }

    #[inline]
    fn peek(&self) -> Option<&Spanned<TokenInner>> {
        self.stream.get(self.position)
    }

    #[inline]
    fn peek2(&self) -> Option<&Spanned<TokenInner>> {
        self.stream.get(self.position + 1)
    }

    #[inline]
    fn peek_offset(&self, offset: usize) -> Option<&Spanned<TokenInner>> {
        self.stream.get(self.position + offset)
    }

    #[inline]
    fn step(&mut self) {
        self.position += 1;
    }
}

impl<'a> Iterator for Cursor<'a> {
    type Item = Spanned<TokenInner>;

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
        pub struct $ident<T> {
            open: Spanned<$open>,
            inner: T,
            close: Spanned<$close>,
        }

        impl<T> $ident<T> {
            pub fn span(&self) -> Span {
                self.open.span().to(&self.close.span())
            }
        }

        impl<T: Parsable> Parsable for $ident<T> {
            fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
                let open = cursor.parse()?;

                let mut depth = 0;
                let start = cursor.position;

                for (i, tok) in (&cursor.stream[start..]).into_iter().enumerate() {
                    cursor.position = start + i;
                    match tok.inner() {
                        $open_inner => depth += 1,
                        $close_inner => {
                            if depth == 0 {
                                return Ok(Self {
                                    open,
                                    inner: T::parse(&mut Cursor::new(
                                        &mut cursor.stream[start..i],
                                    ))?,
                                    close: cursor.parse()?,
                                });
                            }

                            depth -= 1;
                        }
                        _ => {}
                    }
                }

                Err(spanned_error!(open.into_span(), concat!("unmatched opening ", $name)))
            }

            fn description(&self) -> &'static str {
                concat!($name, " expression")
            }
        }
    };
}

delimeterized!(
    "parenthesized",
    Parenthesized, OpenParen, CloseParen,
    TokenInner::Delimeter(Delimeter::OpenParen),
    TokenInner::Delimeter(Delimeter::CloseParen),
    "parenthesis"
);
delimeterized!(
    "bracketed",
    Bracketed, OpenBracket, CloseBracket,
    TokenInner::Delimeter(Delimeter::OpenBracket),
    TokenInner::Delimeter(Delimeter::CloseBracket),
    "bracket"
);
delimeterized!(
    "braced",
    Braced, OpenBrace, CloseBrace,
    TokenInner::Delimeter(Delimeter::OpenBrace),
    TokenInner::Delimeter(Delimeter::CloseBrace),
    "brace"
);
delimeterized!(
    "arrowed",
    Arrowed, Lt, Gt,
    TokenInner::Punctuation(Punctuation::Lt),
    TokenInner::Punctuation(Punctuation::Gt),
    "arrow"
);

pub struct Punctuated<T, S> {
    inner: Vec<(T, S)>,
    last: Option<T>,
}

impl<T, S> Punctuated<T, S> {
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

        Ok(Self { inner, last })
    }

    fn description(&self) -> &'static str {
        "punctuated expression"
    }
}

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
                        return Err(spanned_error!(
                            tok.span().clone(),
                            "unexpected duplicate seperator"
                        ))
                    }
                },
                _ => $cursor.step(),
            }
        }

        Ok(Punctuated { inner, last })
    }};
}

struct Namespace {
    home: PathBuf,
    // constants: HashMap<String, Constant>,
}

struct FnDefinition {
    name: Ident,
    parameters: Punctuated<Variable, Comma>,
    body: FnTree,
}

enum FnTree {
    Body {
        open: OpenBrace,
        content: Vec<FnTree>,
        close: CloseBrace,
    },
    If {
        condition: Expr,
        content: Box<FnTree>,
        else_cond: Option<Box<FnTree>>,
    },
    For {
        word: For,

        init: Expr,
        init_end: Semicolon,
        check: Expr,
        check_end: Semicolon,
        post: Expr,
        post_end: Semicolon,

        contents: Box<FnTree>,
    },
    Break(Break),
    Continue(Continue),
    Instruction(Instruction),
    Initialization {
        let_tok: Let,
        mutability: Option<Mut>,
        name: Ident,
        eq: Eq,
        expr: Expr,
    },
    Assignment {
        name: Ident,
        eq: Eq,
        expr: Expr,
    },
    Return {
        word: Return,
        contents: Expr,
    },
}

enum Instruction {}

pub struct FnCall {
    path: Punctuated<Spanned<Ident>, Spanned<DoubleColon>>,
    parameters: Parenthesized<Punctuated<Spanned<Expr>, Spanned<Comma>>>,
}

impl Parsable for FnCall {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        let path = punctuated!(
            cursor,
            TokenInner::Ident(_),
            TokenInner::Punctuation(Punctuation::DoubleColon)
        )?;
        let parameters = cursor.parse()?;

        Ok(Self { path, parameters })
    }

    fn description(&self) -> &'static str {
        "function call"
    }
}

struct Expr(TokenStream);

impl Parsable for Spanned<Expr> {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        todo!();
    }

    fn description(&self) -> &'static str {
        "expression"
    }
}

trait Size {
    fn size(&self) -> u16;
}

struct Field {
    ty: Type,
    mutability: Option<Mut>,
}

struct Variable {
    identifier: Ident,
    ty: Type,
    mutability: Option<Mut>,
}

enum Type {
    Primitive(lexer::Primitive),
    Pointer {
        mutability: Option<Mut>,
        ty: Box<Type>,
    },
    Array {
        ty: Box<Type>,
        len: u16,
    },
    Composite(Ident),
}

struct Struct {
    fields: IndexMap<String, Field>,
}

struct Enum {
    variants: IndexMap<String, Variant>,
}

enum Variant {
    Void,
    Struct(Struct),
    Tuple(Vec<Type>),
}
