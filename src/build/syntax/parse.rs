use std::{
    collections::HashMap, ops::{Deref, Range, RangeBounds, RangeInclusive}, path::{Path, PathBuf}, slice::SliceIndex, sync::Arc
};

use super::{
    ast::{Enum, Expr, FnDefinition, Static, Struct, Union, Use, Visibility},
    lex::{self, Delimeter, Keyword, Punctuation, Token, TokenStream},
    token::{
        Break, CloseBrace, CloseBracket, CloseParen, Comma, Continue, DoubleColon, Eq, For, Gt,
        Ident, Let, Lt, Mut, OpenBrace, OpenBracket, OpenParen, Return, Semicolon,
    },
};
use crate::{
    diagnostic::Diagnostic, error, span::{Lookup, Span, Spanned}, spanned_debug, spanned_error, Token
};

pub fn parse(stream: TokenStream, home: PathBuf, source_name: Arc<String>, lookup: Arc<Lookup>) -> Result<Namespace, Vec<Diagnostic>> {
    let mut cursor = Cursor::new(&stream, source_name, lookup);
    let mut errors = Vec::new();
    let mut namespace = Namespace::empty(home);
    let mut visibility = Visibility::Private;

    while let Some(tok) = cursor.peek() {
        match tok.inner() {
            Token::Keyword(Keyword::Fn) => namespace.functions.push(match cursor.parse() {
                Ok(func) => {
                    let ret = (func, visibility);
                    visibility = Visibility::Private;
                    ret
                },
                Err(err) => {
                    errors.push(err);
                    continue;
                }
            }),
            Token::Keyword(Keyword::Static) => namespace.statics.push(match cursor.parse() {
                Ok(stat) => {
                    let ret = (stat, visibility);
                    visibility = Visibility::Private;
                    ret
                },
                Err(err) => {
                    errors.push(err);
                    continue;
                }
            }),
            Token::Keyword(Keyword::Struct) => namespace.structs.push(match cursor.parse() {
                Ok(struc) => {
                    let ret = (struc, visibility);
                    visibility = Visibility::Private;
                    ret
                },
                Err(err) => {
                    errors.push(err);
                    continue;
                }
            }),
            Token::Keyword(Keyword::Union) => namespace.unions.push(match cursor.parse() {
                Ok(un) => {
                    let ret = (un, visibility);
                    visibility = Visibility::Private;
                    ret
                },
                Err(err) => {
                    errors.push(err);
                    continue;
                }
            }),
            Token::Keyword(Keyword::Enum) => namespace.enums.push(match cursor.parse() {
                Ok(en) => {
                    let ret = (en, visibility);
                    visibility = Visibility::Private;
                    ret
                },
                Err(err) => {
                    errors.push(err);
                    continue;
                }
            }),
            Token::Keyword(Keyword::Use) => namespace.imports.push(match cursor.parse() {
                Ok(import) => {
                    let ret = (import, visibility);
                    visibility = Visibility::Private;
                    ret
                },
                Err(err) => {
                    errors.push(err);
                    continue;
                }
            }),
            Token::Keyword(Keyword::Namespace) => namespace.submodules.push(match Submodule::parse(&mut cursor) {
                Ok(submodule) => {
                    let ret = (submodule, visibility);
                    visibility = Visibility::Private;
                    ret
                },
                Err(err) => {
                    errors.push(err);
                    continue;
                }
            }),
            Token::Keyword(Keyword::Pub) => {
                cursor.step();
                visibility = Visibility::Public;
            }
            _ => {
                errors.push(spanned_error!(tok.span().clone(), "unexpected {} in top level section", tok.description()));
                cursor.step();
            },
        }
    }

    namespace.bubble_errors(&mut errors);
    if errors.is_empty() {
        Ok(namespace)
    } else {
        Err(errors)
    }
}

pub struct Cursor<'a> {
    stream: &'a [Spanned<Token>],
    pub position: usize,
    eof_span: Span,
}

impl<'a> Cursor<'a> {
    #[inline]
    pub fn new(
        stream: &'a [Spanned<Token>],
        source_name: Arc<String>,
        lookup: Arc<Lookup>,
    ) -> Self {
        let end_position = stream
            .last()
            .map(|last| last.span().end()..(last.span().end() + 1))
            .unwrap_or(0..1);
        Self {
            stream,
            position: 0,
            eof_span: Span::new(source_name, lookup, end_position),
        }
    }

    #[inline]
    pub fn parse<T: Parsable>(&mut self) -> Result<T, Diagnostic> {
        T::parse(self)
    }

    #[inline]
    pub fn stream(&self) -> &[Spanned<Token>] {
        self.stream
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

    pub fn check2(&self, other: &Token) -> bool {
        self.stream
            .get(self.position+1)
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

    #[track_caller]
    pub fn slice<R: Into<Range<usize>>>(&mut self, range: R) -> Cursor {
        Cursor::new(
            &self.stream[range.into()],
            self.eof_span.source_name(),
            self.eof_span.lookup(),
        )
    }

    #[inline]
    pub fn eof_span(&self) -> Span {
        self.eof_span.clone()
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

pub trait Bubble {
    fn bubble_errors(&self, output: &mut Vec<Diagnostic>);
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

        impl<T> $ident<T> {
            pub fn inner(&self) -> &T {
                &self.inner
            }
        }

        impl<T> Deref for $ident<T> {
            type Target = T;

            fn deref(&self) -> &T {
                self.inner()
            }
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
                                        inner: T::parse(&mut cursor.slice(start..i))?,
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
                                return Ok($ident {
                                    open: open.into_inner(),
                                    inner: T::parse(&mut cursor.slice(start..(start + i)))?,
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

    pub fn empty() -> Self {
        Punctuated {
            inner: Vec::new(),
            last: Box::new(None),
        }
    }

    pub fn len(&self) -> usize {
        self.inner.len() + if self.last.is_some() { 1 } else { 0 }
    }

    pub fn last(&self) -> Option<&T> {
        (*self.last)
            .as_ref()
            .or_else(|| self.inner.last().map(|both| &both.0))
    }

    pub fn values<'a>(&'a self) -> Box<dyn Iterator<Item = &T> + 'a> {
        Box::new(
            self.inner
                .iter()
                .map(|pair| &pair.0)
                .chain(self.last.iter()),
        )
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

#[derive(Debug, Clone, PartialEq)]
pub struct Namespace {
    home: PathBuf,
    imports: Vec<(Spanned<Use>, Visibility)>,
    submodules: Vec<(Submodule, Visibility)>,
    statics: Vec<(Spanned<Static>, Visibility)>,
    structs: Vec<(Spanned<Struct>, Visibility)>,
    unions: Vec<(Spanned<Union>, Visibility)>,
    enums: Vec<(Spanned<Enum>, Visibility)>,
    functions: Vec<(Spanned<FnDefinition>, Visibility)>,
}

impl Namespace {
    fn empty(home: PathBuf) -> Self {
        Namespace {
            home,
            imports: Vec::new(),
            submodules: Vec::new(),
            statics: Vec::new(),
            structs: Vec::new(),
            unions: Vec::new(),
            enums: Vec::new(),
            functions: Vec::new(),
        }
    }

    fn bubble_errors(&self, output: &mut Vec<Diagnostic>) {
        self.submodules.iter().for_each(|sm| { sm.0.content.as_ref().map(|nm| nm.bubble_errors(output)); });
        self.statics.iter().for_each(|st| st.0.value.bubble_errors(output));
        self.structs.iter().for_each(|st| st.0.fields.values().for_each(|def| def.ty.bubble_errors(output)));
        self.unions.iter().for_each(|un| un.0.fields.values().for_each(|def| def.ty.bubble_errors(output)));
        self.enums.iter().for_each(|en| en.0.variants.values().for_each(|var| var.ty.bubble_errors(output)));
        self.functions.iter().for_each(|fu| fu.0.bubble_errors(output));
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Submodule {
    ident: Spanned<Ident>,
    content: Option<Spanned<Braced<Namespace>>>,
}

impl Submodule {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        let _: Token![namespace] = cursor.parse()?;
        let ident: Spanned<Ident> = cursor.parse()?;
        
        let content = if cursor.check(&Token::Delimeter(Delimeter::OpenBrace)) {
            let open: Spanned<OpenBrace> = cursor.parse()?;
            let mut depth = 0;

            todo!();
        } else {
            let _: Token![;] = cursor.parse()?;
            None
        };

        Ok(Submodule{ident, content})
    }
}
