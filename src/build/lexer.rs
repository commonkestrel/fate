use std::{io::ErrorKind, ops::Range, path::Path, sync::Arc};

use async_std::{fs::File, io::{prelude::*, BufReader}, stream::StreamExt};
use logos::Logos;
use crate::diagnostic::Diagnostic;

use super::ascii::AsciiStr;

pub type TokenStream = Vec<Token>;
pub type Errors = Vec<Diagnostic>;

#[derive(Debug)]
pub struct Token {
    inner: TokenInner,
    span: SingleSpan,
}

#[derive(Logos, Debug, PartialEq)]
#[logos(error = Diagnostic)]
#[logos(skip r"[ \t\f\n]")]
pub enum TokenInner {
    #[token("import", |_| Keyword::Import)]
    #[token("fn", |_| Keyword::Fn)]
    #[token("return", |_| Keyword::Return)]
    #[token("struct", |_| Keyword::Struct)]
    #[token("enum", |_| Keyword::Enum)]
    #[token("pub", |_| Keyword::Pub)]
    #[token("const", |_| Keyword::Const)]
    #[token("static", |_| Keyword::Static)]
    #[token("mut", |_| Keyword::Mut)]
    #[token("if", |_| Keyword::If)]
    #[token("else", |_| Keyword::Else)]
    #[token("match", |_| Keyword::Match)]
    #[token("for", |_| Keyword::For)]
    #[token("while", |_| Keyword::While)]
    #[token("let", |_| Keyword::Let)]
    Keyword(Keyword),
    
    #[token("(", |_| Delimeter::OpeningParenthesis)]
    #[token(")", |_| Delimeter::ClosingParenthesis)]
    #[token("[", |_| Delimeter::OpeningBrace)]
    #[token("]", |_| Delimeter::ClosingBrace)]
    #[token("{", |_| Delimeter::OpeningBracket)]
    #[token("}", |_| Delimeter::ClosingBracket)]
    Delimeter(Delimeter),

    #[token("+", |_| Punctuation::Plus)]
    #[token("-", |_| Punctuation::Minus)]
    #[token("*", |_| Punctuation::Star)]
    #[token("/", |_| Punctuation::Slash)]
    #[token(":", |_| Punctuation::Colon)]
    #[token("::", |_| Punctuation::DoubleColon)]
    #[token(";", |_| Punctuation::SemiColon)]
    #[token(",", |_| Punctuation::Comma)]
    Punctuation(Punctuation),

    #[token("void", |_| Primitive::Void)]
    #[token("bool", |_| Primitive::Bool)]
    #[token("u8", |_| Primitive::U8)]
    #[token("u16", |_| Primitive::U16)]
    #[token("i8", |_| Primitive::I8)]
    #[token("i16", |_| Primitive::I16)]
    Primitive(Primitive),

    #[token("true", |_| true)]
    #[token("false", |_| false)]
    Boolean(bool),
    Immediate(i64),
    String(AsciiStr),
}

#[derive(Debug, PartialEq)]
pub enum Keyword {
    Import,
    Fn,
    Return,
    Struct,
    Enum,
    Pub,
    Const,
    Static,
    Mut,
    If,
    Else,
    Match,
    For,
    While,
    Let,
}

#[derive(Debug, PartialEq)]
pub enum Primitive {
    /// Equivalent to C's `void` type or Rust's `()` type.
    Void,
    Bool,
    U8,
    U16,
    I8,
    I16,
}

#[derive(Debug, PartialEq)]
pub enum Punctuation {
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Star,
    /// `/`
    Slash,
    /// `:`
    Colon,
    /// `::`
    DoubleColon,
    /// `;`
    SemiColon,
    /// `,`
    Comma
}

#[derive(Debug, PartialEq)]
pub enum Delimeter {
    /// `(`
    OpeningParenthesis,
    /// `)`
    ClosingParenthesis,
    /// `[`
    OpeningBracket,
    /// `]`
    ClosingBracket,
    /// `{`
    OpeningBrace,
    /// `}`
    ClosingBrace,
}

#[derive(Debug,  Clone)]
pub enum Source {
    Str(Arc<String>),
    File(Arc<File>),
}

#[derive(Debug)]
pub struct SingleSpan {
    pub source_name: Arc<String>,
    pub source: Source,
    pub line: usize,
    pub columns: Range<usize>,
}

pub async fn lex(content: File, source: &str) -> Result<TokenStream, Errors> {
    let mut tokens = TokenStream::new();
    let mut errs = Errors::new();
    let source = Arc::new(source.to_owned());
    let content = Arc::new(content);

    let buf = BufReader::new(&*content);
    let mut lines = buf.lines().enumerate();
    while let Some((n, line)) = lines.next().await {
        match line {
            Ok(line) => {
                let lex = TokenInner::lexer(&line).spanned();
                for (tok, span) in lex {
                    let s = SingleSpan {
                        source: Source::File(content.clone()),
                        source_name: source.clone(),
                        columns: span,
                        line: n,
                    };

                    match tok {
                        Ok(tok) => {
                            let token = Token {
                                inner: tok,
                                span: s,
                            };
                            tokens.push(token);
                        },
                        Err(mut err) => {
                            err.set_span(Some(Arc::new(s.into())));
                            errs.push(err)
                        },
                    }
                }
            }
            Err(err) => {
                errs.push(Diagnostic::error(match err.kind() {
                    ErrorKind::InvalidData => format!("encountered invalid data on line {n} (likely not valid UTF-8)"),
                    _ => format!("encountered an unexpected error while reading the input file on line {n}: {}", err.kind()),
                }));
                break;
            }
        }
    }

    Ok(tokens)
}

pub fn lex_string(content: &str, source: &str) -> Result<TokenStream, Errors> {
    todo!()
}
