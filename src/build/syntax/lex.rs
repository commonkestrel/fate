use std::sync::Arc;

use crate::{
    diagnostic::Diagnostic,
    error,
    span::{Lookup, Span, Spanned}, spanned_error,
};

use async_std::{
    fs::File,
    io::{prelude::*, BufReader},
};
use logos::{Lexer, Logos};

use crate::build::{
    ascii::{unescape_str, AsciiStr, UnescapeError},
    symbol_table::{SymbolRef, SymbolTable},
    syntax::parse::Parsable,
};

const TAB_SPACING: &str = "    ";

pub type TokenStream = Vec<Spanned<Token>>;
pub type Errors = Vec<Diagnostic>;

pub struct LexResult {
    pub stream: TokenStream,
    pub source: Arc<String>,
    pub lookup: Arc<Lookup>,
}

pub async fn lex(source: String, content: File) -> Result<LexResult, Errors> {
    let mut tokens = TokenStream::new();
    let mut errors = Errors::new();
    let source = Arc::new(source);

    let mut buf = BufReader::new(&content);
    let mut file = String::new();
    buf.read_to_string(&mut file).await.map_err(|err| {
        vec![error!(
            "encountered an unexpected error reading file `{source}: {err}`"
        )]
    })?;
    // Replace tabs with spaces to keep character spacing the same
    let file = Arc::new(file.replace('\t', TAB_SPACING));
    let lookup = Arc::new(Lookup::new(file.clone()));

    let mut lex = Token::lexer(&file).spanned();
    while let Some((tok, span)) = lex.next() {
        let s = Span::new(source.clone(), lookup.clone(), span);

        match tok {
            Ok(tok) => {
                let token = Spanned::new(tok, s);
                tokens.push(token);
            }
            Err(mut err) => {
                err.set_span(Some(s));
                err.set_message(format!("unrecognized token `{}`", lex.slice()));
                errors.push(err)
            }
        }
    }

    if errors.is_empty() {
        Ok(LexResult {
            stream: tokens,
            source,
            lookup,
        })
    } else {
        Err(errors)
    }
}

pub fn lex_string(content: &str, source: &str) -> Result<TokenStream, Errors> {
    todo!()
}

impl Parsable for Spanned<Token> {
    fn parse(cursor: &mut super::parse::Cursor) -> Result<Self, Diagnostic> {
        cursor.next().ok_or_else(|| spanned_error!(cursor.eof_span(), "expected token, found `EOF`"))
    }

    fn description(&self) -> &'static str {
        self.inner().description()
    }
}

#[derive(Logos, Debug, Clone, PartialEq)]
#[logos(error = Diagnostic)]
#[logos(extras = SymbolTable)]
#[logos(skip r"[ \t\f\n\r]")]
#[logos(skip r"//[^\n]*\n?")]
#[logos(skip r"/\*(?:[^*]|\*[^/])*\*/")]
pub enum Token {
    #[token("use", |_| Keyword::Use)]
    #[token("fn", |_| Keyword::Fn)]
    #[token("return", |_| Keyword::Return)]
    #[token("struct", |_| Keyword::Struct)]
    #[token("enum", |_| Keyword::Enum)]
    #[token("union", |_| Keyword::Union)]
    #[token("impl", |_| Keyword::Impl)]
    #[token("pub", |_| Keyword::Pub)]
    #[token("const", |_| Keyword::Const)]
    #[token("static", |_| Keyword::Static)]
    #[token("mut", |_| Keyword::Mut)]
    #[token("if", |_| Keyword::If)]
    #[token("else", |_| Keyword::Else)]
    #[token("match", |_| Keyword::Match)]
    #[token("for", |_| Keyword::For)]
    #[token("while", |_| Keyword::While)]
    #[token("loop", |_| Keyword::Loop)]
    #[token("break", |_| Keyword::Break)]
    #[token("continue", |_| Keyword::Continue)]
    #[token("let", |_| Keyword::Let)]
    #[token("namespace", |_| Keyword::Namespace)]
    #[token("as", |_| Keyword::As)]
    #[token("self", |_| Keyword::LowerSelf)]
    #[token("Self", |_| Keyword::UpperSelf)]
    Keyword(Keyword),

    #[token("(", |_| Delimeter::OpenParen)]
    #[token(")", |_| Delimeter::CloseParen)]
    #[token("[", |_| Delimeter::OpenBracket)]
    #[token("]", |_| Delimeter::CloseBracket)]
    #[token("{", |_| Delimeter::OpenBrace)]
    #[token("}", |_| Delimeter::CloseBrace)]
    Delimeter(Delimeter),

    #[token("+", |_| Punctuation::Plus)]
    #[token("-", |_| Punctuation::Minus)]
    #[token("*", |_| Punctuation::Star)]
    #[token("/", |_| Punctuation::Slash)]
    #[token("%", |_| Punctuation::Percent)]
    #[token("<<", |_| Punctuation::Shl)]
    #[token(">>", |_| Punctuation::Shr)]
    #[token("!", |_| Punctuation::Not)]
    #[token("^", |_| Punctuation::Caret)]
    #[token("<", |_| Punctuation::Lt)]
    #[token("<=", |_| Punctuation::Le)]
    #[token(">", |_| Punctuation::Gt)]
    #[token(">=", |_| Punctuation::Ge)]
    #[token("+=", |_| Punctuation::PlusEq)]
    #[token("-=", |_| Punctuation::MinusEq)]
    #[token("*=", |_| Punctuation::MulEq)]
    #[token("/=", |_| Punctuation::DivEq)]
    #[token("%=", |_| Punctuation::ModEq)]
    #[token("&=", |_| Punctuation::AndEq)]
    #[token("|=", |_| Punctuation::OrEq)]
    #[token("^=", |_| Punctuation::XorEq)]
    #[token("=", |_| Punctuation::Eq)]
    #[token("==", |_| Punctuation::EqEq)]
    #[token("!=", |_| Punctuation::NotEqual)]
    #[token("&", |_| Punctuation::And)]
    #[token("&&", |_| Punctuation::AndAnd)]
    #[token("|", |_| Punctuation::Pipe)]
    #[token("||", |_| Punctuation::PipePipe)]
    #[token(":", |_| Punctuation::Colon)]
    #[token("::", |_| Punctuation::DoubleColon)]
    #[token(";", |_| Punctuation::Semicolon)]
    #[token(",", |_| Punctuation::Comma)]
    #[token(".", |_| Punctuation::Dot)]
    #[token("->", |_| Punctuation::Arrow)]
    #[token("=>", |_| Punctuation::FatArrow)]
    #[token("?", |_| Punctuation::Question)]
    Punctuation(Punctuation),

    #[token("void", |_| Primitive::Void)]
    #[token("bool", |_| Primitive::Bool)]
    #[token("u8", |_| Primitive::U8)]
    #[token("u16", |_| Primitive::U16)]
    #[token("u24", |_| Primitive::U24)]
    #[token("u32", |_| Primitive::U32)]
    #[token("i8", |_| Primitive::I8)]
    #[token("i16", |_| Primitive::I16)]
    #[token("i24", |_| Primitive::I24)]
    #[token("i32", |_| Primitive::I32)]
    #[token("str", |_| Primitive::Str)]
    Primitive(Primitive),

    #[token("true", |_| true)]
    #[token("false", |_| false)]
    Boolean(bool),

    #[regex(r"0b[01][_01]*", Token::binary)]
    #[regex(r"0o[0-7][_0-7]*", Token::octal)]
    #[regex(r"-?[0-9][_0-9]*", Token::decimal)]
    #[regex(r"0x[0-9a-fA-F][_0-9a-fA-F]*", Token::hexadecimal)]
    #[regex(r"'[\x00-\x7F]*'", Token::char)]
    #[regex(r#"'\\[(\\)n"at0rbfv]'"#, Token::char)]
    #[regex(r"'\\x[[:xdigit:]]{1,2}'", Token::char)]
    Immediate(i64),

    #[regex(r#""((\\")|[\x00-\x21\x23-\x7F])*""#, Token::string)]
    #[regex(r##"r#"((\\")|[\x00-\x21\x23-\x7F])*"#"##, Token::raw_string)]
    String(AsciiStr),

    #[regex(r"[_a-zA-Z][_a-zA-Z0-9]*", Token::ident)]
    Ident(SymbolRef),
}

impl Token {
    pub fn description(&self) -> &'static str {
        use Token as TI;
        match self {
            TI::Boolean(_) => "boolean",
            TI::Primitive(_) => "type",
            TI::Immediate(_) => "integer",
            TI::String(_) => "string",
            TI::Ident(_) => "identifier",
            TI::Delimeter(del) => del.description(),
            TI::Keyword(key) => key.description(),
            TI::Punctuation(punc) => punc.description(),
        }
    }

    fn binary(lex: &mut Lexer<Token>) -> Option<i64> {
        let slice = lex.slice().replace("_", "");
        i64::from_str_radix(&slice.strip_prefix("0b")?, 2).ok()
    }

    fn octal(lex: &mut Lexer<Token>) -> Option<i64> {
        let slice = lex.slice().replace("_", "");
        i64::from_str_radix(&slice.strip_prefix("0o")?, 8).ok()
    }

    fn decimal(lex: &mut Lexer<Token>) -> Option<i64> {
        let slice = lex.slice().replace("_", "");
        i64::from_str_radix(&slice, 10).ok()
    }

    fn hexadecimal(lex: &mut Lexer<Token>) -> Option<i64> {
        let slice = lex.slice().replace("_", "");
        i64::from_str_radix(&slice.strip_prefix("0x")?, 16).ok()
    }

    fn char(lex: &mut Lexer<Token>) -> Result<i64, Diagnostic> {
        let slice = lex.slice();
        Self::char_from_str(slice).map(|c| c.into())
    }

    fn char_from_str(s: &str) -> Result<u8, Diagnostic> {
        let inner = s
            .strip_prefix('\'')
            .ok_or_else(|| error!("char not prefixed with `'`"))?
            .strip_suffix('\'')
            .ok_or_else(|| error!("char not suffixed with `'`"))?;

        let escaped = unescape_str(inner).map_err(|err| {
            Diagnostic::error(match err {
                UnescapeError::InvalidAscii(byte) => format!("invalid ASCII character: {byte}"),
                UnescapeError::UnmatchedBackslash(index) => {
                    format!("unmatched `\\` at string index {index}")
                }
            })
        })?;
        Ok(escaped[0])
    }

    fn string(lex: &mut Lexer<Token>) -> Result<AsciiStr, Diagnostic> {
        let slice = lex
            .slice()
            .strip_prefix("\"")
            .ok_or_else(|| error!("string not prefixed with `\"`"))?
            .strip_suffix("\"")
            .ok_or_else(|| error!("string not suffixed with `\"`"))?;

        Ok(unescape_str(&slice).map_err(|err| {
            Diagnostic::error(match err {
                UnescapeError::InvalidAscii(byte) => format!("invalid ASCII character: {byte}"),
                UnescapeError::UnmatchedBackslash(index) => {
                    format!("unmatched '\\' at string index {index}")
                }
            })
        })?)
    }

    fn raw_string(lex: &mut Lexer<Token>) -> Result<AsciiStr, Diagnostic> {
        let slice = lex
            .slice()
            .strip_prefix("r#\"")
            .ok_or_else(|| error!("string not prefixed with `r#\"`"))?
            .strip_suffix("#\"")
            .ok_or_else(|| error!("string not suffixed with `\"#`"))?;

        Ok(unescape_str(&slice).map_err(|err| {
            Diagnostic::error(match err {
                UnescapeError::InvalidAscii(byte) => format!("invalid ASCII character: {byte}"),
                UnescapeError::UnmatchedBackslash(index) => {
                    format!("unmatched `\\` at string index {index}")
                }
            })
        })?)
    }

    fn ident(lex: &mut Lexer<Token>) -> SymbolRef {
        lex.extras.get_or_insert(lex.slice())
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Keyword {
    Use,
    Fn,
    Return,
    Struct,
    Enum,
    Union,
    Impl,
    Pub,
    Const,
    Static,
    Mut,
    If,
    Else,
    Match,
    For,
    While,
    Loop,
    Break,
    Continue,
    Let,
    Namespace,
    As,
    LowerSelf,
    UpperSelf,
}

impl Keyword {
    fn description(&self) -> &'static str {
        match self {
            Keyword::Use => "keyword `use`",
            Keyword::Fn => "keyword `fn`",
            Keyword::Return => "keyword `return`",
            Keyword::Struct => "keyword `struct`",
            Keyword::Enum => "keyword `enum`",
            Keyword::Union => "keyword `union`",
            Keyword::Impl => "keyword `impl`",
            Keyword::Pub => "keyword `pub`",
            Keyword::Const => "keyword `const`",
            Keyword::Static => "keyword `static`",
            Keyword::Mut => "keyword `mut`",
            Keyword::If => "keyword `if`",
            Keyword::Else => "keyword `else`",
            Keyword::Match => "keyword `match",
            Keyword::For => "keyword `for`",
            Keyword::While => "keyword `while`",
            Keyword::Loop => "keyword `loop`",
            Keyword::Break => "keyword `break`",
            Keyword::Continue => "keyword `continue`",
            Keyword::Let => "keyword `let`",
            Keyword::Namespace => "keyword `namespace`",
            Keyword::As => "keyword `as`",
            Keyword::LowerSelf => "keyword `self`",
            Keyword::UpperSelf => "keyword `Self`",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Primitive {
    Void,
    Bool,
    U8,
    U16,
    U24,
    U32,
    I8,
    I16,
    I24,
    I32,
    Str,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Punctuation {
    /// `+`
    Plus,
    /// `-`
    Minus,
    /// `*`
    Star,
    /// `/`
    Slash,
    /// `%`
    Percent,
    /// `<<`
    Shl,
    /// `>>`
    Shr,
    /// `!`
    Not,
    /// `^`
    Caret,
    /// `<`
    Lt,
    /// `<=`
    Le,
    /// `>`
    Gt,
    /// `>=`
    Ge,
    /// `=`
    Eq,
    /// `+=`
    PlusEq,
    /// `-=`
    MinusEq,
    /// `*=`
    MulEq,
    /// `/=`
    DivEq,
    /// `%=`
    ModEq,
    /// `&=`
    AndEq,
    /// `|=`
    OrEq,
    /// `^=`
    XorEq,
    /// `==`
    EqEq,
    /// `!=`
    NotEqual,
    /// `&`
    And,
    /// `&&`
    AndAnd,
    /// `|`
    Pipe,
    /// `||`
    PipePipe,
    /// `:`
    Colon,
    /// `::`
    DoubleColon,
    /// `;`
    Semicolon,
    /// `,`
    Comma,
    /// `.`
    Dot,
    /// `->`
    Arrow,
    /// `=>`
    FatArrow,
    /// `?`
    Question,
}

impl Punctuation {
    fn description(&self) -> &'static str {
        match self {
            Punctuation::Plus => "`+`",
            Punctuation::Minus => "`-`",
            Punctuation::Star => "`*`",
            Punctuation::Slash => "`/`",
            Punctuation::Percent => "`%`",
            Punctuation::Shl => "`<<`",
            Punctuation::Shr => "`>>`",
            Punctuation::Not => "`!`",
            Punctuation::Caret => "`^`",
            Punctuation::Lt => "`<`",
            Punctuation::Le => "`<=`",
            Punctuation::Gt => "`>`",
            Punctuation::Ge => "`>=`",
            Punctuation::Eq => "`=`",
            Punctuation::PlusEq => "`+=`",
            Punctuation::MinusEq => "`-=`",
            Punctuation::MulEq => "`*=`",
            Punctuation::DivEq => "`/=`",
            Punctuation::ModEq => "`%=`",
            Punctuation::AndEq => "`&=`",
            Punctuation::OrEq => "`|=`",
            Punctuation::XorEq => "`^=`",
            Punctuation::EqEq => "`==`",
            Punctuation::NotEqual => "`!=`",
            Punctuation::And => "`&`",
            Punctuation::AndAnd => "`&&`",
            Punctuation::Pipe => "`|`",
            Punctuation::PipePipe => "`||`",
            Punctuation::Colon => "`:`",
            Punctuation::DoubleColon => "`::`",
            Punctuation::Semicolon => "`;`",
            Punctuation::Comma => "`,`",
            Punctuation::Dot => "`.`",
            Punctuation::Arrow => "`->`",
            Punctuation::FatArrow => "`=>`",
            Punctuation::Question => "`?`",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Delimeter {
    /// `(`
    OpenParen,
    /// `)`
    CloseParen,
    /// `[`
    OpenBracket,
    /// `]`
    CloseBracket,
    /// `{`
    OpenBrace,
    /// `}`
    CloseBrace,
}

impl Delimeter {
    fn description(&self) -> &'static str {
        match self {
            Delimeter::OpenParen => "`(`",
            Delimeter::CloseParen => "`)`",
            Delimeter::OpenBracket => "`[`",
            Delimeter::CloseBracket => "`]`",
            Delimeter::OpenBrace => "`{`",
            Delimeter::CloseBrace => "`}`",
        }
    }
}
