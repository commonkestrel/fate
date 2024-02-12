use super::parser::{Parsable, Cursor};
use super::lexer::{Delimeter, Keyword, Punctuation, Token, TokenInner};
use crate::diagnostic::Diagnostic;
use crate::span::Span;
use crate::{error, spanned_error};

macro_rules! parsable {
    ($($description:literal: $token:ident($inner:pat) => $name:ident),* $(,)?) => {
        $(
            #[derive(Debug, Clone, PartialEq)]
            pub struct $name {
                pub span: Span,
            }

            impl Parsable for $name {
                fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
                    match cursor.next() {
                        Some(Token { inner: TokenInner::$token($inner), span }) => Ok(Self { span }),
                        Some(tok) => Err(spanned_error!(tok.span, concat!("expected ", $description, ", found {}"), tok.inner.description())),
                        None => Err(error!(concat!("expected ", $description, ", found EOF"),))
                    }
                }

                fn description(&self) -> &'static str {
                    $description
                }
            }
        )*
    };
    ($($description:literal: $token:ident($inner:pat) => $name:ident{$($v:vis $field:ident: $ty:ty)*}),* $(,)?) => {
        $(
            #[derive(Debug, Clone, PartialEq)]
            pub struct $name {
                pub span: Span,
                $(
                    $vis $field: $ty,
                )*
            }

            impl Parsable for $name {
                fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
                    match cursor.next() {
                        Some(Token { inner: TokenInner::$token($inner), span }) => Ok(Self { span, $($field)* }),
                        Some(tok) => Err(spanned_error!(tok.span, concat!("expected ", $description, ", found {}"), tok.inner.description())),
                        None => Err(error!(concat!("expected ", $description, ", found EOF"),))
                    }
                }

                fn description(&self) -> &'static str {
                    $description
                }
            }
        )*
    };
}

// Delimeters
parsable! {
    "`(`" : Delimeter(Delimeter::OpeningParen) => OpeningParen,
    "`)`"  : Delimeter(Delimeter::ClosingParen) => ClosedParen,
    "`[`"  : Delimeter(Delimeter::OpeningBracket) => OpenBracket,
    "`]`"  : Delimeter(Delimeter::ClosingBracket) => ClosedBracket,
    "`{{`" : Delimeter(Delimeter::OpeningBrace) => OpenBrace,
    "`}}`" : Delimeter(Delimeter::ClosingBrace) => ClosedBrace,
}

// Punctuation
parsable! {
    "="  : Punctuation(Punctuation::Eq) => Eq,
    "==" : Punctuation(Punctuation::EqEq) => EqEq,
    "!=" : Punctuation(Punctuation::NotEqual) => Ne,
    "<"  : Punctuation(Punctuation::Lt) => Lt,
    "<=" : Punctuation(Punctuation::Le) => Le,
    ">"  : Punctuation(Punctuation::Gt) => Gt,
    ">=" : Punctuation(Punctuation::Ge) => Ge,
    "&"  : Punctuation(Punctuation::And) => And,
    "&&" : Punctuation(Punctuation::AndAnd) => AndAnd,
    "|"  : Punctuation(Punctuation::Pipe) => Pipe,
    "||" : Punctuation(Punctuation::PipePipe) => PipePipe,
    "^"  : Punctuation(Punctuation::Caret) => Caret,
    "!"  : Punctuation(Punctuation::Not) => Not,
    "+"  : Punctuation(Punctuation::Plus) => Plus,
    "-"  : Punctuation(Punctuation::Minus) => Minus,
    "/"  : Punctuation(Punctuation::Slash) => Slash,
    "*"  : Punctuation(Punctuation::Star) => Star,
    "<<" : Punctuation(Punctuation::Shl) => Shl,
    ">>" : Punctuation(Punctuation::Shr) => Shr,
    ","  : Punctuation(Punctuation::Comma) => Comma,
    ":"  : Punctuation(Punctuation::Colon) => Colon,
}

parsable! {
    "`use`" : Keyword(Keyword::Use) => Use,
    "`fn`" : Keyword(Keyword::Fn) => Fn,
    "`return`" : Keyword(Keyword::Return) => Return,
    "`struct`" : Keyword(Keyword::Struct) => Struct,
    "`enum`" : Keyword(Keyword::Enum) => Enum,
    "`impl`" : Keyword(Keyword::Impl) => Impl,
    "`pub`" : Keyword(Keyword::Pub) => Pub,
    "`const`" : Keyword(Keyword::Const) => Const,
    "`static`" : Keyword(Keyword::Static) => Static,
    "`mut`" : Keyword(Keyword::Mut) => Mut,
    "`if`" : Keyword(Keyword::If) => If,
    "`else`" : Keyword(Keyword::Else) => Else,
    "`match`" : Keyword(Keyword::Match) => Match,
    "`for`" : Keyword(Keyword::For) => For,
    "`while`" : Keyword(Keyword::While) => While,
    "`break`" : Keyword(Keyword::Break) => Break,
    "`continue`" : Keyword(Keyword::Continue) => Continue,
    "`let`" : Keyword(Keyword::Let) => Let,
    "`namespace`" : Keyword(Keyword::Namespace) => Namespace,
    "`lowerself`" : Keyword(Keyword::LowerSelf) => LowerSelf,
    "`upperself`" : Keyword(Keyword::UpperSelf) => UpperSelf,
}
