use super::ascii::AsciiStr;
use super::lexer::{Delimeter, Keyword, Punctuation, TokenInner};
use super::symbol_table::SymbolRef;
use super::syntax::{Cursor, Parsable};
use crate::diagnostic::Diagnostic;
use crate::span::Spanned;
use crate::{error, spanned_error};

macro_rules! parsable {
    ($($description:literal: $token:ident($inner:pat) => $name:ident),* $(,)?) => {
        $(
            #[derive(Debug, Clone, PartialEq)]
            pub struct $name;

            impl Parsable for Spanned<$name> {
                fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
                    let next = cursor.next().map(Spanned::deconstruct);
                    match next {
                        Some((TokenInner::$token($inner), span)) => Ok(Spanned::new($name, span)),
                        Some((tok, span)) => Err(spanned_error!(span, concat!("expected ", $description, ", found {}"), tok.description())),
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
                $(
                    $v $field: $ty,
                )*
            }

            impl Parsable for Spanned<$name> {
                fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
                    let next = cursor.next().map(Spanned::deconstruct);
                    match next {
                        Some((TokenInner::$token($inner), span)) => Ok(Spanned::new($name { $($field)* }, span)),
                        Some((tok, span)) => Err(spanned_error!(span, concat!("expected ", $description, ", found {}"), tok.description())),
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

//------ Delimeters ------//

parsable! {
    "`(`" : Delimeter(Delimeter::OpenParen) => OpenParen,
    "`)`"  : Delimeter(Delimeter::CloseParen) => CloseParen,
    "`[`"  : Delimeter(Delimeter::OpenBracket) => OpenBracket,
    "`]`"  : Delimeter(Delimeter::CloseBracket) => CloseBracket,
    "`{{`" : Delimeter(Delimeter::OpenBrace) => OpenBrace,
    "`}}`" : Delimeter(Delimeter::CloseBrace) => CloseBrace,
}

//------ Punctuation ------//



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
    "+=" : Punctuation(Punctuation::PlusEq) => PlusEq,
    "-=" : Punctuation(Punctuation::MinusEq) => MinusEq,
    "*=" : Punctuation(Punctuation::MulEq) => MulEq,
    "/=" : Punctuation(Punctuation::DivEq) => DivEq,
    "<<" : Punctuation(Punctuation::Shl) => Shl,
    ">>" : Punctuation(Punctuation::Shr) => Shr,
    ","  : Punctuation(Punctuation::Comma) => Comma,
    ";"  : Punctuation(Punctuation::Semicolon) => Semicolon,
    ":"  : Punctuation(Punctuation::Colon) => Colon,
    "::" : Punctuation(Punctuation::DoubleColon) => DoubleColon,
    "?"  : Punctuation(Punctuation::Question) => Question,
}

//------ Keywords ------//

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

//------ Containers ------//

parsable! {
    "integer" : Immediate(value) => Immediate { pub value: i64 },
    "string" : String(value) => StringLit { pub value: AsciiStr },
    "identifier" : Ident(position) => Ident { pub position: SymbolRef },
}
