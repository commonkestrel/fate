use super::lex::{Delimeter, Keyword, Punctuation, Token};
use super::parse::{Cursor, Parsable};
use crate::build::ascii::AsciiStr;
use crate::build::symbol_table::SymbolRef;
use crate::diagnostic::Diagnostic;
use crate::span::Spanned;
use crate::{error, spanned_error};

#[macro_export]
macro_rules! Token {
    [=] => {$crate::build::syntax::token::Eq};
    [==] => {$crate::build::syntax::token::EqEq};
    [!=] => {$crate::build::syntax::token::Ne};
    [<] => {$crate::build::syntax::token::Lt};
    [<=] => {$crate::build::syntax::token::Le};
    [>] => {$crate::build::syntax::token::Gt};
    [>=] => {$crate::build::syntax::token::Ge};
    [&] => {$crate::build::syntax::token::And};
    [&&] => {$crate::build::syntax::token::AndAnd};
    [|] => {$crate::build::syntax::token::Or};
    [||] => {$crate::build::syntax::token::OrOr};
    [^] => {$crate::build::syntax::token::Caret};
    [!] => {$crate::build::syntax::token::Not};
    [~] => {$crate::build::syntax::token::Not};
    [+] => {$crate::build::syntax::token::Plus};
    [-] => {$crate::build::syntax::token::Minus};
    [/] => {$crate::build::syntax::token::Slash};
    [*] => {$crate::build::syntax::token::Star};
    [<<] => {$crate::build::syntax::token::Shl};
    [>>] => {$crate::build::syntax::token::Shr};
    [,] => {$crate::build::syntax::token::Comma};
    [:] => {$crate::build::syntax::token::Colon};
    [;] => {$crate::build::syntax::token::Semicolon};
    [if] => {$crate::build::syntax::token::If};
    [else] => {$crate::build::syntax::token::Else};
    [use] => {$crate::build::syntax::token::Use};
    [fn] => {$crate::build::syntax::token::Fn};
    [return] => {$crate::build::syntax::token::Return};
    [struct] => {$crate::build::syntax::token::Struct};
    [enum] => {$crate::build::syntax::token::Enum};
    [union] => {$crate::build::syntax::token::Union};
    [impl] => {$crate::build::syntax::token::Impl};
    [pub] => {$crate::build::syntax::token::Pub};
    [const] => {$crate::build::syntax::token::Const};
    [static] => {$crate::build::syntax::token::Static};
    [mut] => {$crate::build::syntax::token::Mut};
    [match] => {$crate::build::syntax::token::Match};
    [for] => {$crate::build::syntax::token::For};
    [while] => {$crate::build::syntax::token::While};
    [loop] => {$crate::build::syntax::token::Loop};
    [break] => {$crate::build::syntax::token::Break};
    [continue] => {$crate::build::syntax::token::Continue};
    [let] => {$crate::build::syntax::token::Let};
    [namespace] => {$crate::build::syntax::token::Namespace};
    [interface] => {$crate::build::syntax::token::Interface};
    [as] => {$crate::build::syntax::token::As};
    [self] => {$crate::build::syntax::token::LowerSelf};
    [Self] => {$crate::build::syntax::token::UpperSelf};
    ["("] => {$crate::build::syntax::token::OpenParen};
    [")"] => {$crate::build::syntax::token::CloseParen};
    ["["] => {$crate::build::syntax::token::OpenBracket};
    ["]"] => {$crate::build::syntax::token::CloseBracket};
    ["{"] => {$crate::build::syntax::token::OpenBrace};
    ["}"] => {$crate::build::syntax::token::CloseBrace};
}

macro_rules! parsable {
    ($($description:literal: $token:ident($inner:pat) => $name:ident),* $(,)?) => {
        $(
            #[derive(Debug, Clone, PartialEq)]
            pub struct $name;

            impl Parsable for $name {
                fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
                    let next = cursor.next().map(Spanned::deconstruct);
                    match next {
                        Some((Token::$token($inner), _)) => Ok($name),
                        Some((tok, span)) => Err(spanned_error!(span, concat!("expected ", $description, ", found {}"), tok.description())),
                        None => Err(spanned_error!(cursor.eof_span(), concat!("expected ", $description, ", found `EOF`")))
                    }
                }

                fn description(&self) -> &'static str {
                    $description
                }
            }

            impl Parsable for Spanned<$name> {
                fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
                    let next = cursor.next().map(Spanned::deconstruct);
                    match next {
                        Some((Token::$token($inner), span)) => Ok(Spanned::new($name, span)),
                        Some((tok, span)) => Err(spanned_error!(span, concat!("expected ", $description, ", found {}"), tok.description())),
                        None => Err(spanned_error!(cursor.eof_span(), concat!("expected ", $description, ", found `EOF`")))
                    }
                }

                fn description(&self) -> &'static str {
                    $description
                }
            }

            impl Parsable for Option<$name> {
                fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
                    let next = cursor.next().map(Spanned::into_inner);
                    match next {
                        Some(Token::$token($inner)) => Ok(Some($name)),
                        _ => Ok(None),
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
                        Some((Token::$token($inner), span)) => Ok(Spanned::new($name { $($field)* }, span)),
                        Some((tok, span)) => Err(spanned_error!(span, concat!("expected ", $description, ", found {}"), tok.description())),
                        None => Err(spanned_error!(cursor.eof_span(), concat!("expected ", $description, ", found `EOF`")))
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
    "`=`"  : Punctuation(Punctuation::Eq) => Eq,
    "`==`" : Punctuation(Punctuation::EqEq) => EqEq,
    "`!=`" : Punctuation(Punctuation::NotEqual) => Ne,
    "`<`"  : Punctuation(Punctuation::Lt) => Lt,
    "`<=`" : Punctuation(Punctuation::Le) => Le,
    "`>`"  : Punctuation(Punctuation::Gt) => Gt,
    "`>=`" : Punctuation(Punctuation::Ge) => Ge,
    "`&`"  : Punctuation(Punctuation::And) => And,
    "`&&`" : Punctuation(Punctuation::AndAnd) => AndAnd,
    "`|`"  : Punctuation(Punctuation::Pipe) => Pipe,
    "`||`" : Punctuation(Punctuation::PipePipe) => PipePipe,
    "`^`"  : Punctuation(Punctuation::Caret) => Caret,
    "`!`"  : Punctuation(Punctuation::Not) => Not,
    "`+`"  : Punctuation(Punctuation::Plus) => Plus,
    "`-`"  : Punctuation(Punctuation::Minus) => Minus,
    "`/`"  : Punctuation(Punctuation::Slash) => Slash,
    "`*`"  : Punctuation(Punctuation::Star) => Star,
    "`%`"  : Punctuation(Punctuation::Percent) => Percent,
    "`+=`" : Punctuation(Punctuation::PlusEq) => PlusEq,
    "`-=`" : Punctuation(Punctuation::MinusEq) => MinusEq,
    "`*=`" : Punctuation(Punctuation::MulEq) => MulEq,
    "`/=`" : Punctuation(Punctuation::DivEq) => DivEq,
    "`%=`" : Punctuation(Punctuation::ModEq) => ModEq,
    "`<<`" : Punctuation(Punctuation::Shl) => Shl,
    "`>>`" : Punctuation(Punctuation::Shr) => Shr,
    "`,`"  : Punctuation(Punctuation::Comma) => Comma,
    "`;`"  : Punctuation(Punctuation::Semicolon) => Semicolon,
    "`:`"  : Punctuation(Punctuation::Colon) => Colon,
    "`::`" : Punctuation(Punctuation::DoubleColon) => DoubleColon,
    "`?`"  : Punctuation(Punctuation::Question) => Question,
}

//------ Keywords ------//

parsable! {
    "keyword `use`" : Keyword(Keyword::Use) => Use,
    "keyword `fn`" : Keyword(Keyword::Fn) => Fn,
    "keyword `return`" : Keyword(Keyword::Return) => Return,
    "keyword `struct`" : Keyword(Keyword::Struct) => Struct,
    "keyword `enum`" : Keyword(Keyword::Enum) => Enum,
    "keyword `union`" : Keyword(Keyword::Union) => Union,
    "keyword `impl`" : Keyword(Keyword::Impl) => Impl,
    "keyword `pub`" : Keyword(Keyword::Pub) => Pub,
    "keyword `const`" : Keyword(Keyword::Const) => Const,
    "keyword `static`" : Keyword(Keyword::Static) => Static,
    "keyword `mut`" : Keyword(Keyword::Mut) => Mut,
    "keyword `if`" : Keyword(Keyword::If) => If,
    "keyword `else`" : Keyword(Keyword::Else) => Else,
    "keyword `match`" : Keyword(Keyword::Match) => Match,
    "keyword `for`" : Keyword(Keyword::For) => For,
    "keyword `while`" : Keyword(Keyword::While) => While,
    "keyword `loop`" : Keyword(Keyword::Loop) => Loop,
    "keyword `break`" : Keyword(Keyword::Break) => Break,
    "keyword `continue`" : Keyword(Keyword::Continue) => Continue,
    "keyword `let`" : Keyword(Keyword::Let) => Let,
    "keyword `namespace`" : Keyword(Keyword::Namespace) => Namespace,
    "keyword `interface`" : Keyword(Keyword::Interface) => Interface,
    "keyword `as`" : Keyword(Keyword::As) => As,
    "keyword `self`" : Keyword(Keyword::LowerSelf) => LowerSelf,
    "keyword `Self`" : Keyword(Keyword::UpperSelf) => UpperSelf,
}

//------ Containers ------//

parsable! {
    "integer" : Immediate(value) => Immediate { pub value: i64 },
    "string" : String(value) => StringLit { pub value: AsciiStr },
    "identifier" : Ident(symbol) => Ident { pub symbol: SymbolRef },
}
