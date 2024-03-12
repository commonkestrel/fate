use indexmap::IndexMap;

use crate::{
    diagnostic::Diagnostic,
    error, punctuated,
    span::{Span, Spanned},
    spanned_error,
};

use super::{
    lex::{self, Delimeter, Keyword, Punctuation, Token},
    parse::{Braced, Cursor, Parenthesized, Parsable, Punctuated},
    token::{
        Break, CloseBrace, CloseBracket, Colon, Comma, Continue, DoubleColon, Eq, Gt, Ident, Mut,
        Semicolon,
    },
};

pub type Path = Punctuated<Spanned<Ident>, DoubleColon>;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Primitive(lex::Primitive),
    Pointer {
        mutability: Mutability,
        ty: Box<Spanned<Type>>,
    },
    Array {
        ty: Box<Spanned<Type>>,
        len: Box<Spanned<Expr>>,
    },
    Vector(Vec<Spanned<Type>>),
    Composite(Ident),
}

impl Parsable for Spanned<Type> {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        let (tok, span) = cursor
            .next()
            .ok_or_else(|| error!("expected type, found `EOF`"))?
            .deconstruct();
        match tok {
            Token::Primitive(ty) => Ok(Spanned::new(Type::Primitive(ty), span)),
            Token::Punctuation(Punctuation::Star) => {
                let mutability = cursor.parse()?;
                let ty: Spanned<Type> = cursor.parse()?;

                let ptr_span = span.to(ty.span());
                Ok(Spanned::new(
                    Type::Pointer {
                        mutability,
                        ty: Box::new(ty),
                    },
                    ptr_span,
                ))
            }
            Token::Delimeter(Delimeter::OpenBracket) => {
                let ty = cursor.parse()?;
                let _: Semicolon = cursor.parse()?;
                let len = cursor.parse()?;
                let close: Spanned<CloseBracket> = cursor.parse()?;

                let arr_span = span.to(close.span());
                Ok(Spanned::new(
                    Type::Array {
                        ty: Box::new(ty),
                        len: Box::new(len),
                    },
                    arr_span,
                ))
            }
            Token::Punctuation(Punctuation::Lt) => {
                let mut types = Vec::new();

                while !cursor.at_end() {
                    if cursor.check(&Token::Punctuation(Punctuation::Gt)) {
                        let close: Spanned<Gt> = cursor.parse()?;
                        let vec_span = span.to(close.span());

                        return Ok(Spanned::new(Type::Vector(types), vec_span));
                    }

                    let ty = cursor.parse()?;
                    types.push(ty);
                    if !cursor.check(&Token::Punctuation(Punctuation::Gt)) {
                        let _: Comma = cursor.parse()?;
                    }
                }

                Err(spanned_error!(span, "unmatched opening arrow"))
            }
            Token::Ident(symbol) => Ok(Spanned::new(Type::Composite(Ident { symbol }), span)),
            _ => Err(spanned_error!(
                span,
                "expected type, found {}",
                tok.description()
            )),
        }
    }

    fn description(&self) -> &'static str {
        "type"
    }
}

#[derive(Debug, Clone)]
struct Struct {
    fields: Vec<(Spanned<Ident>, Spanned<Type>)>,
}

#[derive(Debug, Clone, PartialEq)]
struct Field {
    ident: Ident,
    value: Expr,
}

impl Parsable for Spanned<Field> {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        let ident: Spanned<Ident> = cursor.parse()?;
        let _: Eq = cursor.parse()?;
        let value: Spanned<Expr> = cursor.parse()?;

        let field_span = ident.span().to(value.span());
        Ok(Spanned::new(
            Field {
                ident: ident.into_inner(),
                value: value.into_inner(),
            },
            field_span,
        ))
    }

    fn description(&self) -> &'static str {
        "field"
    }
}

#[derive(Debug, Clone)]
struct Enum {
    variants: Vec<(Spanned<Ident>, Spanned<Variant>)>,
}

#[derive(Debug, Clone)]
enum Variant {
    Void,
    Struct(Struct),
    Tuple(Vec<Type>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Immediate(i64),
    Reference(Path),
    Call(Box<Spanned<Expr>>, Vec<Expr>),
    Constructor {
        ident: Path,
        fields: Punctuated<Spanned<Field>, Comma>,
    },
    Vector(Vec<Spanned<Expr>>),
    BinaryOp(Box<BinOp>),
    UnaryOp(Spanned<UnaryOp>, Box<Spanned<Expr>>),
    Cast(Spanned<Type>, Box<Spanned<Expr>>),
}

impl Parsable for Spanned<Expr> {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        todo!()
    }

    fn description(&self) -> &'static str {
        "expression"
    }
}

impl Expr {
    fn parse_assignment(cursor: &mut Cursor) -> Result<Spanned<Self>, Diagnostic> {
        todo!()
    }

    fn parse_boolean(cursor: &mut Cursor) -> Result<Spanned<Self>, Diagnostic> {
        todo!()
    }

    fn parse_comparison(cursor: &mut Cursor) -> Result<Spanned<Self>, Diagnostic> {
        todo!()
    }

    fn parse_expression(cursor: &mut Cursor) -> Result<Spanned<Self>, Diagnostic> {
        todo!()
    }

    fn parse_terminal(cursor: &mut Cursor) -> Result<Spanned<Self>, Diagnostic> {
        todo!()
    }

    fn parse_factor(cursor: &mut Cursor) -> Result<Spanned<Self>, Diagnostic> {
        let (tok, span) = cursor
            .next()
            .ok_or_else(|| error!("expected expression, found `EOF`"))?
            .deconstruct();

        match tok {
            Token::Immediate(i) => Ok(Spanned::new(Expr::Immediate(i), span)),
            Token::Punctuation(Punctuation::Lt) => {
                let mut components = Vec::new();

                while !cursor.at_end() {
                    if cursor.check(&Token::Punctuation(Punctuation::Gt)) {
                        let close: Spanned<Gt> = cursor.parse()?;
                        let vec_span = span.to(close.span());

                        return Ok(Spanned::new(Expr::Vector(components), vec_span));
                    }

                    let ty = cursor.parse()?;
                    components.push(ty);
                    if !cursor.check(&Token::Punctuation(Punctuation::Gt)) {
                        let _: Comma = cursor.parse()?;
                    }
                }

                Err(spanned_error!(span, "unmatched opening arrow"))
            }
            Token::Delimeter(Delimeter::OpenParen) => {
                let a = Expr::parse_assignment(cursor)?;

                match cursor.next().map(Spanned::into_inner) {
                    Some(Token::Delimeter(Delimeter::CloseParen)) => Ok(a),
                    _ => Err(spanned_error!(span, "unmatched opening parenthesis")),
                }
            }
            Token::Punctuation(Punctuation::Not) => {
                let expr = Expr::parse_factor(cursor)?;
                let not_span = span.to(expr.span());
                Ok(Spanned::new(
                    Expr::UnaryOp(
                        Spanned::new(UnaryOp::Not, span),
                        Box::new(Expr::parse_factor(cursor)?),
                    ),
                    not_span,
                ))
            }
            Token::Punctuation(Punctuation::Star) => {
                let expr = Expr::parse_factor(cursor)?;
                let deref_span = span.to(expr.span());

                Ok(Spanned::new(
                    Expr::UnaryOp(Spanned::new(UnaryOp::Deref, span), Box::new(expr)),
                    deref_span,
                ))
            }
            Token::Punctuation(Punctuation::Minus) => {
                let expr = Expr::parse_factor(cursor)?;
                let neg_span = span.to(expr.span());

                Ok(Spanned::new(
                    Expr::UnaryOp(Spanned::new(UnaryOp::Negative, span), Box::new(expr)),
                    neg_span,
                ))
            }
            Token::Punctuation(Punctuation::And) => {
                let next = cursor.peek();
                let (mutability, op_span) = match next.map(Spanned::inner) {
                    Some(Token::Keyword(Keyword::Mut)) => (
                        Mutability::Mut,
                        cursor.parse::<Spanned<Mut>>()?.span().clone(),
                    ),
                    _ => (Mutability::Const, span),
                };
                let expr = Expr::parse_factor(cursor)?;
                let addr_span = op_span.to(expr.span());

                Ok(Spanned::new(
                    Expr::UnaryOp(
                        Spanned::new(UnaryOp::Addr(mutability), op_span),
                        Box::new(expr),
                    ),
                    addr_span,
                ))
            }
            Token::Ident(_) => {
                cursor.step_back();
                let path = cursor.parse()?;

                match cursor.peek().map(Spanned::inner) {
                    Some(Token::Delimeter(Delimeter::OpenBrace)) => {
                        cursor.step();
                        let fields = punctuated!(
                            cursor,
                            !Token::Delimeter(Delimeter::CloseBrace),
                            Token::Punctuation(Punctuation::Comma)
                        )?;
                        let close: Spanned<CloseBrace> = cursor.parse()?;

                        Ok(Spanned::new(
                            Expr::Constructor {
                                ident: path,
                                fields,
                            },
                            span.to(close.span()),
                        ))
                    }
                    _ => Ok(Spanned::new(Expr::Reference(path), span)),
                }
            }
            _ => Err(spanned_error!(
                span,
                "expected expression, found {}",
                tok.description()
            )),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinOp {
    left: Spanned<Expr>,
    op: Spanned<BinaryOp>,
    right: Spanned<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    AddEq,
    SubEq,
    MulEq,
    DivEq,
    ModEq,
    AndEq,
    OrEq,
    XorEq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    BitAnd,
    Or,
    BitOr,
    BitXor,
    Assign,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum UnaryOp {
    Negative,
    Not,
    Deref,
    Addr(Mutability),
}

enum Statement {
    Expr(Expr),
    Block(Vec<Spanned<Statement>>),
    If {
        condition: Spanned<Expr>,
        content: Box<Spanned<Statement>>,
        else_block: Option<Box<Spanned<Statement>>>,
    },
    Loop(Box<Spanned<Statement>>),
    For {
        header: Box<ForHeader>,
        contents: Box<Spanned<Statement>>,
    },
    While {
        check: Spanned<Expr>,
        contents: Box<Spanned<Statement>>,
    },
    Break,
    Continue,
    Return(Spanned<Expr>),
    Let {
        mutability: Mutability,
        ident: Spanned<Ident>,
        ty: Spanned<Type>,
        assignment: Spanned<Expr>,
    },
}

impl Parsable for Spanned<Statement> {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        let (tok, span) = cursor
            .next()
            .ok_or_else(|| error!("expected statement, found `EOF`"))?
            .deconstruct();

        match tok {
            Token::Keyword(Keyword::If) => {
                let condition = cursor.parse()?;
                let content: Spanned<Statement> = cursor.parse()?;
                let else_block: Option<Box<Spanned<Statement>>> =
                    if cursor.check(&Token::Keyword(Keyword::Else)) {
                        Some(Box::new(cursor.parse()?))
                    } else {
                        None
                    };

                let if_span = span.to(else_block
                    .as_ref()
                    .map(|e| e.span())
                    .unwrap_or(content.span()));

                Ok(Spanned::new(
                    Statement::If {
                        condition,
                        content: Box::new(content),
                        else_block,
                    },
                    if_span,
                ))
            }
            Token::Keyword(Keyword::Loop) => {
                let contents: Spanned<Statement> = cursor.parse()?;
                let loop_span = span.to(contents.span());
                Ok(Spanned::new(Statement::Loop(Box::new(contents)), loop_span))
            }
            Token::Keyword(Keyword::For) => {
                let init = cursor.parse()?;
                let _: Semicolon = cursor.parse()?;
                let check = cursor.parse()?;
                let _: Semicolon = cursor.parse()?;
                let post = cursor.parse()?;
                let _: Semicolon = cursor.parse()?;
                let header = ForHeader { init, check, post };

                let contents: Spanned<Statement> = cursor.parse()?;
                let for_span = span.to(contents.span());
                Ok(Spanned::new(
                    Statement::For {
                        header: Box::new(header),
                        contents: Box::new(contents),
                    },
                    for_span,
                ))
            }
            Token::Keyword(Keyword::While) => {
                let check = cursor.parse()?;
                let contents: Spanned<Statement> = cursor.parse()?;
                let while_span = span.to(contents.span());

                Ok(Spanned::new(
                    Statement::While {
                        check,
                        contents: Box::new(contents),
                    },
                    while_span,
                ))
            }
            Token::Keyword(Keyword::Break) => Ok(Spanned::new(Statement::Break, span)),
            Token::Keyword(Keyword::Continue) => Ok(Spanned::new(Statement::Continue, span)),
            Token::Keyword(Keyword::Return) => {
                let value: Spanned<Expr> = cursor.parse()?;
                let return_span = span.to(value.span());

                Ok(Spanned::new(Statement::Return(value), return_span))
            }
            Token::Keyword(Keyword::Let) => {
                let mutability = cursor.parse()?;
                let ident = cursor.parse()?;
                let _: Colon = cursor.parse()?;
                let ty = cursor.parse()?;
                let _: Eq = cursor.parse()?;
                let assignment: Spanned<Expr> = cursor.parse()?;

                let let_span = span.to(assignment.span());
                Ok(Spanned::new(
                    Statement::Let {
                        mutability,
                        ident,
                        ty,
                        assignment,
                    },
                    let_span,
                ))
            }
            Token::Delimeter(Delimeter::OpenBrace) => {
                let mut statements = Vec::new();
                while !cursor.at_end() {
                    if cursor.check(&Token::Delimeter(Delimeter::CloseBrace)) {
                        let close: Spanned<CloseBrace> = cursor.parse()?;
                        let block_span = span.to(close.span());

                        return Ok(Spanned::new(Statement::Block(statements), block_span));
                    }

                    statements.push(cursor.parse()?);
                    let _: Semicolon = cursor.parse()?;
                }

                Err(spanned_error!(span, "unmatched opening brace"))
            }
            _ => {
                cursor.step_back();
                Ok(cursor.parse::<Spanned<Expr>>()?.map(Statement::Expr))
            }
        }
    }

    fn description(&self) -> &'static str {
        "statement"
    }
}

#[derive(Debug, Clone, PartialEq)]
struct ForHeader {
    init: Spanned<Expr>,
    check: Spanned<Expr>,
    post: Spanned<Expr>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum Mutability {
    Mut,
    Const,
}

impl Parsable for Mutability {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        match cursor.peek().map(Spanned::inner) {
            Some(Token::Keyword(Keyword::Mut)) => Ok(Mutability::Mut),
            _ => Ok(Mutability::Const),
        }
    }

    fn description(&self) -> &'static str {
        "mutability modifier"
    }
}

struct FnDefinition {
    name: Spanned<Ident>,
    parameters: Punctuated<FnParam, Comma>,
    return_type: Spanned<Type>,
    body: Spanned<Statement>,
}

struct FnParam {
    mutability: Mutability,
    ident: Spanned<Ident>,
    ty: Spanned<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnCall {
    path: Path,
    parameters: Parenthesized<Punctuated<Spanned<Expr>, Comma>>,
}

impl Parsable for FnCall {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        let path = punctuated!(
            cursor,
            Token::Ident(_),
            Token::Punctuation(Punctuation::DoubleColon)
        )?;
        let parameters = cursor.parse()?;

        Ok(Self { path, parameters })
    }

    fn description(&self) -> &'static str {
        "function call"
    }
}
