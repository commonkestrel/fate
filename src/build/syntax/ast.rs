use indexmap::IndexMap;

use crate::{
    build::ascii::AsciiStr,
    debug,
    diagnostic::Diagnostic,
    error, punctuated,
    span::{Span, Spanned},
    spanned_debug, spanned_error, Token,
};

use super::{
    lex::{self, Delimeter, Keyword, Punctuation, Token},
    parse::{Braced, Cursor, Parenthesized, Parsable, Punctuated},
    token::{
        Break, CloseBrace, CloseBracket, CloseParen, Colon, Comma, Continue, DoubleColon, Eq, Gt,
        Ident, Mut, OpenBrace, Semicolon,
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
    Composite(Path),
    Err(Diagnostic),
}

impl Type {
    pub fn bubble_errors(&self, output: &mut Vec<Diagnostic>) {
        match self {
            Type::Primitive(_) => {}
            Type::Composite(_) => {}
            Type::Vector(types) => types.iter().for_each(|ty| ty.bubble_errors(output)),
            Type::Pointer { ty, .. } => ty.bubble_errors(output),
            Type::Array { ty, len } => {
                ty.bubble_errors(output);
                len.bubble_errors(output);
            }
            Type::Err(err) => output.push(err.clone()),
        }
    }
}

impl Parsable for Spanned<Type> {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        let (tok, span) = cursor
            .next()
            .ok_or_else(|| spanned_error!(cursor.eof_span(), "expected type, found `EOF`"))?
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
            Token::Ident(_) => {
                cursor.step_back();

                let mut path_inner = Vec::new();
                let mut last_ident: Option<Spanned<Ident>> = None;

                while let Some(tok) = cursor.peek().cloned() {
                    match tok.inner() {
                        Token::Ident(_) => {
                            if last_ident.is_some() {
                                return Ok(Spanned::new(
                                    Type::Err(spanned_error!(
                                        tok.span().clone(),
                                        "expected seperator, found duplicate ident"
                                    )),
                                    tok.span().clone(),
                                ));
                            }

                            last_ident = Some(match cursor.parse() {
                                Ok(ident) => ident,
                                Err(err) => {
                                    return Ok(Spanned::new(Type::Err(err), tok.into_span()))
                                }
                            });
                        }
                        Token::Punctuation(Punctuation::DoubleColon) => match last_ident.take() {
                            Some(l) => path_inner.push((
                                l,
                                match cursor.parse() {
                                    Ok(field) => field,
                                    Err(err) => {
                                        return Ok(Spanned::new(Type::Err(err), tok.into_span()))
                                    }
                                },
                            )),
                            None => {
                                return Ok(Spanned::new(
                                    Type::Err(spanned_error!(
                                        tok.span().clone(),
                                        "unexpected duplicate seperator"
                                    )),
                                    tok.into_span(),
                                ))
                            }
                        },
                        _ => break,
                    }
                }

                let path = Punctuated::new(path_inner, last_ident);

                let path_span = span.to(path.last().unwrap().span());
                Ok(Spanned::new(Type::Composite(path), path_span))
            }
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
    ident: Spanned<Ident>,
    fields: Punctuated<FieldDef, Comma>,
}

impl Parsable for Spanned<Struct> {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        let open: Spanned<Token![struct]> = cursor.parse()?;
        let ident: Spanned<Ident> = cursor.parse()?;
        let _: OpenBrace = cursor.parse()?;

        let fields = punctuated!(
            cursor,
            !Token::Delimeter(Delimeter::CloseBrace),
            Token::Punctuation(Punctuation::Comma)
        )?;

        let close: Spanned<CloseBrace> = cursor.parse()?;

        let struct_span = open.span().to(close.span());
        Ok(Spanned::new(Struct { ident, fields }, struct_span))
    }

    fn description(&self) -> &'static str {
        "struct definition"
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Union {
    ident: Spanned<Ident>,
    fields: Punctuated<FieldDef, Comma>,
}

impl Parsable for Spanned<Union> {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        let open: Spanned<Token![union]> = cursor.parse()?;
        let ident: Spanned<Ident> = cursor.parse()?;
        let _: OpenBrace = cursor.parse()?;

        let fields = punctuated!(
            cursor,
            !Token::Delimeter(Delimeter::CloseBrace),
            Token::Punctuation(Punctuation::Comma)
        )?;

        let close: Spanned<CloseBrace> = cursor.parse()?;

        let union_span = open.span().to(close.span());
        Ok(Spanned::new(Union { ident, fields }, union_span))
    }

    fn description(&self) -> &'static str {
        "union definition"
    }
}

#[derive(Debug, Clone, PartialEq)]
struct FieldDef {
    ident: Spanned<Ident>,
    ty: Spanned<Type>,
}

impl Parsable for FieldDef {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        let ident: Spanned<Ident> = cursor.parse()?;
        let ty = if cursor.check(&Token::Punctuation(Punctuation::Colon)) {
            cursor.step();
            cursor.parse()?
        } else {
            Spanned::new(
                Type::Err(spanned_error!(
                    ident.span().clone(),
                    "missing type for field"
                )),
                ident.span().clone(),
            )
        };

        Ok(FieldDef { ident, ty })
    }

    fn description(&self) -> &'static str {
        "field definition"
    }
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
    ident: Spanned<Ident>,
    variants: Punctuated<Spanned<Variant>, Comma>,
}

#[derive(Debug, Clone)]
struct Variant {
    ident: Spanned<Ident>,
    ty: VariantType,
}

impl Parsable for Spanned<Variant> {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        let ident: Spanned<Ident> = cursor.parse()?;
        let ty: VariantType = cursor.parse()?;

        let variant_span = match ty {
            VariantType::Void | VariantType::Err(_) => ident.span().clone(),
            VariantType::Vector(ref types) => ident.span().to(types.span()),
            VariantType::Struct(ref fields) => ident.span().to(fields.span()),
        };

        Ok(Spanned::new(Variant { ident, ty }, variant_span))
    }

    fn description(&self) -> &'static str {
        "enum variant"
    }
}

#[derive(Debug, Clone)]
enum VariantType {
    Void,
    Vector(Spanned<Punctuated<Spanned<Type>, Comma>>),
    Struct(Spanned<Punctuated<FieldDef, Comma>>),
    Err(Diagnostic),
}

impl Parsable for VariantType {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        match cursor.peek().map(Spanned::inner) {
            Some(Token::Punctuation(Punctuation::Lt)) => {
                let open: Spanned<Token![<]> = cursor.parse()?;

                let mut types_inner = Vec::new();
                let mut last_type = None;

                while let Some(tok) = cursor.peek().cloned() {
                    match tok.inner() {
                        Token::Punctuation(Punctuation::Comma) => match last_type.take() {
                            Some(l) => types_inner.push((
                                l,
                                match cursor.parse() {
                                    Ok(field) => field,
                                    Err(err) => return Ok(VariantType::Err(err)),
                                },
                            )),
                            None => {
                                return Ok(VariantType::Err(spanned_error!(
                                    tok.span().clone(),
                                    "unexpected duplicate seperator"
                                )));
                            }
                        },
                        Token::Punctuation(Punctuation::Gt) => break,
                        _ => {
                            if last_type.is_some() {
                                return Ok(VariantType::Err(spanned_error!(
                                    tok.span().clone(),
                                    "expected seperator, found duplicate ident"
                                )));
                            }

                            last_type = Some(match cursor.parse() {
                                Ok(ident) => ident,
                                Err(err) => return Ok(VariantType::Err(err)),
                            });
                        }
                    }
                }

                let types = Punctuated::new(types_inner, last_type);

                let close: Spanned<Token![>]> = cursor.parse()?;

                Ok(VariantType::Vector(Spanned::new(
                    types,
                    open.span().to(close.span()),
                )))
            }
            Some(Token::Delimeter(Delimeter::OpenBrace)) => {
                let open: Spanned<OpenBrace> = cursor.parse()?;

                let fields = punctuated!(
                    cursor,
                    !Token::Delimeter(Delimeter::CloseBrace),
                    Token::Punctuation(Punctuation::Comma)
                )?;

                let close: Spanned<CloseBrace> = cursor.parse()?;

                Ok(VariantType::Struct(Spanned::new(
                    fields,
                    open.span().to(close.span()),
                )))
            }
            _ => Ok(VariantType::Void),
        }
    }

    fn description(&self) -> &'static str {
        "variant"
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Immediate(i64),
    Str(AsciiStr),
    Reference(Path),
    Call(Box<Spanned<Expr>>, Punctuated<Spanned<Expr>, Token![,]>),
    Constructor {
        ident: Path,
        components: Punctuated<Spanned<Expr>, Token![,]>,
    },
    NamedConstructor {
        ident: Path,
        fields: Punctuated<Spanned<Field>, Token![,]>,
    },
    Vector(Vec<Spanned<Expr>>),
    BinaryOp(Box<BinOp>),
    UnaryOp(Spanned<UnaryOp>, Box<Spanned<Expr>>),
    Cast(Spanned<Type>, Box<Spanned<Expr>>),
    Err(Diagnostic),
}

impl Parsable for Spanned<Expr> {
    #[inline]
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        Ok(Expr::parse_assignment(cursor))
    }

    fn description(&self) -> &'static str {
        "expression"
    }
}

impl Expr {
    pub fn bubble_errors(&self, output: &mut Vec<Diagnostic>) {
        match self {
            Expr::Immediate(_) => {}
            Expr::Reference(_) => {}
            Expr::Str(_) => {}
            Expr::Cast(_, expr) => expr.bubble_errors(output),
            Expr::Call(expr, params) => {
                expr.bubble_errors(output);
                params
                    .values()
                    .for_each(|param| param.bubble_errors(output));
            }
            Expr::Constructor { ident, components } => components
                .values()
                .for_each(|component| component.bubble_errors(output)),
            Expr::NamedConstructor { ident, fields } => fields
                .values()
                .for_each(|field| field.value.bubble_errors(output)),
            Expr::Vector(exprs) => exprs.iter().for_each(|expr| expr.bubble_errors(output)),
            Expr::BinaryOp(op) => {
                op.left.bubble_errors(output);
                op.right.bubble_errors(output);
            }
            Expr::UnaryOp(_, expr) => expr.bubble_errors(output),
            Expr::Err(err) => output.push(err.clone()),
        }
    }

    fn parse_assignment(cursor: &mut Cursor) -> Spanned<Self> {
        let mut a = Expr::parse_boolean(cursor);

        while let Some(tok) = cursor.peek().cloned() {
            match tok.inner() {
                Token::Punctuation(Punctuation::Eq) => {
                    cursor.step();
                    let b = Expr::parse_boolean(cursor);
                    let expr_span = a.span().to(b.span());
                    a = Spanned::new(
                        Expr::BinaryOp(BinOp::boxed(
                            a,
                            Spanned::new(BinaryOp::Assign, tok.span().clone()),
                            b,
                        )),
                        expr_span,
                    );
                }
                Token::Punctuation(Punctuation::PlusEq) => {
                    cursor.step();
                    let b = Expr::parse_boolean(cursor);
                    let expr_span = a.span().to(b.span());
                    a = Spanned::new(
                        Expr::BinaryOp(BinOp::boxed(
                            a,
                            Spanned::new(BinaryOp::AddEq, tok.span().clone()),
                            b,
                        )),
                        expr_span,
                    );
                }
                Token::Punctuation(Punctuation::MinusEq) => {
                    cursor.step();
                    let b = Expr::parse_boolean(cursor);
                    let expr_span = a.span().to(b.span());
                    a = Spanned::new(
                        Expr::BinaryOp(BinOp::boxed(
                            a,
                            Spanned::new(BinaryOp::SubEq, tok.span().clone()),
                            b,
                        )),
                        expr_span,
                    );
                }
                Token::Punctuation(Punctuation::MulEq) => {
                    cursor.step();
                    let b = Expr::parse_boolean(cursor);
                    let expr_span = a.span().to(b.span());
                    a = Spanned::new(
                        Expr::BinaryOp(BinOp::boxed(
                            a,
                            Spanned::new(BinaryOp::MulEq, tok.span().clone()),
                            b,
                        )),
                        expr_span,
                    );
                }
                Token::Punctuation(Punctuation::DivEq) => {
                    cursor.step();
                    let b = Expr::parse_boolean(cursor);
                    let expr_span = a.span().to(b.span());
                    a = Spanned::new(
                        Expr::BinaryOp(BinOp::boxed(
                            a,
                            Spanned::new(BinaryOp::DivEq, tok.span().clone()),
                            b,
                        )),
                        expr_span,
                    );
                }
                Token::Punctuation(Punctuation::ModEq) => {
                    cursor.step();
                    let b = Expr::parse_boolean(cursor);
                    let expr_span = a.span().to(b.span());
                    a = Spanned::new(
                        Expr::BinaryOp(BinOp::boxed(
                            a,
                            Spanned::new(BinaryOp::ModEq, tok.span().clone()),
                            b,
                        )),
                        expr_span,
                    );
                }
                Token::Punctuation(Punctuation::AndEq) => {
                    cursor.step();
                    let b = Expr::parse_boolean(cursor);
                    let expr_span = a.span().to(b.span());
                    a = Spanned::new(
                        Expr::BinaryOp(BinOp::boxed(
                            a,
                            Spanned::new(BinaryOp::AndEq, tok.span().clone()),
                            b,
                        )),
                        expr_span,
                    );
                }
                Token::Punctuation(Punctuation::OrEq) => {
                    cursor.step();
                    let b = Expr::parse_boolean(cursor);
                    let expr_span = a.span().to(b.span());
                    a = Spanned::new(
                        Expr::BinaryOp(BinOp::boxed(
                            a,
                            Spanned::new(BinaryOp::OrEq, tok.span().clone()),
                            b,
                        )),
                        expr_span,
                    );
                }
                Token::Punctuation(Punctuation::XorEq) => {
                    cursor.step();
                    let b = Expr::parse_boolean(cursor);
                    let expr_span = a.span().to(b.span());
                    a = Spanned::new(
                        Expr::BinaryOp(BinOp::boxed(
                            a,
                            Spanned::new(BinaryOp::XorEq, tok.span().clone()),
                            b,
                        )),
                        expr_span,
                    );
                }
                _ => return a,
            }
        }

        a
    }

    fn parse_boolean(cursor: &mut Cursor) -> Spanned<Self> {
        let mut a = Expr::parse_comparison(cursor);

        while let Some(tok) = cursor.peek().cloned() {
            match tok.inner() {
                Token::Punctuation(Punctuation::AndAnd) => {
                    cursor.step();
                    let b = Expr::parse_comparison(cursor);
                    let expr_span = a.span().to(b.span());
                    a = Spanned::new(
                        Expr::BinaryOp(BinOp::boxed(
                            a,
                            Spanned::new(BinaryOp::And, tok.span().clone()),
                            b,
                        )),
                        expr_span,
                    );
                }
                Token::Punctuation(Punctuation::PipePipe) => {
                    cursor.step();
                    let b = Expr::parse_comparison(cursor);
                    let expr_span = a.span().to(b.span());
                    a = Spanned::new(
                        Expr::BinaryOp(BinOp::boxed(
                            a,
                            Spanned::new(BinaryOp::Or, tok.span().clone()),
                            b,
                        )),
                        expr_span,
                    );
                }
                _ => return a,
            }
        }

        a
    }

    fn parse_comparison(cursor: &mut Cursor) -> Spanned<Self> {
        let mut a = Expr::parse_bitwise(cursor);

        while let Some(tok) = cursor.peek().cloned() {
            match tok.inner() {
                Token::Punctuation(Punctuation::NotEqual) => {
                    cursor.step();
                    let b = Expr::parse_bitwise(cursor);
                    let expr_span = a.span().to(b.span());
                    a = Spanned::new(
                        Expr::BinaryOp(BinOp::boxed(
                            a,
                            Spanned::new(BinaryOp::Ne, tok.span().clone()),
                            b,
                        )),
                        expr_span,
                    );
                }
                Token::Punctuation(Punctuation::EqEq) => {
                    cursor.step();
                    let b = Expr::parse_bitwise(cursor);
                    let expr_span = a.span().to(b.span());
                    a = Spanned::new(
                        Expr::BinaryOp(BinOp::boxed(
                            a,
                            Spanned::new(BinaryOp::Equality, tok.span().clone()),
                            b,
                        )),
                        expr_span,
                    );
                }
                Token::Punctuation(Punctuation::Lt) => {
                    cursor.step();
                    let b = Expr::parse_bitwise(cursor);
                    let expr_span = a.span().to(b.span());
                    a = Spanned::new(
                        Expr::BinaryOp(BinOp::boxed(
                            a,
                            Spanned::new(BinaryOp::Lt, tok.span().clone()),
                            b,
                        )),
                        expr_span,
                    );
                }
                Token::Punctuation(Punctuation::Le) => {
                    cursor.step();
                    let b = Expr::parse_bitwise(cursor);
                    let expr_span = a.span().to(b.span());
                    a = Spanned::new(
                        Expr::BinaryOp(BinOp::boxed(
                            a,
                            Spanned::new(BinaryOp::Le, tok.span().clone()),
                            b,
                        )),
                        expr_span,
                    );
                }
                Token::Punctuation(Punctuation::Gt) => {
                    cursor.step();
                    let b = Expr::parse_bitwise(cursor);
                    let expr_span = a.span().to(b.span());
                    a = Spanned::new(
                        Expr::BinaryOp(BinOp::boxed(
                            a,
                            Spanned::new(BinaryOp::Gt, tok.span().clone()),
                            b,
                        )),
                        expr_span,
                    );
                }
                Token::Punctuation(Punctuation::Ge) => {
                    cursor.step();
                    let b = Expr::parse_bitwise(cursor);
                    let expr_span = a.span().to(b.span());
                    a = Spanned::new(
                        Expr::BinaryOp(BinOp::boxed(
                            a,
                            Spanned::new(BinaryOp::Ge, tok.span().clone()),
                            b,
                        )),
                        expr_span,
                    );
                }
                _ => return a,
            }
        }

        a
    }

    fn parse_bitwise(cursor: &mut Cursor) -> Spanned<Self> {
        let mut a = Expr::parse_expression(cursor);

        while let Some(tok) = cursor.peek().cloned() {
            match tok.inner() {
                Token::Punctuation(Punctuation::And) => {
                    cursor.step();
                    let b = Expr::parse_expression(cursor);
                    let expr_span = a.span().to(b.span());
                    a = Spanned::new(
                        Expr::BinaryOp(BinOp::boxed(
                            a,
                            Spanned::new(BinaryOp::BitAnd, tok.span().clone()),
                            b,
                        )),
                        expr_span,
                    );
                }
                Token::Punctuation(Punctuation::Pipe) => {
                    cursor.step();
                    let b = Expr::parse_expression(cursor);
                    let expr_span = a.span().to(b.span());
                    a = Spanned::new(
                        Expr::BinaryOp(BinOp::boxed(
                            a,
                            Spanned::new(BinaryOp::BitOr, tok.span().clone()),
                            b,
                        )),
                        expr_span,
                    );
                }
                Token::Punctuation(Punctuation::Caret) => {
                    cursor.step();
                    let b = Expr::parse_expression(cursor);
                    let expr_span = a.span().to(b.span());
                    a = Spanned::new(
                        Expr::BinaryOp(BinOp::boxed(
                            a,
                            Spanned::new(BinaryOp::BitXor, tok.span().clone()),
                            b,
                        )),
                        expr_span,
                    );
                }
                _ => return a,
            }
        }

        a
    }

    fn parse_expression(cursor: &mut Cursor) -> Spanned<Self> {
        let mut a = Expr::parse_terminal(cursor);

        while let Some(tok) = cursor.peek().cloned() {
            match tok.inner() {
                Token::Punctuation(Punctuation::Plus) => {
                    cursor.step();
                    let b = Expr::parse_terminal(cursor);
                    let expr_span = a.span().to(b.span());
                    a = Spanned::new(
                        Expr::BinaryOp(BinOp::boxed(
                            a,
                            Spanned::new(BinaryOp::Add, tok.span().clone()),
                            b,
                        )),
                        expr_span,
                    );
                }
                Token::Punctuation(Punctuation::Minus) => {
                    cursor.step();
                    let b = Expr::parse_terminal(cursor);
                    let expr_span = a.span().to(b.span());
                    a = Spanned::new(
                        Expr::BinaryOp(BinOp::boxed(
                            a,
                            Spanned::new(BinaryOp::Sub, tok.span().clone()),
                            b,
                        )),
                        expr_span,
                    );
                }
                _ => return a,
            }
        }

        a
    }

    fn parse_terminal(cursor: &mut Cursor) -> Spanned<Self> {
        let mut a = Expr::parse_factor(cursor);

        // SAFETY: cursor is guarenteed to outlive `tok`,
        // and stream is not mutable in any way
        while let Some(tok) = cursor.stream().get(cursor.position).cloned() {
            match tok.inner() {
                Token::Punctuation(Punctuation::Star) => {
                    cursor.step();
                    let b = Expr::parse_factor(cursor);
                    let expr_span = a.span().to(b.span());
                    a = Spanned::new(
                        Expr::BinaryOp(BinOp::boxed(
                            a,
                            Spanned::new(BinaryOp::Mul, tok.span().clone()),
                            b,
                        )),
                        expr_span,
                    );
                }
                Token::Punctuation(Punctuation::Slash) => {
                    cursor.step();
                    let b = Expr::parse_factor(cursor);
                    let expr_span = a.span().to(b.span());
                    a = Spanned::new(
                        Expr::BinaryOp(BinOp::boxed(
                            a,
                            Spanned::new(BinaryOp::Div, tok.span().clone()),
                            b,
                        )),
                        expr_span,
                    );
                }
                Token::Punctuation(Punctuation::Percent) => {
                    cursor.step();
                    let b = Expr::parse_factor(cursor);
                    let expr_span = a.span().to(b.span());
                    a = Spanned::new(
                        Expr::BinaryOp(BinOp::boxed(
                            a,
                            Spanned::new(BinaryOp::Mod, tok.span().clone()),
                            b,
                        )),
                        expr_span,
                    );
                }
                Token::Delimeter(Delimeter::OpenParen) => {
                    cursor.step();
                    let mut params_inner = Vec::new();
                    let mut last_param = None;

                    while let Some(tok) = cursor.peek().cloned() {
                        match tok.inner() {
                            Token::Delimeter(Delimeter::CloseParen) => break,
                            Token::Punctuation(Punctuation::Comma) => match last_param.take() {
                                Some(l) => params_inner.push((
                                    l,
                                    match cursor.parse() {
                                        Ok(field) => field,
                                        Err(err) => {
                                            return Spanned::new(Expr::Err(err), tok.span().clone())
                                        }
                                    },
                                )),
                                None => {
                                    return Spanned::new(
                                        Expr::Err(spanned_error!(
                                            tok.span().clone(),
                                            "unexpected duplicate seperator"
                                        )),
                                        tok.span().clone(),
                                    )
                                }
                            },
                            _ => {
                                last_param = Some(match cursor.parse() {
                                    Ok(field) => field,
                                    Err(err) => {
                                        return Spanned::new(Expr::Err(err), tok.span().clone())
                                    }
                                })
                            }
                        }
                    }

                    let params = Punctuated::new(params_inner, last_param);

                    let close: Spanned<CloseParen> = match cursor.parse() {
                        Ok(close) => close,
                        Err(err) => return Spanned::new(Expr::Err(err), tok.span().clone()),
                    };

                    let call_span = a.span().to(close.span());
                    a = Spanned::new(Expr::Call(Box::new(a), params), call_span);
                }
                _ => return a,
            }
        }

        a
    }

    fn parse_factor(cursor: &mut Cursor) -> Spanned<Self> {
        let (tok, span) = match cursor.next() {
            Some(next) => next.deconstruct(),
            None => {
                return Spanned::new(
                    Expr::Err(spanned_error!(
                        cursor.eof_span(),
                        "expected expression, found `EOF`"
                    )),
                    cursor.eof_span(),
                )
            }
        };

        match tok {
            Token::Immediate(i) => Spanned::new(Expr::Immediate(i), span),
            Token::String(str) => Spanned::new(Expr::Str(str), span),
            Token::Punctuation(Punctuation::Lt) => {
                let mut components = Vec::new();

                while !cursor.at_end() {
                    if cursor.check(&Token::Punctuation(Punctuation::Gt)) {
                        let close: Spanned<Gt> = match cursor.parse() {
                            Ok(gt) => gt,
                            Err(err) => return Spanned::new(Expr::Err(err), span),
                        };
                        let vec_span = span.to(close.span());

                        return Spanned::new(Expr::Vector(components), vec_span);
                    }

                    let expr = match cursor.parse() {
                        Ok(expr) => expr,
                        Err(err) => return Spanned::new(Expr::Err(err), span),
                    };
                    components.push(expr);
                    if !cursor.check(&Token::Punctuation(Punctuation::Gt)) {
                        let _: Comma = match cursor.parse() {
                            Ok(comma) => comma,
                            Err(err) => return Spanned::new(Expr::Err(err), span),
                        };
                    }
                }

                Spanned::new(
                    Expr::Err(spanned_error!(span.clone(), "unmatched opening arrow")),
                    span,
                )
            }
            Token::Delimeter(Delimeter::OpenParen) => {
                let a = Expr::parse_assignment(cursor);

                match cursor.next().map(Spanned::into_inner) {
                    Some(Token::Delimeter(Delimeter::CloseParen)) => a,
                    _ => Spanned::new(
                        Expr::Err(spanned_error!(
                            span.clone(),
                            "unmatched opening parenthesis"
                        )),
                        span,
                    ),
                }
            }
            Token::Punctuation(Punctuation::Not) => {
                let expr = Expr::parse_factor(cursor);
                let not_span = span.to(expr.span());
                Spanned::new(
                    Expr::UnaryOp(
                        Spanned::new(UnaryOp::Not, span),
                        Box::new(Expr::parse_factor(cursor)),
                    ),
                    not_span,
                )
            }
            Token::Punctuation(Punctuation::Star) => {
                let expr = Expr::parse_factor(cursor);
                let deref_span = span.to(expr.span());

                Spanned::new(
                    Expr::UnaryOp(Spanned::new(UnaryOp::Deref, span), Box::new(expr)),
                    deref_span,
                )
            }
            Token::Punctuation(Punctuation::Minus) => {
                let expr = Expr::parse_factor(cursor);
                let neg_span = span.to(expr.span());

                Spanned::new(
                    Expr::UnaryOp(Spanned::new(UnaryOp::Negative, span), Box::new(expr)),
                    neg_span,
                )
            }
            Token::Punctuation(Punctuation::And) => {
                let next = cursor.peek();
                let (mutability, op_span) = match next.map(Spanned::inner) {
                    Some(Token::Keyword(Keyword::Mut)) => (
                        Mutability::Mut,
                        match cursor.parse::<Spanned<Mut>>() {
                            Ok(key) => key,
                            Err(err) => return Spanned::new(Expr::Err(err), span),
                        }
                        .span()
                        .clone(),
                    ),
                    _ => (Mutability::Const, span),
                };
                let expr = Expr::parse_factor(cursor);
                let addr_span = op_span.to(expr.span());

                Spanned::new(
                    Expr::UnaryOp(
                        Spanned::new(UnaryOp::Addr(mutability), op_span),
                        Box::new(expr),
                    ),
                    addr_span,
                )
            }
            Token::Ident(_) => {
                cursor.step_back();

                let mut path_inner = Vec::new();
                let mut last_ident = None;

                while let Some(tok) = cursor.peek().cloned() {
                    match tok.inner() {
                        Token::Ident(_) => {
                            if last_ident.is_some() {
                                return Spanned::new(
                                    Expr::Err(spanned_error!(
                                        tok.span().clone(),
                                        "expected seperator, found duplicate ident"
                                    )),
                                    tok.span().clone(),
                                );
                            }

                            last_ident = Some(match cursor.parse() {
                                Ok(ident) => ident,
                                Err(err) => return Spanned::new(Expr::Err(err), tok.into_span()),
                            });
                        }
                        Token::Punctuation(Punctuation::DoubleColon) => match last_ident.take() {
                            Some(l) => path_inner.push((
                                l,
                                match cursor.parse() {
                                    Ok(field) => field,
                                    Err(err) => {
                                        return Spanned::new(Expr::Err(err), tok.into_span())
                                    }
                                },
                            )),
                            None => {
                                return Spanned::new(
                                    Expr::Err(spanned_error!(
                                        tok.span().clone(),
                                        "unexpected duplicate seperator"
                                    )),
                                    tok.into_span(),
                                )
                            }
                        },
                        _ => break,
                    }
                }

                let path = Punctuated::new(path_inner, last_ident);

                match cursor.peek().map(Spanned::inner) {
                    Some(Token::Delimeter(Delimeter::OpenBrace)) => {
                        cursor.step();
                        let mut fields_inner = Vec::new();
                        let mut last_field = None;

                        while let Some(tok) = cursor.peek() {
                            match tok.inner() {
                                Token::Delimeter(Delimeter::CloseBrace) => break,
                                Token::Punctuation(Punctuation::Comma) => match last_field.take() {
                                    Some(l) => fields_inner.push((
                                        l,
                                        match cursor.parse() {
                                            Ok(field) => field,
                                            Err(err) => return Spanned::new(Expr::Err(err), span),
                                        },
                                    )),
                                    None => {
                                        return Spanned::new(
                                            Expr::Err(spanned_error!(
                                                tok.span().clone(),
                                                "unexpected duplicate seperator"
                                            )),
                                            span,
                                        )
                                    }
                                },
                                _ => {
                                    last_field = Some(match cursor.parse() {
                                        Ok(field) => field,
                                        Err(err) => return Spanned::new(Expr::Err(err), span),
                                    })
                                }
                            }
                        }

                        let fields = Punctuated::new(fields_inner, last_field);
                        let close: Spanned<CloseBrace> = match cursor.parse() {
                            Ok(field) => field,
                            Err(err) => return Spanned::new(Expr::Err(err), span),
                        };

                        Spanned::new(
                            Expr::NamedConstructor {
                                ident: path,
                                fields,
                            },
                            span.to(close.span()),
                        )
                    }
                    Some(Token::Punctuation(Punctuation::Lt)) => {
                        let open: Spanned<Token![<]> = cursor.parse().unwrap();

                        let mut offset = 0;
                        while let Some(tok) = cursor.peek_offset(offset) {
                            

                            offset += 1;
                        }

                        let mut components_inner = Vec::new();
                        let mut last_component = None;

                        while let Some(tok) = cursor.peek() {
                            match tok.inner() {
                                Token::Punctuation(Punctuation::Gt) => {
                                    let components = Punctuated::new(components_inner, last_component);
                                    let close: Spanned<Token![>]> = match cursor.parse() {
                                        Ok(field) => field,
                                        Err(err) => return Spanned::new(Expr::Err(err), span),
                                    };

                                    return Spanned::new(
                                        Expr::Constructor {
                                            ident: path,
                                            components,
                                        },
                                        span.to(close.span()),
                                    );
                                },
                                Token::Punctuation(Punctuation::Comma) => {
                                    match last_component.take() {
                                        Some(l) => {
                                            cursor.step();
                                            components_inner.push((l, Token![,]));
                                        }
                                        None => {
                                            return Spanned::new(
                                                Expr::Err(spanned_error!(
                                                    tok.span().clone(),
                                                    "unexpected duplicate seperator"
                                                )),
                                                span,
                                            )
                                        }
                                    }
                                }
                                _ => {
                                    last_component = Some(match cursor.parse() {
                                        Ok(field) => field,
                                        Err(err) => return Spanned::new(Expr::Err(err), span),
                                    })
                                }
                            }
                        }

                        Spanned::new(Expr::Err(spanned_error!(open.span().clone(), "unmatched opening arrow")), open.span().clone())
                    }
                    _ => {
                        let path_span = span.to(path.last().unwrap().span());
                        Spanned::new(Expr::Reference(path), path_span)
                    }
                }
            }
            _ => Spanned::new(
                Expr::Err(spanned_error!(
                    span.clone(),
                    "expected expression, found {}",
                    tok.description()
                )),
                span,
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinOp {
    left: Spanned<Expr>,
    op: Spanned<BinaryOp>,
    right: Spanned<Expr>,
}

impl BinOp {
    fn boxed(left: Spanned<Expr>, op: Spanned<BinaryOp>, right: Spanned<Expr>) -> Box<BinOp> {
        Box::new(BinOp { left, op, right })
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Assign,
    AddEq,
    SubEq,
    MulEq,
    DivEq,
    ModEq,
    AndEq,
    OrEq,
    XorEq,
    Equality,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
    BitAnd,
    BitOr,
    BitXor,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum UnaryOp {
    Negative,
    Not,
    Deref,
    Addr(Mutability),
}

#[derive(Debug, Clone)]
pub enum Statement {
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

impl Statement {
    pub fn bubble_errors(&self, output: &mut Vec<Diagnostic>) {
        match self {
            Statement::Break | Statement::Continue => {}
            Statement::Expr(expr) => expr.bubble_errors(output),
            Statement::If {
                condition,
                content,
                else_block,
            } => {
                condition.bubble_errors(output);
                content.bubble_errors(output);
                if let Some(block) = else_block {
                    block.bubble_errors(output);
                }
            }
            Statement::Loop(content) => content.bubble_errors(output),
            Statement::For { contents, header } => {
                header.init.bubble_errors(output);
                header.check.bubble_errors(output);
                header.post.bubble_errors(output);

                contents.bubble_errors(output);
            }
            Statement::While { check, contents } => {
                check.bubble_errors(output);
                contents.bubble_errors(output);
            }
            Statement::Let { assignment, ty, .. } => {
                assignment.bubble_errors(output);
                ty.bubble_errors(output);
            }
            Statement::Return(expr) => expr.bubble_errors(output),
            Statement::Block(content) => content.iter().for_each(|s| s.bubble_errors(output)),
        }
    }
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
                let ident: Spanned<Ident> = cursor.parse()?;

                let ty: Spanned<Type> = if cursor.check(&Token::Punctuation(Punctuation::Colon)) {
                    cursor.step();
                    match cursor.parse() {
                        Ok(ty) => ty,
                        Err(err) => {
                            cursor.step_back();
                            Spanned::new(Type::Err(err), ident.span().clone())
                        }
                    }
                } else {
                    Spanned::new(
                        Type::Err(spanned_error!(
                            ident.span().clone(),
                            "missing type for variable declaration"
                        )),
                        ident.span().clone(),
                    )
                };

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
                    let peek = cursor.peek().unwrap();
                    spanned_debug!(peek.span().clone(), "{:?}", peek.inner()).sync_emit();
                    if cursor.check(&Token::Delimeter(Delimeter::CloseBrace)) {
                        let close: Spanned<CloseBrace> = cursor.parse()?;
                        let block_span = span.to(close.span());

                        return Ok(Spanned::new(Statement::Block(statements), block_span));
                    }

                    statements.push(cursor.parse()?);
                    let mut errors = Vec::new();
                    statements.iter().for_each(|s| s.bubble_errors(&mut errors));
                    errors.into_iter().for_each(Diagnostic::sync_emit);
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
            Some(Token::Keyword(Keyword::Mut)) => {
                cursor.step();
                Ok(Mutability::Mut)
            }
            _ => Ok(Mutability::Const),
        }
    }

    fn description(&self) -> &'static str {
        "mutability modifier"
    }
}

#[derive(Debug, Clone)]
pub struct FnDefinition {
    pub ident: Spanned<Ident>,
    pub parameters: Parenthesized<Punctuated<FnParam, Comma>>,
    pub return_type: Spanned<Type>,
    pub body: Spanned<Statement>,
}

impl Parsable for Spanned<FnDefinition> {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        let open: Spanned<Token![fn]> = cursor.parse()?;
        let ident: Spanned<Ident> = cursor.parse()?;
        let parameters = cursor.parse()?;

        let return_type = if cursor.check(&Token::Punctuation(Punctuation::Colon)) {
            cursor.step();
            cursor.parse()?
        } else {
            Spanned::new(
                Type::Err(spanned_error!(ident.span().clone(), "missing return type")),
                ident.span().clone(),
            )
        };

        let body: Spanned<Statement> = cursor.parse()?;

        let def_span = open.span().to(body.span());

        Ok(Spanned::new(
            FnDefinition {
                ident,
                parameters,
                return_type,
                body,
            },
            def_span,
        ))
    }

    fn description(&self) -> &'static str {
        "function definition"
    }
}

#[derive(Debug, Clone)]
pub struct FnParam {
    pub mutability: Mutability,
    pub ident: Spanned<Ident>,
    pub ty: Spanned<Type>,
}

impl Parsable for FnParam {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        let mutability = cursor.parse()?;
        let ident = cursor.parse()?;
        let _: Colon = cursor.parse()?;
        let ty: Spanned<Type> = cursor.parse()?;

        Ok(FnParam {
            mutability,
            ident,
            ty,
        })
    }

    fn description(&self) -> &'static str {
        "function parameter"
    }
}

impl Parsable for Spanned<FnParam> {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        let open = cursor.peek().map(|next| next.span().clone());
        let mutability = cursor.parse()?;
        let ident = cursor.parse()?;
        let _: Colon = cursor.parse()?;
        let ty: Spanned<Type> = cursor.parse()?;

        // SAFETY: if `cursor.peek()` returned `None`, this would have been caught when parsing `ident`
        let param_span = unsafe { open.unwrap_unchecked() }.to(ty.span());

        Ok(Spanned::new(
            FnParam {
                mutability,
                ident,
                ty,
            },
            param_span,
        ))
    }

    fn description(&self) -> &'static str {
        "function parameter"
    }
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
