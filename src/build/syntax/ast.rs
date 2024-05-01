use crate::{
    build::ascii::AsciiStr,
    debug,
    diagnostic::Diagnostic,
    error, punctuated, seek,
    span::{Span, Spanned},
    spanned_debug, spanned_error, Token,
};

use super::{
    lex::{self, Delimeter, Keyword, Punctuation, Token},
    parse::{Cursor, Parenthesized, Parsable, Punctuated},
    token::{
        CloseBrace, CloseBracket, CloseParen, Colon, Comma, DoubleColon, Eq, Ident, Mut, OpenBrace,
        OpenParen, Semicolon,
    },
};

#[derive(Debug, Clone, PartialEq)]
pub struct Path {
    inner: Punctuated<Spanned<Ident>, DoubleColon>,
}

impl Path {
    pub fn span(&self) -> Span {
        self.inner
            .first()
            .unwrap()
            .span()
            .to(self.inner.last().unwrap().span())
    }
}

impl Parsable for Path {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        let mut inner = Vec::new();
        let mut last = Some(cursor.parse()?);
        while let Some(tok) = cursor.peek() {
            match tok.inner() {
                Token::Ident(_) => last = Some(cursor.parse()?),
                Token::Punctuation(Punctuation::DoubleColon) => match last.take() {
                    Some(l) => inner.push((l, cursor.parse()?)),
                    None => {
                        return Err(spanned_error!(
                            tok.span().clone(),
                            "unexpected duplicate seperator",
                        ))
                    }
                },
                _ => break,
            }
        }

        Ok(Path {
            inner: Punctuated::new(inner, last),
        })
    }

    fn description(&self) -> &'static str {
        "path"
    }
}

impl From<Punctuated<Spanned<Ident>, DoubleColon>> for Path {
    fn from(value: Punctuated<Spanned<Ident>, DoubleColon>) -> Self {
        Path { inner: value }
    }
}

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
    Tuple(Vec<Spanned<Type>>),
    Composite {
        ident: Path,
        generics: Vec<Spanned<Type>>,
    },
    BigSelf,
    FnPtr {
        params: Punctuated<Spanned<Type>, Token![,]>,
        return_type: Box<Spanned<Type>>,
    },
    Err,
}

impl Type {
    pub fn contains_errors(&self) -> bool {
        match self {
            Type::BigSelf => false,
            Type::Primitive(_) => false,
            Type::Composite { ident: _, generics } => {
                generics.iter().any(|gen| gen.contains_errors())
            }
            Type::Tuple(types) => types.iter().any(|ty| ty.contains_errors()),
            Type::Pointer { ty, .. } => ty.contains_errors(),
            Type::Array { ty, len } => ty.contains_errors() || len.contains_errors(),
            Type::FnPtr {
                params,
                return_type,
            } => params.values().any(|ty| ty.contains_errors()) || return_type.contains_errors(),
            Type::Err => true,
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
            Token::Keyword(Keyword::UpperSelf) => Ok(Spanned::new(Type::BigSelf, span)),
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
            Token::Delimeter(Delimeter::OpenParen) => {
                let mut types = Vec::new();

                while !cursor.at_end() {
                    if cursor.check(&Token::Delimeter(Delimeter::CloseParen)) {
                        let close: Spanned<Token![")"]> = cursor.parse()?;
                        let vec_span = span.to(close.span());

                        return Ok(Spanned::new(Type::Tuple(types), vec_span));
                    }

                    let ty = cursor.parse()?;
                    types.push(ty);
                    if !cursor.check(&Token::Delimeter(Delimeter::CloseParen)) {
                        let _: Comma = cursor.parse()?;
                    }
                }

                Err(spanned_error!(span, "unmatched opening parenthesis"))
            }
            Token::Keyword(Keyword::Fn) => {
                let open_paren: Spanned<OpenParen> = cursor.parse()?;

                let mut types = Vec::new();
                let mut last_type = None;

                while let Some(tok) = cursor.peek().cloned() {
                    match tok.inner() {
                        Token::Delimeter(Delimeter::CloseParen) => {
                            cursor.step();
                            let params = Punctuated::new(types, last_type);
                            let _: Token![:] = cursor.parse()?;
                            let ret: Spanned<Type> = cursor.parse()?;

                            return Ok(Spanned::new(
                                Type::FnPtr {
                                    params,
                                    return_type: Box::new(ret),
                                },
                                open_paren.span().to(tok.span()),
                            ));
                        }
                        Token::Punctuation(Punctuation::Comma) => match last_type.take() {
                            Some(ty) => types.push((ty, cursor.parse()?)),
                            None => {
                                cursor.reporter().report_sync(spanned_error!(
                                    tok.span().clone(),
                                    "unmatched duplicate seperator"
                                ));
                                types.push((
                                    Spanned::new(Type::Err, tok.into_span()),
                                    cursor.parse()?,
                                ))
                            }
                        },
                        inner => {
                            if last_type.is_some() {
                                cursor.reporter().report_sync(spanned_error!(
                                    tok.span().clone(),
                                    "expected seperator or closing parenthesis, found {}",
                                    inner.description()
                                ));

                                return Ok(Spanned::new(Type::Err, tok.into_span()));
                            }

                            last_type = Some(match cursor.parse() {
                                Ok(ty) => ty,
                                Err(err) => {
                                    cursor.reporter().report_sync(err);
                                    Spanned::new(Type::Err, tok.into_span())
                                }
                            });
                        }
                    }
                }

                cursor.reporter().report_sync(spanned_error!(
                    open_paren.span().clone(),
                    "unmatched opening parenthesis"
                ));

                Ok(Spanned::new(Type::Err, open_paren.into_span()))
            }
            Token::Ident(_) => {
                cursor.step_back();

                let mut path_inner = Vec::new();
                let mut last_ident: Option<Spanned<Ident>> = None;

                while let Some(tok) = cursor.peek().cloned() {
                    match tok.inner() {
                        Token::Ident(_) => {
                            if last_ident.is_some() {
                                cursor.reporter().report_sync(spanned_error!(
                                    tok.span().clone(),
                                    "expected seperator, found duplicate ident"
                                ));

                                return Ok(Spanned::new(Type::Err, tok.span().clone()));
                            }

                            last_ident = Some(match cursor.parse() {
                                Ok(ident) => ident,
                                Err(err) => {
                                    cursor.reporter().report_sync(err);
                                    return Ok(Spanned::new(Type::Err, tok.into_span()));
                                }
                            });
                        }
                        Token::Punctuation(Punctuation::DoubleColon) => match last_ident.take() {
                            Some(l) => path_inner.push((
                                l,
                                match cursor.parse() {
                                    Ok(field) => field,
                                    Err(err) => {
                                        cursor.reporter().report_sync(err);
                                        return Ok(Spanned::new(Type::Err, tok.into_span()));
                                    }
                                },
                            )),
                            None => {
                                cursor.reporter().report_sync(spanned_error!(
                                    tok.span().clone(),
                                    "unexpected duplicate seperator"
                                ));

                                return Ok(Spanned::new(Type::Err, tok.into_span()));
                            }
                        },
                        _ => break,
                    }
                }

                let path = Punctuated::new(path_inner, last_ident);

                let (composite, span) = if cursor.check(&Token::Punctuation(Punctuation::Lt)) {
                    let open: Spanned<Token![<]> = cursor.parse()?;
                    let mut types: Vec<Spanned<Type>> = Vec::new();

                    // Set comma to `true` to account for the initial missing comma
                    let mut comma = true;

                    while let Some(tok) = cursor.peek() {
                        let span = tok.span().clone();
                        match tok.inner() {
                            Token::Punctuation(Punctuation::Gt) => {
                                cursor.step();
                                return Ok(Spanned::new(
                                    Type::Composite {
                                        ident: path.into(),
                                        generics: types,
                                    },
                                    open.span().to(&span),
                                ));
                            }
                            Token::Punctuation(Punctuation::Comma) => {
                                cursor.step();
                                comma = true;
                            }
                            _ => {
                                if !comma {
                                    cursor.reporter().report_sync(spanned_error!(
                                        span.clone(),
                                        "expected comma between types, found {}",
                                        tok.inner().description()
                                    ));

                                    types.push(Spanned::new(Type::Err, span.clone()));
                                }
                                comma = false;

                                types.push(match cursor.parse() {
                                    Ok(ty) => ty,
                                    Err(err) => {
                                        cursor.reporter().report_sync(err);
                                        Spanned::new(Type::Err, span)
                                    }
                                });
                            }
                        }
                    }

                    let open_span = open.into_span();
                    cursor
                        .reporter()
                        .report_sync(spanned_error!(open_span.clone(), "unmatched opening arrow"));

                    (Type::Err, open_span)
                } else {
                    let path_span = span.to(path.last().unwrap().span());
                    let composite = Type::Composite {
                        ident: path.into(),
                        generics: Vec::new(),
                    };

                    (composite, path_span)
                };

                Ok(Spanned::new(composite, span))
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

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub ident: Spanned<Ident>,
    pub generics: Vec<Spanned<Generic>>,
    pub implements: Punctuated<Path, Token![,]>,
    pub fields: Punctuated<FieldDef, Comma>,
    pub methods: Vec<Spanned<Method>>,
}

impl Parsable for Spanned<Struct> {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        let open: Spanned<Token![struct]> = cursor.parse()?;
        let ident: Spanned<Ident> = cursor.parse()?;

        let generics = match parse_generics(cursor) {
            Ok(generics) => generics,
            Err(_) => {
                seek!(
                    cursor,
                    Token::Punctuation(Punctuation::Semicolon)
                        | Token::Keyword(Keyword::Impl)
                        | Token::Delimeter(Delimeter::OpenBrace)
                );
                Vec::new()
            }
        };

        let implements = if cursor.check(&Token::Keyword(Keyword::Impl)) {
            cursor.step();

            punctuated!(
                cursor,
                !Token::Delimeter(Delimeter::OpenBrace)
                    | Token::Punctuation(Punctuation::Semicolon),
                Token::Punctuation(Punctuation::Comma)
            )?
        } else {
            Punctuated::empty()
        };

        let (fields, methods, close) = if cursor.check(&Token::Delimeter(Delimeter::OpenBrace)) {
            let _: OpenBrace = cursor.parse()?;

            let mut fields_inner = Vec::new();
            let mut fields_last: Option<FieldDef> = None;
            let mut methods = Vec::new();

            while !cursor.at_end() {
                if cursor.check(&Token::Delimeter(Delimeter::CloseBrace)) {
                    break;
                } else if cursor.check(&Token::Keyword(Keyword::Fn))
                    || (cursor.check(&Token::Keyword(Keyword::Pub))
                        && cursor.check2(&Token::Keyword(Keyword::Fn)))
                {
                    methods.push(cursor.parse()?);
                } else {
                    let field: FieldDef = cursor.parse()?;
                    if cursor.check(&Token::Delimeter(Delimeter::CloseBrace)) {
                        fields_last = Some(field);
                    } else {
                        fields_inner.push((field, cursor.parse()?))
                    }
                }
            }

            let close = cursor.parse::<Spanned<CloseBrace>>()?.into_span();

            (Punctuated::new(fields_inner, fields_last), methods, close)
        } else {
            let close = cursor.parse::<Spanned<Token![;]>>()?.into_span();
            (Punctuated::empty(), Vec::new(), close)
        };

        let struct_span = open.span().to(&close);
        Ok(Spanned::new(
            Struct {
                ident,
                generics,
                implements,
                fields,
                methods,
            },
            struct_span,
        ))
    }

    fn description(&self) -> &'static str {
        "struct definition"
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Union {
    pub ident: Spanned<Ident>,
    pub implements: Punctuated<Path, Token![,]>,
    pub fields: Punctuated<FieldDef, Comma>,
}

impl Parsable for Spanned<Union> {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        let open: Spanned<Token![union]> = cursor.parse()?;
        let ident: Spanned<Ident> = cursor.parse()?;

        let implements = if cursor.check(&Token::Keyword(Keyword::Impl)) {
            cursor.step();

            punctuated!(
                cursor,
                !Token::Delimeter(Delimeter::OpenBrace)
                    | Token::Punctuation(Punctuation::Semicolon),
                Token::Punctuation(Punctuation::Comma)
            )?
        } else {
            Punctuated::empty()
        };

        let (fields, close) = if cursor.check(&Token::Delimeter(Delimeter::OpenBrace)) {
            let _: OpenBrace = cursor.parse()?;

            let fields = punctuated!(
                cursor,
                !Token::Delimeter(Delimeter::CloseBrace),
                Token::Punctuation(Punctuation::Comma)
            )?;

            let close = cursor.parse::<Spanned<CloseBrace>>()?.into_span();

            (fields, close)
        } else {
            let close = cursor.parse::<Spanned<Token![;]>>()?.into_span();
            (Punctuated::empty(), close)
        };

        let union_span = open.span().to(&close);
        Ok(Spanned::new(
            Union {
                ident,
                implements,
                fields,
            },
            union_span,
        ))
    }

    fn description(&self) -> &'static str {
        "union definition"
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Public,
    Private,
}

impl Parsable for Visibility {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        if cursor.check(&Token::Keyword(Keyword::Pub)) {
            cursor.step();
            Ok(Visibility::Public)
        } else {
            Ok(Visibility::Private)
        }
    }

    fn description(&self) -> &'static str {
        "visibility modifier"
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FieldDef {
    pub vis: Visibility,
    pub ident: Spanned<Ident>,
    pub ty: Spanned<Type>,
}

impl Parsable for FieldDef {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        let vis = cursor.parse()?;
        let ident: Spanned<Ident> = cursor.parse()?;
        let ty = if cursor.check(&Token::Punctuation(Punctuation::Colon)) {
            cursor.step();
            cursor.parse()?
        } else {
            cursor.reporter().report_sync(spanned_error!(
                ident.span().clone(),
                "missing type for field"
            ));
            Spanned::new(Type::Err, ident.span().clone())
        };

        Ok(FieldDef { ident, vis, ty })
    }

    fn description(&self) -> &'static str {
        "field definition"
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
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

#[derive(Debug, Clone, PartialEq)]
pub struct Enum {
    pub ident: Spanned<Ident>,
    pub generics: Vec<Spanned<Generic>>,
    pub implements: Punctuated<Path, Token![,]>,
    pub variants: Punctuated<Spanned<Variant>, Comma>,
    pub methods: Vec<Spanned<Method>>,
}

impl Parsable for Spanned<Enum> {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        let open: Spanned<Token![enum]> = cursor.parse()?;
        let ident = cursor.parse()?;

        let generics = match parse_generics(cursor) {
            Ok(generics) => generics,
            Err(_) => {
                seek!(
                    cursor,
                    Token::Punctuation(Punctuation::Semicolon)
                        | Token::Keyword(Keyword::Impl)
                        | Token::Delimeter(Delimeter::OpenBrace)
                );
                Vec::new()
            }
        };

        let implements = if cursor.check(&Token::Keyword(Keyword::Impl)) {
            cursor.step();

            punctuated!(
                cursor,
                !Token::Delimeter(Delimeter::OpenBrace)
                    | Token::Punctuation(Punctuation::Semicolon),
                Token::Punctuation(Punctuation::Comma)
            )?
        } else {
            Punctuated::empty()
        };

        let _: OpenBrace = cursor.parse()?;
        let variants = match punctuated!(
            cursor,
            !Token::Delimeter(Delimeter::CloseBrace)
                | Token::Keyword(Keyword::Pub)
                | Token::Keyword(Keyword::Fn),
            Token::Punctuation(Punctuation::Comma)
        ) {
            Ok(vars) => vars,
            Err(err) => {
                cursor.reporter().report_sync(err);
                seek!(
                    cursor,
                    Token::Keyword(Keyword::Pub)
                        | Token::Keyword(Keyword::Fn)
                        | Token::Delimeter(Delimeter::CloseBrace)
                );
                Punctuated::new(Vec::new(), None)
            }
        };

        let mut methods = Vec::new();

        while !cursor.at_end() {
            if cursor.check(&Token::Delimeter(Delimeter::CloseBrace)) {
                break;
            }

            match cursor.parse::<Spanned<Method>>() {
                Ok(method) => methods.push(method),
                Err(err) => {
                    cursor.reporter().report_sync(err);
                    seek!(cursor, Token::Delimeter(Delimeter::CloseBrace));
                    break;
                }
            }
        }

        let close: Spanned<CloseBrace> = cursor.parse()?;

        Ok(Spanned::new(
            Enum {
                ident,
                generics,
                implements,
                variants,
                methods,
            },
            open.span().to(close.span()),
        ))
    }

    fn description(&self) -> &'static str {
        "enum definition"
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variant {
    pub ident: Spanned<Ident>,
    pub ty: VariantType,
}

impl Parsable for Spanned<Variant> {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        let ident: Spanned<Ident> = cursor.parse()?;
        let ty: VariantType = cursor.parse()?;

        let variant_span = match ty {
            VariantType::Void | VariantType::Err => ident.span().clone(),
            VariantType::Tuple(ref types) => ident.span().to(types.span()),
            VariantType::Struct(ref fields) => ident.span().to(fields.span()),
        };

        Ok(Spanned::new(Variant { ident, ty }, variant_span))
    }

    fn description(&self) -> &'static str {
        "enum variant"
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum VariantType {
    Void,
    Tuple(Spanned<Punctuated<Spanned<Type>, Comma>>),
    Struct(Spanned<Punctuated<FieldDef, Comma>>),
    Err,
}

impl Parsable for VariantType {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        match cursor.peek().map(Spanned::inner) {
            Some(Token::Delimeter(Delimeter::OpenParen)) => {
                let open: Spanned<Token!["("]> = cursor.parse()?;

                let mut types_inner = Vec::new();
                let mut last_type = None;

                while let Some(tok) = cursor.peek().cloned() {
                    match tok.inner() {
                        Token::Punctuation(Punctuation::Comma) => match last_type.take() {
                            Some(l) => types_inner.push((
                                l,
                                match cursor.parse() {
                                    Ok(field) => field,
                                    Err(err) => {
                                        cursor.reporter().report_sync(err);
                                        return Ok(VariantType::Err);
                                    }
                                },
                            )),
                            None => {
                                cursor.reporter().report_sync(spanned_error!(
                                    tok.span().clone(),
                                    "unexpected duplicate seperator"
                                ));

                                return Ok(VariantType::Err);
                            }
                        },
                        Token::Delimeter(Delimeter::CloseParen) => break,
                        _ => {
                            if last_type.is_some() {
                                cursor.reporter().report_sync(spanned_error!(
                                    tok.span().clone(),
                                    "expected seperator, found duplicate ident"
                                ));

                                return Ok(VariantType::Err);
                            }

                            last_type = Some(match cursor.parse() {
                                Ok(ident) => ident,
                                Err(err) => {
                                    cursor.reporter().report_sync(err);
                                    return Ok(VariantType::Err);
                                }
                            });
                        }
                    }
                }

                let types = Punctuated::new(types_inner, last_type);

                let close: Spanned<CloseParen> = cursor.parse()?;

                Ok(VariantType::Tuple(Spanned::new(
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
pub struct Static {
    pub vis: Visibility,
    pub ident: Spanned<Ident>,
    pub ty: Spanned<Type>,
    pub value: Spanned<Expr>,
}

impl Parsable for Spanned<Static> {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        let open: Spanned<Token![static]> = cursor.parse()?;
        let vis = cursor.parse()?;
        let ident = cursor.parse()?;

        match cursor.parse::<Token![:]>() {
            Ok(_) => {}
            Err(err) => {
                cursor.step_back();
                cursor.reporter().report_sync(err);
            }
        }

        let ty: Spanned<Type> = match cursor.parse() {
            Ok(ty) => ty,
            Err(err) => {
                cursor.step_back();
                cursor.reporter().report_sync(err);
                seek!(cursor, Token::Punctuation(Punctuation::Eq));
                Spanned::new(Type::Err, cursor.peek().unwrap().span().clone())
            }
        };

        let _: Token![=] = cursor.parse()?;

        let value: Spanned<Expr> = cursor.parse()?;

        cursor.expect_semicolon();

        let static_span = open.span().to(value.span());
        Ok(Spanned::new(
            Static {
                vis,
                ident,
                ty,
                value,
            },
            static_span,
        ))
    }

    fn description(&self) -> &'static str {
        "static"
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Use {
    reference: Path,
}

impl Parsable for Spanned<Use> {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        let open: Spanned<Token![use]> = cursor.parse()?;
        let reference = cursor.parse()?;
        let close: Spanned<Token![;]> = cursor.parse()?;

        Ok(Spanned::new(
            Use { reference },
            open.span().to(close.span()),
        ))
    }

    fn description(&self) -> &'static str {
        "use statement"
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Immediate(i64),
    Boolean(bool),
    Str(AsciiStr),
    Reference(Path),
    SelfVar,
    Call(Box<Spanned<Expr>>, Punctuated<Spanned<Expr>, Token![,]>),
    NamedConstructor {
        ident: Path,
        fields: Punctuated<Spanned<Field>, Token![,]>,
    },
    Tuple(Vec<Spanned<Expr>>),
    Array(Punctuated<Spanned<Expr>, Token![,]>),
    Dot(Box<(Spanned<Expr>, Spanned<Ident>)>),
    BinaryOp(Box<BinOp>),
    UnaryOp(Spanned<UnaryOp>, Box<Spanned<Expr>>),
    Cast(Spanned<Type>, Box<Spanned<Expr>>),
    Err,
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
    pub fn contains_errors(&self) -> bool {
        match self {
            Expr::Immediate(_) => false,
            Expr::Boolean(_) => false,
            Expr::Reference(_) => false,
            Expr::Str(_) => false,
            Expr::SelfVar => false,
            Expr::Cast(_, expr) => expr.contains_errors(),
            Expr::Call(expr, params) => {
                expr.contains_errors() || params.values().any(|param| param.contains_errors())
            }
            Expr::NamedConstructor { ident, fields } => {
                fields.values().any(|field| field.value.contains_errors())
            }
            Expr::Tuple(exprs) => exprs.iter().any(|expr| expr.contains_errors()),
            Expr::Array(exprs) => exprs.values().any(|expr| expr.contains_errors()),
            Expr::Dot(dot) => dot.as_ref().0.contains_errors(),
            Expr::BinaryOp(op) => op.left.contains_errors() || op.right.contains_errors(),
            Expr::UnaryOp(_, expr) => expr.contains_errors(),
            Expr::Err => true,
        }
    }

    fn parse_tuple(cursor: &mut Cursor) -> Spanned<Self> {
        let mut a = Expr::parse_assignment(cursor);

        while let Some(tok) = cursor.peek().cloned() {
            match tok.inner() {
                Token::Punctuation(Punctuation::Comma) => {
                    cursor.step();
                    let b = Expr::parse_assignment(cursor);
                    if let Expr::Tuple(ref mut components) = a.inner_mut() {
                        components.push(b);
                    } else {
                        let mut components = Vec::new();
                        let span = a.span().to(b.span());
                        components.push(a);
                        components.push(b);
                        a = Spanned::new(Expr::Tuple(components), span);
                    }
                }
                _ => return a,
            }
        }

        return a;
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
        while let Some(tok) = cursor.peek().cloned() {
            match tok.inner() {
                Token::Punctuation(Punctuation::Dot) => {
                    cursor.step();
                    let b: Spanned<Ident> = match cursor.parse() {
                        Ok(b) => b,
                        Err(err) => {
                            cursor.reporter().report_sync(err);
                            return Spanned::new(Expr::Err, tok.into_span());
                        }
                    };

                    let expr_span = a.span().to(b.span());

                    a = Spanned::new(Expr::Dot(Box::new((a, b))), expr_span);
                }
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
                                            cursor.reporter().report_sync(err);
                                            return Spanned::new(Expr::Err, tok.span().clone());
                                        }
                                    },
                                )),
                                None => {
                                    cursor.reporter().report_sync(spanned_error!(
                                        tok.span().clone(),
                                        "unexpected duplicate seperator"
                                    ));

                                    return Spanned::new(Expr::Err, tok.span().clone());
                                }
                            },
                            _ => {
                                last_param = Some(match cursor.parse() {
                                    Ok(field) => field,
                                    Err(err) => {
                                        cursor.reporter().report_sync(err);
                                        return Spanned::new(Expr::Err, tok.span().clone());
                                    }
                                })
                            }
                        }
                    }

                    let params = Punctuated::new(params_inner, last_param);

                    let close: Spanned<CloseParen> = match cursor.parse() {
                        Ok(close) => close,
                        Err(err) => {
                            cursor.reporter().report_sync(err);
                            return Spanned::new(Expr::Err, tok.span().clone());
                        }
                    };

                    let call_span = a.span().to(close.span());
                    a = Spanned::new(Expr::Call(Box::new(a), params), call_span);
                }
                Token::Delimeter(Delimeter::OpenBracket) => {
                    cursor.step();
                    let expr: Spanned<Expr> = match cursor.parse() {
                        Ok(expr) => expr,
                        Err(err) => {
                            cursor.reporter().report_sync(err);
                            Spanned::new(Expr::Err, tok.span().clone())
                        }
                    };

                    if cursor.check(&Token::Delimeter(Delimeter::CloseBracket)) {
                        let index_span = a.span().to(cursor.next().unwrap().span());
                        a = Spanned::new(
                            Expr::BinaryOp(BinOp::boxed(
                                a,
                                Spanned::new(BinaryOp::Index, tok.into_span()),
                                expr,
                            )),
                            index_span,
                        );
                    } else {
                        let index_span = a.span().to(expr.span());
                        a = Spanned::new(
                            Expr::BinaryOp(BinOp::boxed(
                                a,
                                Spanned::new(BinaryOp::Index, tok.into_span()),
                                Spanned::new(Expr::Err, index_span.clone()),
                            )),
                            index_span,
                        )
                    }
                }
                _ => return a,
            }
        }

        return a;
    }

    fn parse_factor(cursor: &mut Cursor) -> Spanned<Self> {
        let (tok, span) = match cursor.next() {
            Some(next) => next.deconstruct(),
            None => {
                cursor.reporter().report_sync(spanned_error!(
                    cursor.eof_span(),
                    "expected expression, found `EOF`"
                ));

                return Spanned::new(Expr::Err, cursor.eof_span());
            }
        };

        match tok {
            Token::Immediate(i) => Spanned::new(Expr::Immediate(i), span),
            Token::Boolean(b) => Spanned::new(Expr::Boolean(b), span),
            Token::String(str) => Spanned::new(Expr::Str(str), span),
            Token::Keyword(Keyword::LowerSelf) => Spanned::new(Expr::SelfVar, span),
            Token::Delimeter(Delimeter::OpenParen) => {
                let a = Expr::parse_tuple(cursor);

                match cursor.next().map(Spanned::into_inner) {
                    Some(Token::Delimeter(Delimeter::CloseParen)) => a,
                    _ => {
                        cursor.reporter().report_sync(spanned_error!(
                            span.clone(),
                            "unmatched opening parenthesis"
                        ));

                        Spanned::new(Expr::Err, span)
                    }
                }
            }
            Token::Delimeter(Delimeter::OpenBracket) => {
                let mut contents_inner = Vec::new();
                let mut last_expr = None;

                while let Some(tok) = cursor.peek().cloned() {
                    match tok.inner() {
                        Token::Delimeter(Delimeter::CloseBracket) => break,
                        Token::Punctuation(Punctuation::Comma) => match last_expr.take() {
                            Some(l) => contents_inner.push((
                                l,
                                match cursor.parse() {
                                    Ok(field) => field,
                                    Err(err) => {
                                        cursor.reporter().report_sync(err);
                                        return Spanned::new(Expr::Err, tok.span().clone());
                                    }
                                },
                            )),
                            None => {
                                cursor.reporter().report_sync(spanned_error!(
                                    tok.span().clone(),
                                    "unexpected duplicate seperator"
                                ));

                                return Spanned::new(Expr::Err, tok.span().clone());
                            }
                        },
                        _ => {
                            last_expr = Some(match cursor.parse() {
                                Ok(field) => field,
                                Err(err) => {
                                    cursor.reporter().report_sync(err);
                                    return Spanned::new(Expr::Err, tok.span().clone());
                                }
                            })
                        }
                    }
                }

                let contents = Punctuated::new(contents_inner, last_expr);

                let close: Spanned<Token!["]"]> = match cursor.parse() {
                    Ok(close) => close,
                    Err(err) => {
                        cursor.reporter().report_sync(err);
                        return Spanned::new(Expr::Err, span.clone());
                    }
                };

                let array_span = span.to(close.span());
                Spanned::new(Expr::Array(contents), array_span)
            }
            Token::Punctuation(Punctuation::Not) => {
                let expr = Expr::parse_factor(cursor);
                let not_span = span.to(expr.span());

                Spanned::new(
                    Expr::UnaryOp(Spanned::new(UnaryOp::Not, span), Box::new(expr)),
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
                            Err(err) => {
                                cursor.reporter().report_sync(err);
                                return Spanned::new(Expr::Err, span);
                            }
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
                let mut last_ident: Option<Spanned<Ident>> = None;

                while let Some(tok) = cursor.peek().cloned() {
                    match tok.inner() {
                        Token::Ident(_) => {
                            if last_ident.is_some() {
                                cursor.reporter().report_sync(spanned_error!(
                                    tok.span().clone(),
                                    "expected seperator, found duplicate ident"
                                ));

                                return Spanned::new(Expr::Err, tok.span().clone());
                            }

                            last_ident = Some(match cursor.parse() {
                                Ok(ident) => ident,
                                Err(err) => {
                                    cursor.reporter().report_sync(err);
                                    return Spanned::new(Expr::Err, tok.into_span());
                                }
                            });
                        }
                        Token::Punctuation(Punctuation::DoubleColon) => match last_ident.take() {
                            Some(l) => path_inner.push((
                                l,
                                match cursor.parse() {
                                    Ok(field) => field,
                                    Err(err) => {
                                        cursor.reporter().report_sync(err);
                                        return Spanned::new(Expr::Err, tok.into_span());
                                    }
                                },
                            )),
                            None => {
                                cursor.reporter().report_sync(spanned_error!(
                                    tok.span().clone(),
                                    "unexpected duplicate seperator"
                                ));

                                return Spanned::new(Expr::Err, tok.into_span());
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
                                            Ok(comma) => comma,
                                            Err(err) => {
                                                cursor.reporter().report_sync(err);
                                                return Spanned::new(Expr::Err, span);
                                            }
                                        },
                                    )),
                                    None => {
                                        cursor.reporter().report_sync(spanned_error!(
                                            tok.span().clone(),
                                            "unexpected duplicate seperator"
                                        ));

                                        return Spanned::new(Expr::Err, tok.span().clone());
                                    }
                                },
                                _ => {
                                    let description = tok.inner().description();
                                    let next_span = tok.span().clone();
                                    last_field = Some(match cursor.parse() {
                                        Ok(field) => field,
                                        Err(err) => {
                                            cursor.reporter().report_sync(err);
                                            return Spanned::new(Expr::Err, span);
                                        }
                                    })
                                }
                            }
                        }

                        let fields = Punctuated::new(fields_inner, last_field);
                        let close: Spanned<CloseBrace> = match cursor.parse() {
                            Ok(field) => field,
                            Err(err) => {
                                cursor.reporter().report_sync(err);
                                return Spanned::new(Expr::Err, span);
                            }
                        };

                        Spanned::new(
                            Expr::NamedConstructor {
                                ident: path.into(),
                                fields,
                            },
                            span.to(close.span()),
                        )
                    }
                    _ => {
                        let path_span = span.to(path.last().unwrap().span());
                        Spanned::new(Expr::Reference(path.into()), path_span)
                    }
                }
            }
            Token::Punctuation(Punctuation::Semicolon) => {
                cursor.step_back();

                cursor.reporter().report_sync(spanned_error!(
                    span.clone(),
                    "expected expression, found {}",
                    tok.description()
                ));

                Spanned::new(Expr::Err, span)
            }
            _ => {
                cursor.reporter().report_sync(spanned_error!(
                    span.clone(),
                    "expected expression, found {}",
                    tok.description()
                ));

                Spanned::new(Expr::Err, span)
            }
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
    Index,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum UnaryOp {
    Negative,
    Not,
    Deref,
    Addr(Mutability),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expr(Expr),
    Block(Vec<Spanned<Statement>>),
    If {
        condition: Spanned<Expr>,
        content: Box<Spanned<Statement>>,
        else_block: Option<Box<Spanned<Statement>>>,
    },
    Match {
        condition: Spanned<Expr>,
        arms: Vec<Spanned<MatchArm>>,
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
    pub fn contains_errors(&self) -> bool {
        match self {
            Statement::Break | Statement::Continue => false,
            Statement::Expr(expr) => expr.contains_errors(),
            Statement::If {
                condition,
                content,
                else_block,
            } => {
                condition.contains_errors()
                    || content.contains_errors()
                    || else_block
                        .as_ref()
                        .map(|block| block.contains_errors())
                        .unwrap_or(false)
            }
            Statement::Match { condition, arms } => {
                condition.contains_errors() || arms.iter().any(|arm| arm.exe.contains_errors())
            }
            Statement::Loop(content) => content.contains_errors(),
            Statement::For { contents, header } => {
                header.init.contains_errors()
                    || header.check.contains_errors()
                    || header.post.contains_errors()
                    || contents.contains_errors()
            }
            Statement::While { check, contents } => {
                check.contains_errors() || contents.contains_errors()
            }
            Statement::Let { assignment, ty, .. } => {
                assignment.contains_errors() || ty.contains_errors()
            }
            Statement::Return(expr) => expr.contains_errors(),
            Statement::Block(content) => content.iter().any(|s| s.contains_errors()),
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
            Token::Keyword(Keyword::Match) => {
                let _: Token!["("] = match cursor.parse() {
                    Ok(op) => op,
                    Err(err) => {
                        cursor.step_back();
                        cursor.reporter().report_sync(err);

                        // Just construct it since we have to return something
                        Token!["("]
                    }
                };

                let condition: Spanned<Expr> = cursor.parse()?;
                if condition.contains_errors() {
                    cursor.seek(&Token::Delimeter(Delimeter::CloseParen));
                }

                let _: Token![")"] = match cursor.parse() {
                    Ok(op) => op,
                    Err(err) => {
                        cursor.step_back();
                        cursor.reporter().report_sync(err);

                        // Just construct it since we have to return something
                        Token![")"]
                    }
                };

                let _: Token!["{"] = match cursor.parse() {
                    Ok(ob) => ob,
                    Err(err) => {
                        cursor.step_back();
                        return Err(err);
                    }
                };

                let mut arms = Vec::new();

                while !cursor.at_end() {
                    if cursor.check(&Token::Delimeter(Delimeter::CloseBrace)) {
                        break;
                    }

                    let arm = match cursor.parse() {
                        Ok(arm) => arm,
                        Err(err) => {
                            cursor.seek(&Token::Delimeter(Delimeter::CloseBrace));
                            cursor.reporter().report_sync(err);
                            break;
                        }
                    };
                    arms.push(arm);
                }

                let close: Spanned<Token!["}"]> = cursor.parse()?;

                let match_span = span.to(close.span());
                Ok(Spanned::new(
                    Statement::Match { condition, arms },
                    match_span,
                ))
            }
            Token::Keyword(Keyword::Loop) => {
                let contents: Spanned<Statement> = cursor.parse()?;
                let loop_span = span.to(contents.span());
                Ok(Spanned::new(Statement::Loop(Box::new(contents)), loop_span))
            }
            Token::Keyword(Keyword::For) => {
                let _: OpenParen = cursor.parse()?;

                let init: Spanned<Statement> = cursor.parse()?;
                if init.contains_errors() {
                    cursor.seek(&Token::Punctuation(Punctuation::Semicolon));
                }

                let check: Spanned<Statement> = cursor.parse()?;
                if check.contains_errors() {
                    cursor.seek(&Token::Punctuation(Punctuation::Semicolon));
                }

                let post: Spanned<Statement> = cursor.parse()?;
                if post.contains_errors() {
                    seek!(
                        cursor,
                        Token::Delimeter(Delimeter::CloseParen)
                            | Token::Punctuation(Punctuation::Semicolon)
                    );
                }

                let header = ForHeader { init, check, post };

                let _: CloseParen = cursor.parse()?;

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
            Token::Keyword(Keyword::Break) => {
                cursor.expect_semicolon();
                Ok(Spanned::new(Statement::Break, span))
            }
            Token::Keyword(Keyword::Continue) => {
                cursor.expect_semicolon();
                Ok(Spanned::new(Statement::Continue, span))
            }
            Token::Keyword(Keyword::Return) => {
                let value: Spanned<Expr> = cursor.parse()?;
                let return_span = span.to(value.span());
                cursor.expect_semicolon();

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
                            cursor.reporter().report_sync(err);
                            Spanned::new(Type::Err, ident.span().clone())
                        }
                    }
                } else {
                    cursor.reporter().report_sync(spanned_error!(
                        ident.span().clone(),
                        "missing type for variable declaration"
                    ));

                    Spanned::new(Type::Err, ident.span().clone())
                };

                let _: Eq = cursor.parse()?;
                let assignment: Spanned<Expr> = cursor.parse()?;

                let let_span = span.to(assignment.span());
                cursor.expect_semicolon();

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
                    } else if cursor.check(&Token::Punctuation(Punctuation::Semicolon)) {
                        cursor.step();
                        continue;
                    }

                    match cursor.parse() {
                        Ok(next_statement) => {
                            statements.push(next_statement);
                        }
                        Err(err) => {
                            cursor.reporter().report_sync(err);
                            seek!(
                                cursor,
                                Token::Punctuation(Punctuation::Semicolon)
                                    | Token::Delimeter(Delimeter::CloseBrace)
                            );
                        }
                    }
                }

                Err(spanned_error!(span, "unmatched opening brace"))
            }
            _ => {
                cursor.step_back();
                let expr: Spanned<Expr> = cursor.parse()?;
                cursor.expect_semicolon();

                Ok(expr.map(Statement::Expr))
            }
        }
    }

    fn description(&self) -> &'static str {
        "statement"
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForHeader {
    init: Spanned<Statement>,
    check: Spanned<Statement>,
    post: Spanned<Statement>,
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

#[derive(Debug, Clone, PartialEq)]
pub struct FnDefinition {
    pub ident: Spanned<Ident>,
    pub generics: Vec<Spanned<Generic>>,
    pub parameters: Parenthesized<Punctuated<FnParam, Comma>>,
    pub return_type: Spanned<Type>,
    pub body: Spanned<Statement>,
}

impl Parsable for Spanned<FnDefinition> {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        let open: Spanned<Token![fn]> = cursor.parse()?;
        let ident: Spanned<Ident> = cursor.parse()?;

        let generics = match parse_generics(cursor) {
            Ok(generics) => generics,
            Err(_) => {
                seek!(
                    cursor,
                    Token::Punctuation(Punctuation::Semicolon)
                        | Token::Keyword(Keyword::Impl)
                        | Token::Delimeter(Delimeter::OpenBrace)
                );
                Vec::new()
            }
        };

        let parameters: Parenthesized<Punctuated<FnParam, Comma>> = cursor.parse()?;

        let return_type = if cursor.check(&Token::Punctuation(Punctuation::Colon)) {
            cursor.step();
            cursor.parse()?
        } else {
            cursor
                .reporter()
                .report_sync(spanned_error!(ident.span().clone(), "missing return type"));

            Spanned::new(Type::Err, ident.span().clone())
        };

        let body: Spanned<Statement> = cursor.parse()?;

        let def_span = open.span().to(body.span());

        Ok(Spanned::new(
            FnDefinition {
                ident,
                generics,
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

#[derive(Debug, Clone, PartialEq)]
pub struct Method {
    pub vis: Visibility,
    pub ident: Spanned<Ident>,
    pub generics: Vec<Spanned<Generic>>,
    pub parameters: Parenthesized<Punctuated<Spanned<MethodParam>, Token![,]>>,
    pub return_type: Spanned<Type>,
    pub implements: Option<Path>,
    pub body: Spanned<Statement>,
}

impl Parsable for Spanned<Method> {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        let vis: Visibility = cursor.parse()?;
        let open: Spanned<Token![fn]> = cursor.parse()?;
        let ident: Spanned<Ident> = cursor.parse()?;

        let generics = match parse_generics(cursor) {
            Ok(generics) => generics,
            Err(_) => {
                seek!(
                    cursor,
                    Token::Punctuation(Punctuation::Semicolon)
                        | Token::Keyword(Keyword::Impl)
                        | Token::Delimeter(Delimeter::OpenBrace)
                );
                Vec::new()
            }
        };

        let parameters = cursor.parse()?;

        let _: Token![:] = cursor.parse()?;
        let return_type = cursor.parse()?;

        let implements = if cursor.check(&Token::Keyword(Keyword::Impl)) {
            cursor.step();
            Some(cursor.parse()?)
        } else {
            None
        };

        let body: Spanned<Statement> = cursor.parse()?;

        let method_span = open.span().to(body.span());
        Ok(Spanned::new(
            Method {
                vis,
                ident,
                generics,
                parameters,
                return_type,
                implements,
                body,
            },
            method_span,
        ))
    }

    fn description(&self) -> &'static str {
        "method"
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum MethodParam {
    SelfParam {
        mutability: Mutability,
        ty: SelfParam,
    },
    FnParam(FnParam),
}

impl Parsable for Spanned<MethodParam> {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        if cursor.check(&Token::Keyword(Keyword::LowerSelf)) {
            let s: Spanned<Token![self]> = cursor.parse()?;
            Ok(Spanned::new(
                MethodParam::SelfParam {
                    mutability: Mutability::Const,
                    ty: SelfParam::Value,
                },
                s.into_span(),
            ))
        } else if cursor.check(&Token::Punctuation(Punctuation::Star)) {
            let open: Spanned<Token![*]> = cursor.parse()?;
            let mutability: Mutability = cursor.parse()?;
            let s: Spanned<Token![self]> = cursor.parse()?;

            let param_span = open.span().to(s.span());
            Ok(Spanned::new(
                MethodParam::SelfParam {
                    mutability,
                    ty: SelfParam::Reference,
                },
                param_span,
            ))
        } else if cursor.check(&Token::Keyword(Keyword::Mut))
            && cursor.check2(&Token::Keyword(Keyword::LowerSelf))
        {
            let open: Spanned<Token![mut]> = cursor.parse()?;
            let s: Spanned<Token![self]> = cursor.parse()?;

            let param_span = open.span().to(s.span());
            Ok(Spanned::new(
                MethodParam::SelfParam {
                    mutability: Mutability::Mut,
                    ty: SelfParam::Value,
                },
                param_span,
            ))
        } else {
            let (param, span) = cursor.parse::<Spanned<FnParam>>()?.deconstruct();
            Ok(Spanned::new(MethodParam::FnParam(param), span))
        }
    }

    fn description(&self) -> &'static str {
        "method parameter"
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SelfParam {
    Value,
    Reference,
}

#[derive(Debug, Clone, PartialEq)]
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
        let path = cursor.parse()?;
        let parameters = cursor.parse()?;

        Ok(Self { path, parameters })
    }

    fn description(&self) -> &'static str {
        "function call"
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Generic {
    ident: Spanned<Ident>,
    requirements: Punctuated<Path, Token![+]>,
}

impl Parsable for Spanned<Generic> {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        let ident: Spanned<Ident> = cursor.parse()?;

        let (requirements, span) = if cursor.check(&Token::Punctuation(Punctuation::Colon)) {
            cursor.step();

            let requirements = match punctuated!(
                cursor,
                !Token::Punctuation(Punctuation::Gt),
                Token::Punctuation(Punctuation::Plus)
            ) {
                Ok(requirements) => requirements,
                Err(err) => {
                    seek!(cursor, Token::Punctuation(Punctuation::Gt));
                    return Err(err);
                }
            };

            let span = requirements
                .last()
                .map(|last: &Path| ident.span().to(&last.span()))
                .unwrap_or_else(|| ident.span().clone());
            (requirements, span)
        } else {
            (Punctuated::empty(), ident.span().clone())
        };

        Ok(Spanned::new(
            Generic {
                ident,
                requirements,
            },
            span,
        ))
    }

    fn description(&self) -> &'static str {
        "generic"
    }
}

fn parse_generics(cursor: &mut Cursor) -> Result<Vec<Spanned<Generic>>, ()> {
    let mut types = Vec::new();

    // Just using parse to check if there is even generics specified
    let open: Spanned<Token![<]> = match cursor.parse() {
        Ok(open) => open,
        Err(_) => {
            cursor.step_back();
            return Ok(types);
        }
    };

    // Set comma to true initially so an error is not raised
    let mut comma = true;

    while let Some(tok) = cursor.peek() {
        match tok.inner() {
            Token::Punctuation(Punctuation::Gt) => {
                cursor.step();
                return Ok(types);
            }
            Token::Punctuation(Punctuation::Comma) => {
                comma = true;
                cursor.step();
            }
            _ => {
                let span = tok.span().clone();
                if !comma {
                    cursor.reporter().report_sync(spanned_error!(
                        span.clone(),
                        "expected comma between types, found {}",
                        tok.inner().description()
                    ));

                    return Err(());
                }
                comma = false;

                types.push(match cursor.parse() {
                    Ok(ty) => ty,
                    Err(err) => {
                        cursor.reporter().report_sync(err);
                        return Err(());
                    }
                });
            }
        }
    }

    let open_span = open.into_span();
    cursor
        .reporter()
        .report_sync(spanned_error!(open_span.clone(), "unmatched opening arrow"));

    Err(())
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pattern: Spanned<Pattern>,
    exe: Spanned<Statement>,
}

impl Parsable for Spanned<MatchArm> {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        let pat: Spanned<Pattern> = match cursor.parse() {
            Ok(pat) => pat,
            Err(err) => {
                seek!(
                    cursor,
                    Token::Punctuation(Punctuation::Semicolon)
                        | Token::Delimeter(Delimeter::CloseBrace)
                );
                return Err(err);
            }
        };

        let _ = match cursor.parse::<Token![:]>() {
            Ok(pat) => Ok(pat),
            Err(err) => {
                cursor.step_back();
                cursor.reporter().report_sync(err);
                Err(())
            }
        };

        let exe: Spanned<Statement> = cursor.parse()?;

        let arm_span = pat.span().to(exe.span());
        Ok(Spanned::new(MatchArm { pattern: pat, exe }, arm_span))
    }

    fn description(&self) -> &'static str {
        "`match` arm"
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Immediate(i64),
    Boolean(bool),
    Str(AsciiStr),
    Bind(Path),
    Tuple(Vec<Spanned<Pattern>>),
    Array(Vec<Spanned<Pattern>>),
    NamedTuple {
        path: Path,
        patterns: Vec<Spanned<Pattern>>,
    },
    Struct {
        path: Path,
        fields: Vec<Spanned<StructPat>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructPat {
    ident: Spanned<Ident>,
    pattern: Spanned<Pattern>,
}

impl Parsable for Spanned<Pattern> {
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic> {
        let (tok, span) = cursor
            .next()
            .ok_or_else(|| spanned_error!(cursor.eof_span(), "expected token, found `EOF`"))?
            .deconstruct();
        match tok {
            Token::Immediate(imm) => Ok(Spanned::new(Pattern::Immediate(imm), span)),
            Token::Boolean(b) => Ok(Spanned::new(Pattern::Boolean(b), span)),
            Token::String(s) => Ok(Spanned::new(Pattern::Str(s), span)),
            Token::Delimeter(Delimeter::OpenParen) => {
                let mut patterns = Vec::new();

                while !cursor.at_end() {
                    if cursor.check(&Token::Delimeter(Delimeter::CloseParen)) {
                        let close: Spanned<Token![")"]> = cursor.parse()?;
                        let tup_span = span.to(close.span());

                        return Ok(Spanned::new(Pattern::Tuple(patterns), tup_span));
                    }

                    let pat = cursor.parse()?;
                    patterns.push(pat);
                    if !cursor.check(&Token::Delimeter(Delimeter::CloseParen)) {
                        let _: Comma = cursor.parse()?;
                    }
                }

                Err(spanned_error!(span, "unmatched opening parenthesis"))
            }
            Token::Delimeter(Delimeter::OpenBracket) => {
                let mut patterns = Vec::new();

                while !cursor.at_end() {
                    if cursor.check(&Token::Delimeter(Delimeter::CloseBracket)) {
                        let close: Spanned<Token!["]"]> = cursor.parse()?;
                        let tup_span = span.to(close.span());

                        return Ok(Spanned::new(Pattern::Array(patterns), tup_span));
                    }

                    let pat = cursor.parse()?;
                    patterns.push(pat);
                    if !cursor.check(&Token::Delimeter(Delimeter::CloseBracket)) {
                        let _: Comma = cursor.parse()?;
                    }
                }

                Err(spanned_error!(span, "unmatched opening bracket"))
            }
            Token::Ident(_) => {
                cursor.step_back();

                let path: Path = cursor.parse()?;

                if let Some(tok) = cursor.peek() {
                    match tok.inner() {
                        Token::Delimeter(Delimeter::OpenParen) => {
                            todo!() // TODO: Implement `NamedTuple`
                        }
                        Token::Delimeter(Delimeter::OpenBrace) => {
                            todo!() // TODO: Implement `Struct`
                        }
                        _ => {}
                    }
                }

                let span = path.span();
                Ok(Spanned::new(Pattern::Bind(path), span))
            }
            _ => {
                cursor.step_back();
                Err(spanned_error!(
                    span,
                    "expected pattern, found {}",
                    tok.description()
                ))
            }
        }
    }

    fn description(&self) -> &'static str {
        "pattern"
    }
}
