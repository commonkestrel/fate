use std::{
    collections::HashMap,
    ops::{Range, RangeInclusive},
    sync::Arc,
};

use async_std::path::PathBuf;
use indexmap::IndexMap;

use super::lexer::{self, SingleSpan, TokenStream};

pub fn parse(stream: TokenStream) {}

#[derive(Debug, Clone)]
pub struct Span {
    source_name: Arc<String>,
    source: lexer::Source,
    lines: RangeInclusive<usize>,
    columns: Range<usize>,
}

impl From<SingleSpan> for Span {
    fn from(value: SingleSpan) -> Self {
        Span {
            source_name: value.source_name,
            source: value.source,
            columns: value.columns,
            lines: value.line..=value.line,
        }
    }
}

struct Namespace {
    home: PathBuf,
    constants: HashMap<String, Variable>,
}

struct FnDefinition {
    args: Vec<Variable>,
}

trait Size {
    fn size(&self) -> u16;
}

impl Size for lexer::Primitive {
    fn size(&self) -> u16 {
        use lexer::Primitive as P;
        match self {
            P::Void => 0,
            P::Bool => 1,
            P::U8 => 1,
            P::I8 => 1,
            P::U16 => 2,
            P::I16 => 2,
        }
    }
}

struct Variable {
    name: String,
    ty: Type,
}

enum Mutability {
    Const,
    Mut,
}

enum Type {
    Primitive(lexer::Primitive),
    Pointer {
        mutability: Mutability,
        ty: Box<Type>,
    },
    Array {
        ty: Box<Type>,
        len: u16,
    },
    Tuple(Vec<Type>),
    Struct(Struct),
    Enum(Enum),
}

impl Size for Type {
    fn size(&self) -> u16 {
        match self {
            Type::Primitive(ty) => ty.size(),
            Type::Pointer {
                mutability: _,
                ty: _,
            } => 2,
            Type::Array { ty, len } => ty.size() * len,
            Type::Tuple(types) => types.iter().map(|ty| ty.size()).sum(),
            Type::Struct(st) => st.size(),
            Type::Enum(en) => en.size(),
        }
    }
}

struct Struct {
    fields: IndexMap<String, Variable>,
}

impl Size for Struct {
    fn size(&self) -> u16 {
        self.fields.values().map(|field| field.ty.size()).sum()
    }
}

struct Enum {
    variants: IndexMap<String, Variant>,
}

impl Size for Enum {
    fn size(&self) -> u16 {
        // identifier + data
        self.variants.len().checked_ilog2().unwrap_or(0) as u16
            + self
                .variants
                .values()
                .map(|variant| variant.size())
                .max()
                .unwrap_or(0)
    }
}

enum Variant {
    Void,
    Struct(Struct),
    Tuple(Vec<Type>),
}

impl Size for Variant {
    fn size(&self) -> u16 {
        match self {
            Variant::Void => 0,
            Variant::Struct(st) => st.size(),
            Variant::Tuple(tup) => tup.iter().map(|ty| ty.size()).sum(),
        }
    }
}
