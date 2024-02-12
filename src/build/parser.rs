use std::{
    collections::HashMap,
    ops::{Range, RangeInclusive},
    sync::Arc,
};

use async_std::path::PathBuf;
use indexmap::IndexMap;

use crate::{diagnostic::Diagnostic, span::Span};

use super::lexer::{self, Token, TokenStream};

pub fn parse<L>(stream: TokenStream) {}

pub struct Cursor {
    stream: TokenStream,
    position: usize,
}

impl Iterator for Cursor {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let ret = self.stream.get(self.position);
        self.position += 1;
        ret.cloned()
    }
}

pub trait Parsable: Sized {
    /// Parses the item from a stream of raw tokens.
    fn parse(cursor: &mut Cursor) -> Result<Self, Diagnostic>;
    /// Static description of the item.
    /// Used for error messages.
    fn description(&self) -> &'static str;
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
    mutability: Mutability,
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

enum Mutability {
    Const,
    Mut,
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
