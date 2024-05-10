use std::ops::{Index, IndexMut};

use indexmap::IndexMap;

use crate::{build::syntax::{ast::{Mutability, Visibility}, lex::Primitive, token::Ident}, span::Spanned};

use super::ast::typed;

pub fn resolve() {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FnId(usize);

pub struct Database {
    types: Vec<Type>,
    functions: Vec<Function>,
}

impl Database {
    pub fn new() -> Self {
        Self {
            types: vec![
                Type::Primitive(Primitive::Void),
                Type::Primitive(Primitive::Bool),
                Type::Primitive(Primitive::Str),
                Type::Primitive(Primitive::U8),
                Type::Primitive(Primitive::U16),
                Type::Primitive(Primitive::U24),
                Type::Primitive(Primitive::U32),
                Type::Primitive(Primitive::I8),
                Type::Primitive(Primitive::I16),
                Type::Primitive(Primitive::I24),
            ],
            functions: Vec::new(),
        }
    }

    pub fn reserve_ty(&mut self) -> TypeId {
        self.types.push(Type::Empty);
        
        TypeId(self.types.len() - 1)
    }

    pub fn populate_ty(&mut self, id: TypeId, value: Type) {
        self[id] = value;
    }

    pub fn insert_ty(&mut self, value: Type) -> TypeId {
        self.types.push(value);

        TypeId(self.types.len() - 1)
    }

    pub fn insert_fn(&mut self, func: Function) -> FnId {
        self.functions.push(func);

        FnId(self.functions.len() - 1)
    }
}

impl Index<TypeId> for Database {
    type Output = Type;

    fn index(&self, index: TypeId) -> &Self::Output {
        &self.types[index.0]
    }
}

impl IndexMut<TypeId> for Database {
    fn index_mut(&mut self, index: TypeId) -> &mut Self::Output {
        &mut self.types[index.0]
    }
}

impl Index<FnId> for Database {
    type Output = Function;

    fn index(&self, index: FnId) -> &Self::Output {
        &self.functions[index.0]
    }
}

impl IndexMut<FnId> for Database {
    fn index_mut(&mut self, index: FnId) -> &mut Self::Output {
        &mut self.functions[index.0]
    }
}

pub struct Function {

}

pub enum Type {
    Struct(Struct),
    Enum(Enum),
    Fn(FnSignature),
    Pointer(Ptr),
    Tuple(Vec<TypeId>),
    Array(Array),
    Primitive(Primitive),
    Empty,
}

pub struct Struct {
    ident: Spanned<Ident>,
    fields: IndexMap<Ident, TypeId>,
    methods: IndexMap<Ident, TypeId>,
}

pub struct Enum {
    ident: Spanned<Ident>,
    varients: IndexMap<Ident, Varient>,
    methods: IndexMap<Ident, TypeId>,
}

pub enum Varient {
    Void,
    Tuple(TypeId),
    Struct(IndexMap<Ident, TypeId>),
}

pub struct FnSignature {
    vis: Visibility,
    ident: Spanned<Ident>,
    params: Vec<TypeId>,
    ret: TypeId,
}

pub struct Ptr {
    mutability: Mutability,
    ty: TypeId,
}

pub struct Array {
    ty: TypeId,
    len: u16,
}
