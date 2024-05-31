use std::{collections::HashMap, ops::{Index, IndexMut}};

use indexmap::IndexMap;

use crate::{build::syntax::{ast::{Mutability, Visibility}, lex::Primitive, token::Ident}, diagnostic::Diagnostic, span::Spanned, spanned_error};

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
    pub fn empty() -> Self {
        Self {
            types: Vec::new(),
            functions: Vec::new(),
        }
    }

    pub fn populate(&mut self) {
        todo!("populate with primatives and respective methods")
    }

    pub fn reserve_ty(&mut self) -> TypeId {
        self.types.push(Type::from(TypeInner::Empty));
        
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

impl Default for Database {
    fn default() -> Self {
        let mut s = Self::empty();
        s.populate();
        s
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

pub struct Type {
    inner: TypeInner,
    methods: HashMap<Ident, FnId>,
}

impl Type {
    pub fn add_method(&mut self, ident: Spanned<Ident>, func: FnId) -> Result<(), Diagnostic> {
        let (ident, span) = ident.deconstruct();
        if let Some(_) = self.methods.insert(ident, func) {
            Err(spanned_error!(span, "found duplicate method"))
        } else {
            Ok(())
        }
    }
}

impl From<TypeInner> for Type {
    fn from(value: TypeInner) -> Self {
        Self {
            inner: value,
            methods: HashMap::new(),
        }
    }
}

pub enum TypeInner {
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
}

pub struct Enum {
    ident: Spanned<Ident>,
    varients: IndexMap<Ident, Varient>,
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
