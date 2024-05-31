use std::collections::HashMap;

use crate::build::symbol_table::SymbolRef;

use super::type_resolution::{FnId, TypeId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LibId(usize);

pub struct LibDatabase {
    libraries: Vec<Lib>,
}

pub struct Lib {
    types: HashMap<SymbolRef, TypeId>,
    functions: HashMap<SymbolRef, FnId>,
}
