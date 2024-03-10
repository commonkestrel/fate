use super::token::Ident;



#[derive(Debug, Clone, PartialEq)]
pub enum ExprTree {
    Literal(i128),
    Reference(Ident),
    Call(),
}


