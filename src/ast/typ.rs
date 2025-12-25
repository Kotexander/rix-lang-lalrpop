use super::Expr;
use super::Ident;
use super::Node;

#[derive(Clone)]
pub enum TypKind {
    Ident(Ident),
    Ref(Box<Typ>),
    Slice(Box<Typ>),
    Ptr(Box<Typ>),
    Array(Box<Typ>, Box<Expr>),
}

pub type Typ = Node<TypKind>;

#[derive(Clone)]
pub enum ArgTypKind {
    Typ(Typ),
    VarArgs,
}
pub type ArgTyp = Node<ArgTypKind>;
