use super::{Node, strings};

#[derive(Clone)]
pub enum TypKind {
    Ident(strings::Id),
    Ref(Box<Typ>),
    Slice(Box<Typ>),
    Ptr(Box<Typ>),
    VarArgs,
}

pub type Typ = Node<TypKind>;
