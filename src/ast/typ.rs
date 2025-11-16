use super::Node;

#[derive(Debug, Clone)]
pub enum TypKind {
    Id(String),
    Ref(Box<Typ>),
    Slice(Box<Typ>),
    Ptr(Box<Typ>),
    VarArgs,
}
impl std::fmt::Display for TypKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypKind::Id(name) => write!(f, "{}", name),
            TypKind::Ref(typ) => write!(f, "&{}", typ.kind),
            TypKind::Slice(typ) => write!(f, "[{}]", typ.kind),
            TypKind::Ptr(typ) => write!(f, "*{}", typ.kind),
            TypKind::VarArgs => write!(f, "..."),
        }
    }
}

pub type Typ = Node<TypKind>;
