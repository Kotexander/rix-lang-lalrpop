use super::{ArgTyp, Ident, Instr, Node, Typ};

#[derive(Clone)]
pub enum ItemKind {
    Function {
        name: Ident,
        args: Vec<(Ident, ArgTyp)>,
        ret: Option<Typ>,
        body: Option<Vec<Instr>>,
    },
    Struct {
        name: Ident,
        fields: Vec<(Ident, Typ)>,
    },
    Union {
        name: Ident,
        variants: Vec<(Ident, Typ)>,
    },
    // Error,
}

pub type Item = Node<ItemKind>;
