use super::{Expr, Ident, Node, Typ};

#[derive(Clone)]
pub enum InstrKind {
    VarInit {
        name: Ident,
        typ: Option<Typ>,
        expr: Expr,
    },
    VarAssign {
        name: Ident,
        expr: Expr,
    },
    Return(Option<Expr>),
    Expr(Expr),

    While {
        cond: Expr,
        body: Vec<Instr>,
    },
    Break,
    Continue,

    If {
        cond: Expr,
        then: Vec<Instr>,
        elifs: Vec<(Expr, Vec<Instr>)>,
        els: Option<Vec<Instr>>,
    },

    Error,
}

pub type Instr = Node<InstrKind>;
