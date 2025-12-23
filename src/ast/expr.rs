use super::{Ident, Node};
use crate::strings;

#[derive(Clone)]
pub enum ExprKind {
    Number(i32),
    String(strings::Id),

    Variable(Ident),
    Call {
        name: Ident,
        args: Vec<Expr>,
    },
    BinOp {
        op: BinOp,
        l: Box<Expr>,
        r: Box<Expr>,
    },
    UniOp {
        op: UniOp,
        expr: Box<Expr>,
    },
    FieldAccess {
        expr: Box<Expr>,
        field: Ident,
    },

    StructInit {
        name: Ident,
        fields: Vec<(Ident, Expr)>,
    },
}

pub type Expr = Node<ExprKind>;

#[derive(Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Lt,
    Gt,
    Le,
    Ge,
    Ne,
    And,
    Or,
}
impl std::fmt::Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op_str = match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Mod => "%",
            BinOp::Eq => "==",
            BinOp::Lt => "<",
            BinOp::Gt => ">",
            BinOp::Le => "<=",
            BinOp::Ge => ">=",
            BinOp::Ne => "!=",
            BinOp::And => "and",
            BinOp::Or => "or",
        };
        write!(f, "{}", op_str)
    }
}

#[derive(Clone, Copy)]
pub enum UniOp {
    Neg,
    Ref,
    Deref,
    Not,
}
impl std::fmt::Display for UniOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op_str = match self {
            UniOp::Neg => "-",
            UniOp::Ref => "&",
            UniOp::Deref => "*",
            UniOp::Not => "!",
        };
        write!(f, "{}", op_str)
    }
}
