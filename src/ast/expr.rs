use super::Id;
use super::Node;

#[derive(Debug, Clone)]
pub enum ExprKind {
    Number(i32),
    String(String),

    Variable(Id),
    Call {
        name: Id,
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

    StructInit {
        name: Id,
        fields: Vec<(Id, Expr)>,
    },
}
impl std::fmt::Display for ExprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprKind::Number(n) => write!(f, "{}", n),
            ExprKind::Call { name, args } => {
                let args: Vec<String> = args.iter().map(|e| format!("{}", e.kind)).collect();
                write!(f, "{}({})", name.kind, args.join(", "))
            }
            ExprKind::BinOp { op, l, r } => write!(f, "({} {} {})", l.kind, op, r.kind),
            ExprKind::UniOp { op, expr } => write!(f, "({}{})", op, expr.kind),
            ExprKind::String(s) => write!(f, "{:?}", s),
            ExprKind::Variable(name) => write!(f, "{}", name.kind),
            ExprKind::StructInit { name, fields } => {
                let field_strs: Vec<String> = fields
                    .iter()
                    .map(|(field_name, expr)| format!(".{} = {}", field_name.kind, expr.kind))
                    .collect();
                write!(f, "{} {{ {} }}", name.kind, field_strs.join(", "))
            }
        }
    }
}

pub type Expr = Node<ExprKind>;

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
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
            BinOp::And => "and",
            BinOp::Or => "or",
        };
        write!(f, "{}", op_str)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UniOp {
    Neg,
    Ref,
    Deref,
}
impl std::fmt::Display for UniOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op_str = match self {
            UniOp::Neg => "-",
            UniOp::Ref => "&",
            UniOp::Deref => "*",
        };
        write!(f, "{}", op_str)
    }
}
