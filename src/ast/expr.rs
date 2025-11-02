use super::Span;

#[derive(Debug, Clone)]
pub enum ExprKind {
    Number(i32),
    String(String),

    Variable(String),
    Call {
        name: String,
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
    // Error,
}
impl std::fmt::Display for ExprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprKind::Number(n) => write!(f, "{}", n),
            ExprKind::Call { name, args } => {
                let args: Vec<String> = args.iter().map(|e| format!("{}", e.kind)).collect();
                write!(f, "{}({})", name, args.join(", "))
            }
            ExprKind::BinOp { op, l, r } => write!(f, "({} {} {})", l.kind, op, r.kind),
            ExprKind::UniOp { op, expr } => write!(f, "({}{})", op, expr.kind),
            ExprKind::String(s) => write!(f, "\"{}\"", s),
            ExprKind::Variable(name) => write!(f, "{}", name),
            // ExprKind::Error => write!(f, "<error>"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}
impl Expr {
    pub fn new(start: usize, typ: ExprKind, end: usize) -> Self {
        Self {
            kind: typ,
            span: Span { start, end },
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    And,
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
