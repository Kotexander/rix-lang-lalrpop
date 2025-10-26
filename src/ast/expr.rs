#[derive(Debug, Clone)]
pub enum Expr {
    Number(i32),
    Call(String, Vec<Expr>),
    BinOp(Box<Expr>, BinOp, Box<Expr>),
    UniOp(UniOp, Box<Expr>),
    String(String),
    Variable(String),
}
impl Expr {
    pub fn bin_op(left: Expr, op: BinOp, right: Expr) -> Self {
        Expr::BinOp(Box::new(left), op, Box::new(right))
    }
    pub fn uni_op(op: UniOp, expr: Expr) -> Self {
        Expr::UniOp(op, Box::new(expr))
    }
}
impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Number(n) => write!(f, "{}", n),
            Expr::Call(name, exprs) => {
                let exprs_str: Vec<String> = exprs.iter().map(|e| format!("{}", e)).collect();
                write!(f, "{}({})", name, exprs_str.join(", "))
            }
            Expr::BinOp(left, op, right) => write!(f, "({} {} {})", left, op, right),
            Expr::UniOp(op, expr) => write!(f, "({}{})", op, expr),
            Expr::String(s) => write!(f, "\"{}\"", s),
            Expr::Variable(name) => write!(f, "{}", name),
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
