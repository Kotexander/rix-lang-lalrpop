use super::Expr;

#[derive(Debug, Clone)]
pub enum Instr {
    VarInit(String, Expr),
    Return(Option<Expr>),
    Expr(Expr),
}
