use super::Expr;

#[derive(Debug, Clone)]
pub enum Instr {
    Call(String, Vec<Expr>),
    Return(Option<Expr>),
}
