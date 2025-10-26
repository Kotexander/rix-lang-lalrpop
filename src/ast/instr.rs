use super::Expr;

#[derive(Debug, Clone)]
pub enum Instr {
    VarInit(String, Expr),
    Return(Option<Expr>),
    Expr(Expr),

    /// for `String` in `Expr`..`Expr` {`Instrs`}
    For(String, Expr, Expr, Block),
    /// if `Expr` {`Instrs`} else if `Expr` {`Instrs`} ... else {`Instrs`}
    If(Expr, Block, Vec<(Expr, Block)>, Option<Block>),
}
pub type Block = Vec<Instr>;
impl std::fmt::Display for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instr::VarInit(name, expr) => write!(f, "var {} = {}", name, expr),
            Instr::Return(Some(expr)) => write!(f, "return {}", expr),
            Instr::Return(None) => write!(f, "return"),
            Instr::Expr(expr) => write!(f, "{}", expr),
            Instr::For(var, start, end, block) => {
                writeln!(f, "for {} in {}..{} {{", var, start, end)?;
                for instr in block {
                    writeln!(f, "    {}", instr)?;
                }
                write!(f, "}}")
            }
            Instr::If(cond, block, elifs, else_block) => {
                writeln!(f, "if {} {{", cond)?;
                for instr in block {
                    writeln!(f, "    {}", instr)?;
                }
                writeln!(f, "}}")?;
                for (elif_cond, elif_block) in elifs {
                    writeln!(f, "else if {} {{", elif_cond)?;
                    for instr in elif_block {
                        writeln!(f, "    {}", instr)?;
                    }
                    writeln!(f, "}}")?;
                }
                if let Some(els_block) = else_block {
                    writeln!(f, "else {{")?;
                    for instr in els_block {
                        writeln!(f, "    {}", instr)?;
                    }
                    writeln!(f, "}}")?;
                }
                Ok(())
            }
        }
    }
}
