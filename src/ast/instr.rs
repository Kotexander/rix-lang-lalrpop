use super::{Expr, Span};

#[derive(Debug, Clone)]
pub enum InstrKind {
    VarInit {
        name: String,
        expr: Expr,
    },
    VarAssign {
        name: String,
        expr: Expr,
    },
    Return(Option<Expr>),
    Expr(Expr),

    For {
        var: String,
        start: Expr,
        end: Expr,
        body: Vec<Instr>,
    },
    If {
        cond: Expr,
        then: Vec<Instr>,
        elifs: Vec<(Expr, Vec<Instr>)>,
        els: Option<Vec<Instr>>,
    },

    Error,
}
impl InstrKind {
    fn fmt_indent(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        for _ in 0..indent {
            write!(f, "    ")?;
        }
        macro_rules! new_ident {
            () => {
                writeln!(f)?;
                for _ in 0..indent {
                    write!(f, "    ")?;
                }
            };
        }
        match self {
            InstrKind::VarInit { name, expr } => write!(f, "var {} = {}", name, expr.kind),
            InstrKind::VarAssign { name, expr } => write!(f, "{} = {}", name, expr.kind),
            InstrKind::Return(Some(expr)) => write!(f, "return {}", expr.kind),
            InstrKind::Return(None) => write!(f, "return"),
            InstrKind::Expr(expr) => write!(f, "{}", expr.kind),
            InstrKind::For {
                var,
                start,
                end,
                body: block,
            } => {
                writeln!(f, "for {} in {}..{} {{", var, start.kind, end.kind)?;
                Self::fmt_block(block, f, indent + 1)?;
                new_ident!();
                write!(f, "}}")
            }
            InstrKind::If {
                cond,
                then,
                elifs,
                els,
            } => {
                writeln!(f, "if {} {{", cond.kind)?;
                Self::fmt_block(then, f, indent + 1)?;
                new_ident!();
                write!(f, "}}")?;

                for (elif_cond, elif_block) in elifs {
                    new_ident!();
                    writeln!(f, "else if {} {{", elif_cond.kind)?;
                    Self::fmt_block(elif_block, f, indent + 1)?;
                    new_ident!();
                    write!(f, "}}")?;
                }
                if let Some(els_block) = els {
                    new_ident!();
                    writeln!(f, "else {{")?;
                    Self::fmt_block(els_block, f, indent + 1)?;
                    new_ident!();
                    write!(f, "}}")?;
                }
                Ok(())
            }
            InstrKind::Error => write!(f, "<error>"),
        }
    }

    pub fn fmt_block(
        block: &[Instr],
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        for (i, instr) in block.iter().enumerate() {
            instr.kind.fmt_indent(f, indent)?;
            if i != block.len() - 1 {
                writeln!(f)?;
            }
        }
        Ok(())
    }
}
impl std::fmt::Display for InstrKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_indent(f, 0)
    }
}

#[derive(Debug, Clone)]
pub struct Instr {
    pub kind: InstrKind,
    pub span: Span,
}
impl Instr {
    pub fn new(start: usize, kind: InstrKind, end: usize) -> Self {
        Self {
            kind,
            span: Span { start, end },
        }
    }
}
