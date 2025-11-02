use crate::ast::InstrKind;

use super::Instr;

#[derive(Debug, Clone)]
pub enum Item {
    Function {
        name: String,
        args: Vec<(String, String)>,
        body: Vec<Instr>,
    },
}
impl std::fmt::Display for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Item::Function { name, args, body } => {
                write!(f, "fn {}(", name)?;
                for (i, (arg_name, typ)) in args.iter().enumerate() {
                    write!(f, "{}: {}", arg_name, typ)?;
                    if i != args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                writeln!(f, ") {{")?;
                InstrKind::fmt_block(body, f, 1)?;
                writeln!(f, "\n}}")
            }
        }
    }
}
