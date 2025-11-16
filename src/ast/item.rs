use super::Id;
use super::Instr;
use super::InstrKind;
use super::Node;
use super::Typ;

#[derive(Debug, Clone)]
pub enum ItemKind {
    Function {
        name: Id,
        args: Vec<(Id, Typ)>,
        ret: Option<Typ>,
        body: Option<Vec<Instr>>,
    },
    Struct {
        name: Id,
        fields: Vec<(Id, Typ)>,
    },
    Union {
        name: Id,
        variants: Vec<(Id, Typ)>,
    },
    // Error,
}
impl std::fmt::Display for ItemKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ItemKind::Function {
                name,
                args,
                ret,
                body,
            } => {
                write!(f, "fn {}(", name.kind)?;
                for (i, (arg_name, typ)) in args.iter().enumerate() {
                    write!(f, "{}: {}", arg_name.kind, typ.kind)?;
                    if i != args.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                if let Some(body) = body {
                    if let Some(ret_type) = ret {
                        writeln!(f, ") : {} {{", ret_type.kind)?;
                    } else {
                        writeln!(f, ") {{")?;
                    }
                    InstrKind::fmt_block(body, f, 1)?;
                    writeln!(f, "\n}}")
                } else {
                    writeln!(f, ")")?;
                    if let Some(ret_type) = ret {
                        write!(f, " : {}", ret_type.kind)
                    } else {
                        Ok(())
                    }
                }
            }
            ItemKind::Struct { name, fields } => {
                writeln!(f, "struct {} {{", name.kind)?;
                for (field_name, typ) in fields {
                    writeln!(f, "    {}: {}", field_name.kind, typ.kind)?;
                }
                writeln!(f, "}}")
            }
            ItemKind::Union { name, variants } => {
                writeln!(f, "union {} {{", name.kind)?;
                for (variant_name, typ) in variants {
                    writeln!(f, "    {}: {}", variant_name.kind, typ.kind)?;
                }
                writeln!(f, "}}")
            } // ItemKind::Error => writeln!(f, "<error>"),
        }
    }
}
pub type Item = Node<ItemKind>;
