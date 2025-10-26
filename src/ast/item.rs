use super::Instr;

#[derive(Debug, Clone)]
pub enum Item {
    Function(String, Vec<Instr>),
}
impl std::fmt::Display for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Item::Function(name, body) => {
                writeln!(f, "fn {}() {{", name)?;
                for instr in body {
                    writeln!(f, "{}", instr)?;
                }
                writeln!(f, "}}")
            }
        }
    }
}
