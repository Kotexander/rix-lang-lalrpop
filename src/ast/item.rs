use super::Instr;

#[derive(Debug, Clone)]
pub enum Item {
    Function(String, Vec<Instr>),
}
