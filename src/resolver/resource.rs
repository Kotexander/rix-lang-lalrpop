use crate::{ast::Span, strings};

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Function { span: Span },
    Variable { span: Span },
}
impl Value {
    pub fn span(&self) -> Span {
        match self {
            Value::Function { span } => *span,
            Value::Variable { span } => *span,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum PrimitiveType {
    I32,
    U8,
    Bool,
}

#[derive(Debug, Clone, Copy)]
pub enum Type {
    Prim(PrimitiveType),
    Custom(strings::Id, Span),
}
