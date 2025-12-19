use super::typ;
use crate::ast::Span;

#[derive(Debug, Clone)]
pub enum Value {
    Function {
        span: Span,
        typ: Option<typ::FunctionType>,
    },
    Variable {
        span: Span,
        typ: Option<typ::Type>,
    },
}
impl Value {
    pub fn span(&self) -> Span {
        match self {
            Value::Function { span, .. } => *span,
            Value::Variable { span, .. } => *span,
        }
    }
    pub fn typ(&self) -> Option<typ::Type> {
        match self {
            Value::Function { typ, .. } => typ
                .as_ref()
                .map(|f| typ::Type::Function(std::rc::Rc::new(f.clone()))),
            Value::Variable { typ, .. } => typ.clone(),
        }
    }
}
