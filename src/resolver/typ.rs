use crate::ast::{BinOp, UniOp};
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Primitive(PrimitiveType),
    Slice(Rc<Type>),
    Ref(Rc<Type>),
    Ptr(Rc<Type>),
    Array(Rc<Type>, u64),
    Function(Rc<FunctionType>),
}
impl Type {
    pub const VOID: Self = Type::Primitive(PrimitiveType::Void);
    pub const ERROR: Self = Type::Primitive(PrimitiveType::Error);

    pub fn is_error(&self) -> bool {
        matches!(self, Type::Primitive(PrimitiveType::Error))
    }

    pub fn binop(&self, op: BinOp, other: &Self) -> Option<Type> {
        match (self, other) {
            (Type::Primitive(lhs), Type::Primitive(rhs)) => lhs.binop(op, rhs),
            _ => None,
        }
    }
    pub fn uniop(&self, op: UniOp) -> Option<Type> {
        match self {
            Type::Primitive(ty) => ty.uniop(op),
            Type::Ref(typ) | Type::Ptr(typ) => match op {
                UniOp::Deref => Some((*typ.as_ref()).clone()),
                UniOp::Not => Some(Type::Primitive(PrimitiveType::Bool)),
                _ => None,
            },
            _ => None,
        }
    }
}
#[derive(Debug, Clone, Copy)]
pub struct DisplayType<'a> {
    interner: &'a crate::strings::Interner,
    typ: &'a Type,
}
impl<'a> DisplayType<'a> {
    pub fn new(interner: &'a crate::strings::Interner, typ: &'a Type) -> Self {
        Self { interner, typ }
    }
    pub fn with(self, typ: &'a Type) -> Self {
        Self {
            interner: self.interner,
            typ,
        }
    }
}
impl<'a> std::fmt::Display for DisplayType<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.typ {
            Type::Primitive(prim) => write!(f, "{}", prim),
            Type::Slice(t) => write!(f, "[{}]", self.with(t)),
            Type::Ref(t) => write!(f, "&{}", self.with(t)),
            Type::Ptr(t) => write!(f, "*{}", self.with(t)),
            Type::Function(t) => write!(f, "{}", DisplayFunctionType::new(self.interner, t)),
            Type::Array(t, size) => write!(f, "[{}; {}]", self.with(t), size),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrimitiveType {
    I32,
    U8,
    Bool,
    Void,
    Error,
}
impl PrimitiveType {
    pub fn binop(&self, op: BinOp, other: &Self) -> Option<Type> {
        match (self, other) {
            (PrimitiveType::I32, PrimitiveType::I32) | (PrimitiveType::U8, PrimitiveType::U8) => {
                match op {
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod => {
                        Some(Type::Primitive(*self))
                    }
                    BinOp::Eq | BinOp::Ne | BinOp::Lt | BinOp::Le | BinOp::Gt | BinOp::Ge => {
                        Some(Type::Primitive(PrimitiveType::Bool))
                    }
                    _ => None,
                }
            }
            (PrimitiveType::Bool, PrimitiveType::Bool) => match op {
                BinOp::And | BinOp::Or | BinOp::Eq | BinOp::Ne => {
                    Some(Type::Primitive(PrimitiveType::Bool))
                }
                _ => None,
            },
            _ => None,
        }
    }
    pub fn uniop(&self, op: UniOp) -> Option<Type> {
        match self {
            PrimitiveType::I32 | PrimitiveType::U8 => match op {
                UniOp::Neg | UniOp::Not => Some(Type::Primitive(*self)),
                _ => None,
            },
            PrimitiveType::Bool => match op {
                UniOp::Not => Some(Type::Primitive(PrimitiveType::Bool)),
                _ => None,
            },
            _ => None,
        }
    }
}
impl std::fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrimitiveType::I32 => write!(f, "i32"),
            PrimitiveType::U8 => write!(f, "u8"),
            PrimitiveType::Bool => write!(f, "bool"),
            PrimitiveType::Void => write!(f, "void"),
            PrimitiveType::Error => write!(f, "<error>"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    pub ret: Type,
    pub args: Vec<Type>,
    pub var_args: bool,
}
pub struct DisplayFunctionType<'a> {
    interner: &'a crate::strings::Interner,
    typ: &'a FunctionType,
}
impl<'a> DisplayFunctionType<'a> {
    pub fn new(interner: &'a crate::strings::Interner, typ: &'a FunctionType) -> Self {
        Self { interner, typ }
    }
}
impl<'a> std::fmt::Display for DisplayFunctionType<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn(")?;
        for (i, arg) in self.typ.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            if self.typ.var_args && i == self.typ.args.len() - 1 {
                write!(f, "...")?;
            } else {
                write!(
                    f,
                    "{}",
                    DisplayType {
                        interner: self.interner,
                        typ: arg,
                    }
                )?;
            }
        }
        write!(
            f,
            ") : {}",
            DisplayType {
                interner: self.interner,
                typ: &self.typ.ret,
            }
        )
    }
}
