use crate::ast;
use std::{collections::HashMap, ops::Deref};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Register(String);
impl Deref for Register {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl From<String> for Register {
    fn from(s: String) -> Self {
        Register(s)
    }
}
impl From<&str> for Register {
    fn from(s: &str) -> Self {
        Register(s.to_owned())
    }
}
impl std::fmt::Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.0)
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Number(i32),
    Register(Register),
}
impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Register(r) => write!(f, "{}", r),
        }
    }
}
impl From<Register> for Value {
    fn from(reg: Register) -> Self {
        Value::Register(reg)
    }
}

pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}
impl std::fmt::Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOp::Add => write!(f, "add"),
            BinOp::Sub => write!(f, "sub"),
            BinOp::Mul => write!(f, "mul"),
            BinOp::Div => write!(f, "div"),
        }
    }
}

pub enum InstrOp {
    Stack,
    Load(Register),

    Op(BinOp, Value, Value),
    Neg(Register),

    Call(String, Vec<Value>),
}
impl std::fmt::Display for InstrOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InstrOp::Stack => write!(f, "stack"),
            InstrOp::Load(name) => write!(f, "load {}", name),
            InstrOp::Op(op, l, r) => write!(f, "{} {}, {}", op, l, r),
            InstrOp::Neg(v) => write!(f, "neg {}", v),
            InstrOp::Call(name, args) => {
                let args_str: Vec<String> = args.iter().map(|a| format!("{}", a)).collect();
                write!(f, "call {}({})", name, args_str.join(", "))
            }
        }
    }
}

pub enum Instr {
    Assign { reg: Register, op: InstrOp },
    Store(Register, Value),
    Ret(Option<Value>),
}
impl std::fmt::Display for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instr::Assign { reg, op } => write!(f, "{} = {}", reg, op),
            Instr::Store(reg, value) => write!(f, "store {}, {}", reg, value),
            Instr::Ret(Some(value)) => write!(f, "ret {}", value),
            Instr::Ret(None) => write!(f, "ret void"),
        }
    }
}

struct ModuleBuilder {
    instructions: Vec<Instr>,
    temp_reg_counter: usize,
}
impl ModuleBuilder {
    fn new() -> Self {
        Self {
            instructions: vec![],
            temp_reg_counter: 0,
        }
    }
    fn push_assign(&mut self, instr: InstrOp) -> Register {
        let reg: Register = format!("t.{}", self.temp_reg_counter).into();
        let instr = Instr::Assign {
            reg: reg.clone(),
            op: instr,
        };
        self.temp_reg_counter += 1;
        self.instructions.push(instr);
        reg
    }
    fn push_ret(&mut self, value: Option<Value>) {
        let instr = Instr::Ret(value);
        self.instructions.push(instr);
    }
    fn push_store(&mut self, reg: Register, value: Value) {
        let instr = Instr::Store(reg, value);
        self.instructions.push(instr);
    }
    fn push(&mut self, instr: Instr) {
        self.instructions.push(instr);
    }
}

pub fn generate(ast: &[ast::Instr]) -> Vec<Instr> {
    let mut module = ModuleBuilder::new();
    for instr in ast {
        generate_instr(instr, &mut module);
    }
    module.instructions
}
fn generate_instr(instr: &ast::Instr, module: &mut ModuleBuilder) {
    match instr {
        ast::Instr::VarInit(name, expr) => {
            let reg: Register = name.clone().into();
            module.push(Instr::Assign {
                reg: reg.clone(),
                op: InstrOp::Stack,
            });

            let value = generate_expr(expr, module);
            module.push_store(reg, value);
        }
        ast::Instr::Return(expr) => {
            let v = expr.as_ref().map(|e| generate_expr(e, module));
            module.push_ret(v);
        }
        ast::Instr::Expr(expr) => {
            generate_expr(expr, module);
        }
    }
}
fn generate_expr(expr: &ast::Expr, module: &mut ModuleBuilder) -> Value {
    match expr {
        ast::Expr::Number(n) => Value::Number(*n),
        ast::Expr::Call(name, args) => {
            let args = args.iter().map(|a| generate_expr(a, module)).collect();
            module.push_assign(InstrOp::Call(name.clone(), args)).into()
        }
        ast::Expr::BinOp(l, op, r) => {
            let l_val = generate_expr(l, module);
            let r_val = generate_expr(r, module);

            let instr_op = match op {
                ast::BinOp::Add => BinOp::Add,
                ast::BinOp::Sub => BinOp::Sub,
                ast::BinOp::Mul => BinOp::Mul,
                ast::BinOp::Div => BinOp::Div,
            };
            module
                .push_assign(InstrOp::Op(instr_op, l_val, r_val))
                .into()
        }
        ast::Expr::UniOp(op, expr) => {
            let val = generate_expr(expr, module);
            match op {
                ast::UniOp::Neg => match val {
                    Value::Number(n) => Value::Number(-n),
                    Value::Register(reg) => module.push_assign(InstrOp::Neg(reg)).into(),
                },
                _ => unimplemented!(),
            }
        }
        ast::Expr::String(_) => module
            .push_assign(InstrOp::Load("string_todo".into()))
            .into(),
        ast::Expr::Variable(v) => module.push_assign(InstrOp::Load(v.as_str().into())).into(),
    }
}

pub fn op_constant_fold(ir: &mut Vec<Instr>) {
    let mut values: HashMap<Register, Option<i32>> = HashMap::new();
    let mut i = 0;
    while i < ir.len() {
        match &mut ir[i] {
            Instr::Assign { reg, op } => match op {
                InstrOp::Stack => {
                    values.insert(reg.clone(), None);
                }
                InstrOp::Load(register) => {
                    let v = values[register];
                    values.insert(reg.clone(), v);
                    if v.is_some() {
                        ir.remove(i);
                        continue;
                    }
                    // values.insert(reg.clone(), None);
                }
                InstrOp::Op(op, l, r) => {
                    let l_val = match l {
                        Value::Number(n) => Some(*n),
                        Value::Register(reg) => match values[reg] {
                            Some(n) => {
                                *l = Value::Number(n);
                                Some(n)
                            }
                            None => None,
                        },
                    };
                    let r_val = match r {
                        Value::Number(n) => Some(*n),
                        Value::Register(reg) => match values[reg] {
                            Some(n) => {
                                *r = Value::Number(n);
                                Some(n)
                            }
                            None => None,
                        },
                    };
                    match (l_val, r_val) {
                        (Some(lv), Some(rv)) => {
                            let result = match op {
                                BinOp::Add => lv + rv,
                                BinOp::Sub => lv - rv,
                                BinOp::Mul => lv * rv,
                                BinOp::Div => lv / rv,
                            };
                            values.insert(reg.clone(), Some(result));
                            ir.remove(i);
                            continue;
                        }
                        _ => {
                            values.insert(reg.clone(), None);
                        }
                    }
                }
                InstrOp::Neg(neg) => match values[neg] {
                    Some(n) => {
                        values.insert(reg.clone(), Some(-n));
                        ir.remove(i);
                        continue;
                    }
                    None => {
                        values.insert(reg.clone(), None);
                    }
                },
                InstrOp::Call(_, _) => {
                    values.insert(reg.clone(), None);
                }
            },
            Instr::Store(reg, value) => match value {
                Value::Number(n) => {
                    values.insert(reg.clone(), Some(*n));
                }
                Value::Register(register) => {
                    let v = values[register];
                    values.insert(reg.clone(), v);
                    if let Some(n) = v {
                        *value = Value::Number(n)
                    }
                }
            },
            Instr::Ret(Some(v)) => {
                if let Value::Register(register) = v {
                    let val = values[register];
                    if let Some(n) = val {
                        *v = Value::Number(n)
                    }
                }
                return;
            }
            Instr::Ret(None) => return,
        }
        i += 1;
    }
}
