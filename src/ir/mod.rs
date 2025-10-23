use crate::ast;

#[derive(Debug, Clone)]
pub struct Register(String);
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
    Void,
}
impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::Register(r) => write!(f, "{}", r),
            Value::Void => write!(f, "void"),
        }
    }
}
impl From<Register> for Value {
    fn from(reg: Register) -> Self {
        Value::Register(reg)
    }
}

pub enum InstrOp {
    Stack,
    Load(Register),

    Add(Value, Value),
    Sub(Value, Value),
    Mul(Value, Value),
    Div(Value, Value),
    Neg(Register),

    Call(String, Vec<Value>),
}
impl std::fmt::Display for InstrOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InstrOp::Stack => write!(f, "stack"),
            // InstrOp::Store(reg, value) => write!(f, "store {}, {}", reg, value),
            InstrOp::Load(name) => write!(f, "load {}", name),
            InstrOp::Add(l, r) => write!(f, "add {}, {}", l, r),
            InstrOp::Sub(l, r) => write!(f, "sub {}, {}", l, r),
            InstrOp::Mul(l, r) => write!(f, "mul {}, {}", l, r),
            InstrOp::Div(l, r) => write!(f, "div {}, {}", l, r),
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
    Ret(Value),
}
impl std::fmt::Display for Instr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instr::Assign { reg, op } => write!(f, "{} = {}", reg, op),
            Instr::Store(reg, value) => write!(f, "store {}, {}", reg, value),
            Instr::Ret(value) => write!(f, "ret {}", value),
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
        let reg: Register = format!("{}", self.temp_reg_counter).into();
        let instr = Instr::Assign {
            reg: reg.clone(),
            op: instr,
        };
        self.temp_reg_counter += 1;
        self.instructions.push(instr);
        reg
    }
    fn push_ret(&mut self, value: Value) {
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
            if let Some(expr) = expr {
                let value = generate_expr(expr, module);
                module.push_ret(value);
            } else {
                module.push_ret(Value::Void);
            }
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
            // // check if both are numbers and fold them
            // if let (Value::Number(l_num), Value::Number(r_num)) = (&l_val, &r_val) {
            //     return match op {
            //         ast::BinOp::Add => Value::Number(l_num + r_num),
            //         ast::BinOp::Sub => Value::Number(l_num - r_num),
            //         ast::BinOp::Mul => Value::Number(l_num * r_num),
            //         ast::BinOp::Div => Value::Number(l_num / r_num),
            //     };
            // }
            let instr_op = match op {
                ast::BinOp::Add => InstrOp::Add(l_val, r_val),
                ast::BinOp::Sub => InstrOp::Sub(l_val, r_val),
                ast::BinOp::Mul => InstrOp::Mul(l_val, r_val),
                ast::BinOp::Div => InstrOp::Div(l_val, r_val),
            };
            module.push_assign(instr_op).into()
        }
        ast::Expr::UniOp(op, expr) => {
            let val = generate_expr(expr, module);
            match op {
                ast::UniOp::Neg => match val {
                    Value::Number(n) => Value::Number(-n),
                    Value::Register(reg) => module.push_assign(InstrOp::Neg(reg)).into(),
                    Value::Void => todo!(),
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
