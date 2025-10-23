use crate::ast::{BinOp, Expr, Instr, Item, UniOp};
use std::collections::HashMap;

#[derive(Debug, Clone)]
enum Value {
    Number(i32),
    String(String),
    None,
}

struct Scope {
    // a stack of scopes
    vars: Vec<HashMap<String, Value>>,
}
impl Scope {
    fn new() -> Self {
        Self { vars: vec![] }
    }
    fn enter_scope(&mut self) {
        self.vars.push(HashMap::new());
    }
    fn exit_scope(&mut self) {
        self.vars.pop();
    }
    fn set_var(&mut self, name: String, value: Value) {
        if let Some(scope) = self.vars.last_mut() {
            scope.insert(name, value);
        } else {
            panic!("No scope to set variable in");
        }
    }
    fn get_var(&self, name: &str) -> Option<&Value> {
        for scope in self.vars.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }
        None
    }
}

fn eval_expr(expr: &Expr, scope: &Scope) -> Result<Value, String> {
    match expr {
        Expr::Number(i) => Ok(Value::Number(*i)),
        Expr::BinOp(expr, bin_op, expr1) => {
            let v0 = eval_expr(expr, scope)?;
            let v1 = eval_expr(expr1, scope)?;
            match (v0, v1, bin_op) {
                (Value::Number(l), Value::Number(r), BinOp::Add) => Ok(Value::Number(l + r)),
                (Value::Number(l), Value::Number(r), BinOp::Sub) => Ok(Value::Number(l - r)),
                (Value::Number(l), Value::Number(r), BinOp::Mul) => Ok(Value::Number(l * r)),
                (Value::Number(l), Value::Number(r), BinOp::Div) => Ok(Value::Number(l / r)),
                _ => Err("Unsupported operand types for binary operation".to_owned()),
            }
        }
        Expr::UniOp(uni_op, expr) => {
            let v = eval_expr(expr, scope)?;
            match (v, uni_op) {
                (Value::Number(n), UniOp::Neg) => Ok(Value::Number(-n)),
                _ => Err("Unsupported operand type for unary operation".to_owned()),
            }
        }
        Expr::String(s) => Ok(Value::String(s.clone())),
        Expr::Variable(id) => scope
            .get_var(id)
            .cloned()
            .ok_or_else(|| format!("Undefined variable: {}", id)),
        Expr::Call(func_name, args) => match func_name.as_str() {
            "print" => {
                for arg in args {
                    let arg = eval_expr(arg, &scope);
                    let arg = match arg {
                        Ok(v) => v,
                        Err(e) => {
                            return Err(format!("Error evaluating argument for print: {e}"));
                        }
                    };

                    match arg {
                        Value::Number(n) => print!("{n} "),
                        Value::String(s) => print!("{s} "),
                        Value::None => print!("!"),
                    }
                }
                println!();
                Ok(Value::None)
            }
            _ => Err(format!("Unknown function: {}", func_name)),
        },
    }
}

pub fn run(ast: &[Item]) {
    for item in ast {
        match item {
            Item::Function(name, body) => {
                println!("Running function: {name:?}");

                let mut scope = Scope::new();
                scope.enter_scope();

                for instr in body {
                    match instr {
                        Instr::VarInit(id, val) => match eval_expr(val, &scope) {
                            Ok(value) => {
                                scope.set_var(id.clone(), value);
                            }
                            Err(e) => {
                                println!("\nError initializing variable {id:?}: {e}");
                                return;
                            }
                        },
                        Instr::Return(ret_expr) => {
                            println!(
                                "Function {name:?} returned: {:?}",
                                ret_expr.as_ref().map(|e| eval_expr(e, &scope))
                            );
                            break;
                        }
                        Instr::Expr(expr) => match eval_expr(expr, &scope) {
                            Ok(_) => {}
                            Err(e) => {
                                println!("\nError evaluating expression: {e}");
                                return;
                            }
                        },
                    }
                }

                scope.exit_scope();
            }
        }
    }
}
