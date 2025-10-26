mod scope;

use crate::ast::{BinOp, Expr, Instr, Item, UniOp};
use scope::{ScopeGuard, ScopeManager};

#[derive(Debug, Clone)]
enum Value {
    Number(i32),
    String(String),
    Bool(bool),
    Void,
}
impl Value {
    fn as_number(&self) -> Result<i32, String> {
        match self {
            Value::Number(n) => Ok(*n),
            _ => Err("Value is not a number".to_owned()),
        }
    }
    fn as_bool(&self) -> Result<bool, String> {
        match self {
            Value::Bool(b) => Ok(*b),
            _ => Err("Value is not a boolean".to_owned()),
        }
    }
}

pub fn run(ast: &[Item]) {
    for item in ast {
        match item {
            Item::Function(name, block) => {
                println!("Running function: {name:?}");
                if let Err(err) = run_block(block, ScopeManager::new().new_scope()) {
                    eprintln!("{err}");
                }
            }
        }
    }
}

fn run_block(block: &[Instr], mut scope: ScopeGuard) -> Result<(), String> {
    for instr in block {
        match instr {
            Instr::VarInit(id, val) => {
                let value = eval_expr(val, &mut scope)?;
                scope.set_var(id.clone(), value);
            }
            Instr::Return(Some(expr)) => {
                println!("return: {:?}", eval_expr(expr, &mut scope));
                break;
            }
            Instr::Return(None) => {
                println!("return: None");
                break;
            }
            Instr::Expr(expr) => {
                eval_expr(expr, &mut scope)?;
            }
            Instr::For(id, start, end, block) => {
                let start = eval_expr(start, &mut scope)?.as_number()?;
                let end = eval_expr(end, &mut scope)?.as_number()?;

                for i in start..end {
                    let mut new_scope = scope.new_scope();
                    new_scope.set_var(id.clone(), Value::Number(i));
                    run_block(block, new_scope)?;
                }
            }
            Instr::If(expr, block, elifs, els) => {
                let cond = eval_expr(expr, &mut scope)?.as_bool()?;
                if cond {
                    run_block(block, scope.new_scope())?;
                } else {
                    let mut executed_elif = false;
                    for (elif_expr, elif_block) in elifs {
                        let elif_cond = eval_expr(elif_expr, &mut scope)?.as_bool()?;
                        if elif_cond {
                            let new_scope = scope.new_scope();
                            run_block(elif_block, new_scope)?;
                            executed_elif = true;
                            break;
                        }
                    }
                    if !executed_elif && let Some(els_block) = els {
                        let new_scope = scope.new_scope();
                        run_block(els_block, new_scope)?;
                    }
                }
            }
        }
    }
    Ok(())
}

fn eval_expr(expr: &Expr, scope: &mut ScopeGuard) -> Result<Value, String> {
    match expr {
        Expr::Number(i) => Ok(Value::Number(*i)),
        Expr::BinOp(l, bin_op, r) => {
            let v0 = eval_expr(l, scope)?;
            let v1 = eval_expr(r, scope)?;
            match (v0, v1) {
                (Value::Number(l), Value::Number(r)) => match bin_op {
                    BinOp::Add => Ok(Value::Number(l + r)),
                    BinOp::Sub => Ok(Value::Number(l - r)),
                    BinOp::Mul => Ok(Value::Number(l * r)),
                    BinOp::Div => Ok(Value::Number(l / r)),
                    BinOp::Mod => Ok(Value::Number(l % r)),
                    BinOp::Eq => Ok(Value::Bool(l == r)),
                    _ => Err(format!("Unsupported binary operation for numbers: {expr}")),
                },
                (Value::Bool(l), Value::Bool(r)) => match bin_op {
                    BinOp::And => Ok(Value::Bool(l && r)),
                    BinOp::Eq => Ok(Value::Bool(l == r)),
                    _ => Err(format!("Unsupported binary operation for booleans: {expr}")),
                },
                _ => Err(format!(
                    "Unsupported operand types for binary operation: {expr}"
                )),
            }
        }
        Expr::UniOp(uni_op, expr) => {
            let v = eval_expr(expr, scope)?;
            match (v, uni_op) {
                (Value::Number(n), UniOp::Neg) => Ok(Value::Number(-n)),
                _ => Err(format!(
                    "Unsupported operand type for unary operation: {expr}"
                )),
            }
        }
        Expr::String(s) => Ok(Value::String(s.clone())),
        Expr::Variable(id) => scope
            .get_var(id)
            .cloned()
            .ok_or_else(|| format!("Undefined variable: {}", id)),
        Expr::Call(func_name, args) => match func_name.as_str() {
            "printf" => {
                for arg in args {
                    let arg = eval_expr(arg, scope)?;

                    match arg {
                        Value::Number(n) => print!("{n} "),
                        Value::String(s) => print!("{s} "),
                        Value::Void => print!("void "),
                        Value::Bool(b) => print!("{b} "),
                    }
                }
                println!();
                Ok(Value::Void)
            }
            _ => Err(format!("Unknown function: {}", func_name)),
        },
    }
}
