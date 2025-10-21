use crate::ast::{Expr, Instr, Item};

pub fn run(ast: &[Item]) {
    for item in ast {
        match item {
            Item::Function(name, body) => {
                println!("Running function: {name:?}");
                for instr in body {
                    match instr {
                        Instr::Call(func_name, args) => match func_name.as_str() {
                            "print" => {
                                for arg in args {
                                    match arg {
                                        Expr::String(lit) => {
                                            print!("{lit} ");
                                        }
                                        Expr::Number(num) => {
                                            print!("{num} ");
                                        }
                                        _ => {
                                            println!("\nPrint: Unsupported argument {arg:?}");
                                            return;
                                        }
                                    }
                                }
                                println!()
                            }
                            _ => {
                                println!("Unknown function call to {func_name}");
                                return;
                            }
                        },
                        Instr::Return(ret_expr) => {
                            println!("Function {name:?} returned: {ret_expr:?}");
                            break;
                        }
                    }
                }
            }
        }
    }
}
