mod ast;
mod interpreter;
mod ir;
mod lexer;
#[cfg(feature = "llvm")]
mod llvm;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(grammar);

fn main() {
    #[cfg(feature = "llvm")]
    let llvm_ctx = llvm::initialize_llvm();

    let input = include_str!("../main.rix");
    let ast = grammar::ProgramParser::new().parse(input, lexer::Lexer::new(input));
    match ast {
        Ok(ast) => {
            // println!("{ast:#?}");
            for item in &ast {
                match item {
                    ast::Item::Function(name, body) => {
                        #[cfg(feature = "llvm")]
                        {
                            let mut ir = ir::generate(body);
                            ir::op_constant_fold(&mut ir);
                            let code_gen = llvm::CodeGen::new("rix_module", &llvm_ctx);
                            code_gen.generate(name, &ir);
                            code_gen.print_ir();
                            code_gen.optimize();
                            code_gen.print_ir();
                            code_gen.generate_object_file("rix.o");
                        }
                        #[cfg(not(feature = "llvm"))]
                        {
                            let mut ir = ir::generate(body);
                            println!("IR for function {name:?}:");
                            for i in &ir {
                                println!("{i}");
                            }
                            ir::op_constant_fold(&mut ir);
                            println!("\nAfter optimization:");
                            for i in &ir {
                                println!("{i}");
                            }
                        }
                    }
                }
            }

            println!();
            interpreter::run(&ast);
        }
        Err(e) => match e {
            lalrpop_util::ParseError::InvalidToken { location } => {
                println!("Invalid token at {location}");
            }
            lalrpop_util::ParseError::UnrecognizedEof { location, expected } => {
                print!("Unexpected token at {location}, expected: [");
                for exp in expected {
                    print!("{exp} ");
                }
                println!("]")
            }
            lalrpop_util::ParseError::UnrecognizedToken { token, expected } => {
                print!("Unexpected token: {token:?}, expected: [");
                for exp in expected {
                    print!("{exp}, ");
                }
                println!("]")
            }
            lalrpop_util::ParseError::ExtraToken { token } => {
                println!("Unexpected token: {token:?}")
            }
            lalrpop_util::ParseError::User { error } => println!("Error: {error}"),
        },
    }
}
