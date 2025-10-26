mod ast;
mod interpreter;
mod lexer;
mod llvm;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(grammar);

fn main() {
    let llvm_ctx = llvm::initialize_llvm();

    let input = include_str!("../main.rix");
    let ast = grammar::ProgramParser::new().parse(input, lexer::Lexer::new(input));
    match ast {
        Ok(ast) => {
            // println!("{ast:#?}");
            for item in &ast {
                println!("{item}");
            }
            interpreter::run(&ast);
            println!();

            let mut code_gen = llvm::CodeGen::new("rix_module", &llvm_ctx);
            for item in &ast {
                match item {
                    ast::Item::Function(name, block) => {
                        code_gen.generate(name, block);
                    }
                }
            }
            code_gen.print_ir();
            code_gen.optimize();
            code_gen.print_ir();
            code_gen.generate_object_file("rix.o");
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
