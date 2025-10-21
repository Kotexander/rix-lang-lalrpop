mod ast;
mod interpreter;
mod lexer;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(grammar);

fn main() {
    // let ast = grammar::ExprParser::new().parse("-1*(22+1*2-1)");
    let input = include_str!("../main.rix");
    let ast = grammar::ProgramParser::new().parse(input, lexer::Lexer::new(input));
    match ast {
        Ok(ast) => {
            println!("{ast:#?}");
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

    // println!("{:#?}", lexer::Lexer::new(input).collect::<Vec<_>>());
}
