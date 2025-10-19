mod ast;

use lalrpop_util::lalrpop_mod;

lalrpop_mod!(grammar);

fn main() {
    // let ast = grammar::ExprParser::new().parse("-1*(22+1*2-1)");
    let ast = grammar::ProgramParser::new().parse(include_str!("../main.rix"));
    match ast {
        Ok(ast) => println!("{ast:#?}"),
        Err(e) => println!("Error: {:?}", e),
    }
}
