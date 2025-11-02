mod ast;
mod lexer;
mod llvm;

use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};
use lalrpop_util::{ParseError, lalrpop_mod};

use crate::lexer::Tok;

lalrpop_mod!(
    #[allow(clippy::ptr_arg)]
    grammar
);

fn make_error(error: &ParseError<usize, Tok, String>, file: usize) -> Diagnostic<usize> {
    match error {
        ParseError::InvalidToken { location } => {
            let l = *location;
            Diagnostic::error()
                .with_message("unexpected token")
                .with_label(Label::primary(file, l..l))
        }
        ParseError::UnrecognizedEof { location, expected } => {
            let l = *location;
            let expected_str = expected.join(", ");
            Diagnostic::error()
                .with_message("unexpected end of file")
                .with_label(Label::primary(file, l..l))
                .with_note(format!("expected one of: {}", expected_str))
        }
        ParseError::UnrecognizedToken { token, expected } => {
            let (start, _tok, end) = token;
            let s = *start;
            let e = *end;
            let expected_str = expected.join(", ");
            Diagnostic::error()
                .with_message("unexpected token")
                .with_label(Label::primary(file, s..e))
                .with_note(format!("expected one of: {}", expected_str))
        }
        ParseError::ExtraToken { token } => {
            let (start, _, end) = token;
            let s = *start;
            let e = *end;
            Diagnostic::error()
                .with_message("extra token")
                .with_label(Label::primary(file, s..e))
        }
        ParseError::User { error } => Diagnostic::error().with_message(error),
    }
}

fn main() {
    let llvm_ctx = llvm::initialize_llvm();
    let input = include_str!("../main.rix");
    // let file = SimpleFile::new("main.rix", input);

    let mut files = SimpleFiles::new();
    let file = files.add("main.rix", input);

    let mut writer = StandardStream::stderr(ColorChoice::Auto);
    let config = term::Config::default();

    let mut errors = Vec::new();
    let ast = grammar::ProgramParser::new().parse(input, &mut errors, lexer::Lexer::new(input));

    match ast {
        Ok(ast) if errors.is_empty() => {
            // println!("{ast:#?}");

            for item in &ast {
                println!("{item}");
            }

            let mut code_gen = llvm::CodeGen::new("main.rix", &llvm_ctx);
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
        Err(e) => {
            term::emit_to_write_style(&mut writer, &config, &files, &make_error(&e, file)).unwrap()
        }
        Ok(_) => {
            for error in &errors {
                term::emit_to_write_style(
                    &mut writer,
                    &config,
                    &files,
                    &make_error(&error.error, file),
                )
                .unwrap();
            }
        }
    }
}
