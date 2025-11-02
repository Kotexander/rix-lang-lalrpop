mod ast;
mod lexer;
mod llvm;
lalrpop_mod!(
    #[allow(clippy::ptr_arg)]
    grammar
);

use clap::Parser;
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};
use lalrpop_util::{ParseError, lalrpop_mod};
use lexer::Tok;

enum ReturnStatus {
    Syntax,
    Io { file: String, kind: std::io::Error },
}
impl std::fmt::Debug for ReturnStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReturnStatus::Syntax => write!(f, "one or more syntax errors"),
            ReturnStatus::Io { file, kind } => {
                write!(f, "'{}': {}", file, kind)
            }
        }
    }
}

#[derive(Debug, Clone, clap::Parser)]
struct Cli {
    /// Source file
    pub input: String,

    /// Write output to <FILE>
    #[arg(short, long, value_name = "FILE")]
    output: Option<String>,

    /// Generate LLVM IR instead of object file
    #[arg(long)]
    pub emit_llvm: bool,
}
impl Cli {
    pub fn output(&mut self) -> &str {
        if self.output.is_none() {
            let mut p = std::path::PathBuf::from(&self.input);
            p.set_extension(if self.emit_llvm { "ll" } else { "o" });
            self.output = Some(p.to_string_lossy().to_string());
        }
        &self.output.as_ref().unwrap()
    }
}

fn main() -> Result<(), ReturnStatus> {
    let mut cli = Cli::parse();

    let input = std::fs::read_to_string(&cli.input).map_err(|kind| ReturnStatus::Io {
        file: cli.input.clone(),
        kind,
    })?;

    let mut files = SimpleFiles::new();
    let file = files.add(&cli.input, &input);

    let mut writer = StandardStream::stderr(ColorChoice::Auto);
    let config = term::Config::default();

    let mut errors = Vec::new();
    let ast = grammar::ProgramParser::new().parse(&input, &mut errors, lexer::Lexer::new(&input));

    match ast {
        Ok(ast) if errors.is_empty() => {
            for item in &ast {
                println!("{item}");
            }

            let llvm_ctx = llvm::initialize_llvm();
            let mut code_gen = llvm::CodeGen::new(&cli.input, &llvm_ctx);
            for item in &ast {
                match item {
                    ast::Item::Function { name, args, body } => {
                        code_gen.generate(name, args, body);
                    }
                }
            }
            // code_gen.print_ir();
            code_gen.optimize();
            // code_gen.print_ir();
            if cli.emit_llvm {
                code_gen.save_ir(cli.output());
            } else {
                code_gen.generate_object_file(cli.output());
            }
            Ok(())
        }
        Err(e) => {
            term::emit_to_write_style(&mut writer, &config, &files, &make_error(&e, file)).unwrap();
            Err(ReturnStatus::Syntax)
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
            Err(ReturnStatus::Syntax)
        }
    }
}

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
