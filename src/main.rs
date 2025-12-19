mod ast;
mod lexer;
mod llvm;
mod resolver;
mod strings;
lalrpop_mod!(
    #[allow(clippy::ptr_arg)]
    grammar
);

use ast::AstBuilder;
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

    /// Output file
    #[arg(short, long)]
    output: Option<String>,

    /// Run with optimizations
    #[arg(short = 'O', long)]
    pub optimize: bool,

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
        self.output.as_ref().unwrap()
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
    let mut ast_builder = AstBuilder::default();
    let ast = grammar::ProgramParser::new().parse(
        &input,
        &mut ast_builder,
        &mut errors,
        lexer::Lexer::new(&input),
    );
    // println!("{:#?}", ast_builder.interner);

    let mut errors: Vec<_> = errors
        .into_iter()
        .map(|e| make_error(&e.error, file))
        .collect();

    let mut annotations = None;
    if let Ok(ast) = &ast {
        let mut resolver = resolver::Resolver::new(&mut ast_builder.interner, &mut errors, file);
        resolver.resolve(ast);
        annotations = Some(resolver.finish());
    }

    match ast {
        Ok(ast) if errors.is_empty() => {
            for item in &ast {
                println!(
                    "{}",
                    ast::debug::DisplayItem::new(&ast_builder.interner, &item.kind)
                );
            }

            let llvm_ctx = llvm::initialize_llvm();
            let mut code_gen = llvm::CodeGen::new(
                &cli.input,
                &llvm_ctx,
                &ast_builder.interner,
                annotations.as_ref().unwrap(),
            );

            code_gen.generate(&ast);

            if cli.optimize {
                code_gen.optimize();
            }

            if cli.emit_llvm {
                code_gen.print(cli.output());
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
                term::emit_to_write_style(&mut writer, &config, &files, error).unwrap();
            }
            Err(ReturnStatus::Syntax)
        }
    }
}

fn make_error(error: &ParseError<usize, Tok, lexer::Error>, file: usize) -> Diagnostic<usize> {
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
        ParseError::User { error } => match error {
            lexer::Error::UnterminatedString { span } => Diagnostic::error()
                .with_message("unterminated string literal")
                .with_label(
                    Label::primary(file, span.clone()).with_message("missing closing `\"`"),
                ),
            lexer::Error::UnknownCharacter { span, character } => Diagnostic::error()
                .with_message(format!("unknown character: '{}'", character))
                .with_label(Label::primary(file, span.clone())),
            lexer::Error::UnkownEscapeSequence { span, esc_span } => Diagnostic::error()
                .with_message("unknown escape sequence")
                .with_label(Label::primary(file, esc_span.clone()))
                .with_label(Label::secondary(file, span.clone())),
            lexer::Error::ParseInt { span, err } => Diagnostic::error()
                .with_message(format!("failed to parse integer: {}", err))
                .with_label(Label::primary(file, span.clone())),
        },
    }
}
