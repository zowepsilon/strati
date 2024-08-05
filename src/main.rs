use std::{process::ExitCode, env};

mod lexer;
mod ast;
mod parser;
mod typer;
mod interpreter;

fn main() -> ExitCode {
    use std::fs;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::typer::Typer;

    let mut args = env::args();
    let _ = args.next();
    let source_filename = args.next().expect("please provide a source file");

    let source = fs::read_to_string(source_filename).expect("unable to read file");
    let tokens = Lexer::new(&source).lex().expect("lexing error");
    
    let untyped_program = Parser::new(tokens).parse().expect("parsing error");
    let typed_program = Typer::new().type_program(untyped_program).expect("typing error");
    
    typed_program.interpret();

    // println!("{typed_program:#?}");

    ExitCode::SUCCESS
}
