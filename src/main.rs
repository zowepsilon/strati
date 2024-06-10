use std::process::ExitCode;

mod lexer;
mod parser;

fn main() -> ExitCode {
    use std::fs;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    let source = fs::read_to_string("test.str").expect("unable to read file");
    let tokens = match Lexer::new(&source).lex() {
        Ok(tokens) => tokens,
        Err(errors) => {
            for err in errors {
                println!("Lexing error: {:?} at :{}:{}", err.kind, err.position.line, err.position.column);
            }

            return ExitCode::FAILURE;
        }
    };
    
    let ast = Parser::new(tokens).parse().unwrap();

    println!("{ast:#?}");

    ExitCode::SUCCESS
}
