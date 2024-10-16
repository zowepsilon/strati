mod lexer;
mod ast;
mod parser;
mod interpreter;
mod stage1;

#[cfg(test)]
mod tests;

fn run_program(filename: &str) -> ast::Expression {
    use std::fs;

    use crate::lexer::Lexer;
    use crate::parser::Parser;

    let source = fs::read_to_string(filename).expect("unable to read file");
    let tokens = Lexer::new(&source).lex().expect("lexing error");
    
    let program = Parser::new(tokens).parse().expect("parsing error");

    program.interpret()
}

fn main() {
    use std::env;

    let mut args = env::args();

    let _ = args.next();
    let source_filename = args.next().expect("please provide a source file");
    let result = run_program(&source_filename);

    println!("{}", result.data);
}

