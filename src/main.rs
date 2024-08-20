mod lexer;
mod ast;
mod parser;
mod interpreter;

fn main() {
    use std::{env, fs};
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    let mut args = env::args();

    let _ = args.next();
    let source_filename = args.next().expect("please provide a source file");

    let source = fs::read_to_string(source_filename).expect("unable to read file");
    let tokens = Lexer::new(&source).lex().expect("lexing error");
    
    let program = Parser::new(tokens).parse().expect("parsing error");
    
    // for stmt in program.root {
    //     println!("{stmt}");
    // }
    
    program.interpret();

    // println!("{typed_program:#?}");
}
