mod ast;
mod parser;

use parser::Parser;
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: blob-lang [source_file]");
        return;
    }
    let mut parser = Parser::new(args.get(1).unwrap()).unwrap();
    match parser.parse() {
        Ok(stmts) => stmts.iter().for_each(|stmt| println!("{:?}", stmt)),
        Err(err) => eprintln!("{:?}", err),
    };
}
