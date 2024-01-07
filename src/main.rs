mod ast;
mod parser;

use parser::{Parser, ParserStatus};
use std::env;

fn cmain() -> i32 {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: blob-lang [source_file]");
        return 1;
    }
    let file_name = args.get(1).unwrap();

    let parser = Parser::new(file_name);
    if parser.is_err() {
        eprintln!(
            "Failed to read file {}\n{}",
            file_name,
            parser.err().unwrap()
        );
        return 1;
    }
    let mut parser = parser.unwrap();
    match parser.parse() {
        ParserStatus::Succeeded(stmts) => {
            stmts.iter().for_each(|stmt| println!("{:?}", stmt));
            0
        }
        ParserStatus::Failed => {
            eprintln!("ERROR: Parsing failed");
            1
        }
        ParserStatus::IOError => {
            eprintln!("ERROR: Failed while reading chunk from file");
            1
        }
    }
}

fn main() {
    std::process::exit(cmain());
}
