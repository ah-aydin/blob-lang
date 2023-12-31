mod ast;
mod parser;

use parser::{Parser, ParserStatus};
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: blob-lang [source_file]");
        return;
    }

    match Parser::new(args.get(1).unwrap()).unwrap().parse() {
        ParserStatus::Succeeded(stmts) => stmts.iter().for_each(|stmt| println!("{:?}", stmt)),
        ParserStatus::Failed => eprintln!("ERROR: Paring failed"),
        ParserStatus::IOError => eprintln!("ERROR: Failed while reading chunk from file"),
    };
}
