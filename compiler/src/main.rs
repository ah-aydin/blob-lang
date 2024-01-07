mod compiler;

use compiler::arm32::Arm32Compiler;
use parser::{Parser, ParserStatus};
use std::env;

fn cmain() -> Result<i32, i32> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: blob-lang [source_file]");
        return Err(1);
    }
    let file_name = args.get(1).unwrap();

    let parser = Parser::new(file_name);
    if parser.is_err() {
        eprintln!(
            "Failed to read file {}\n{}",
            file_name,
            parser.err().unwrap()
        );
        return Err(1);
    }

    let stmts = match parser.unwrap().parse() {
        ParserStatus::Succeeded(stmts) => Ok(stmts),
        ParserStatus::Failed => {
            eprintln!("[ERROR] Parsing failed");
            Err(1)
        }
        ParserStatus::IOError => {
            eprintln!("[ERROR] Failed while reading chunk from file");
            Err(1)
        }
    }?;

    match Arm32Compiler::new(stmts).compile() {
        Ok(()) => Ok(1),
        Err(compiler_error) => {
            eprintln!("[ERROR] compilation failed {:?}", compiler_error);
            Err(1)
        }
    }
}

fn main() {
    std::process::exit(match cmain() {
        Ok(exit_code) | Err(exit_code) => exit_code,
    });
}
