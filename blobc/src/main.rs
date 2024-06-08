mod ast;
mod common;
mod parser;
mod scanner;
mod token;

use std::{env, fs::File, io::Read};

fn main() -> Result<(), i32> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: blob-lang [source_file]");
        return Err(1);
    }
    let file_name = args.get(1).unwrap();

    let mut file = match File::open(file_name) {
        Ok(file) => Ok(file),
        Err(_) => {
            eprintln!("Failed to open file '{}'", file_name);
            Err(1)
        }
    }?;

    let mut src = String::new();
    match file.read_to_string(&mut src) {
        Ok(_) => Ok(()),
        Err(_) => {
            eprintln!("Failed to read contents of '{}'", file_name);
            Err(1)
        }
    }?;

    println!("Compiling and running '{}'...", file_name);

    let tokens = scanner::scan(&src).unwrap();
    let ast = parser::parse(tokens).unwrap();
    ast.iter().for_each(|a| println!("{:?}", a));

    Ok(())
}
