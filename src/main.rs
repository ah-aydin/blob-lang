mod ast;
mod declartions;
mod file_coords;
mod log;
mod parser;
mod scanner;
mod semantic_analyzer;
mod simple_compiler;
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
            error!("Failed to open file '{}'", file_name);
            Err(1)
        }
    }?;

    let mut src = String::new();
    match file.read_to_string(&mut src) {
        Ok(_) => Ok(()),
        Err(_) => {
            error!("Failed to read contents of '{}'", file_name);
            Err(1)
        }
    }?;

    info!("Compiling and running {}...\n", file_name);

    let tokens = scanner::scan(&src);
    let ast = parser::parse(tokens);
    let extracted_declarations = declartions::extract_declarations(&ast);
    let contains_main = semantic_analyzer::analyze(&ast, &extracted_declarations);

    let file_name = format!("{}.asm", file_name);
    simple_compiler::compile(&ast, contains_main, file_name);

    Ok(())
}
