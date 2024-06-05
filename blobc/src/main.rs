mod ast;

use std::env;

fn main() -> Result<(), i32> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: blob-lang [source_file]");
        return Err(1);
    }
    let file_name = args.get(1).unwrap();

    println!("{}", file_name);

    Ok(())
}
