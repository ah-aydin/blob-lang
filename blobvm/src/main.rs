mod repl;
mod vm;

use std::env;

use repl::REPL;

fn main() -> Result<(), i32> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: blobvm [repl]");
        return Err(1);
    }

    match args.get(1).unwrap().as_str() {
        "repl" => {
            REPL::new().run();
        }
        _ => {
            eprintln!("Unrecognized command");
        }
    };

    Ok(())
}
