mod ast;
mod cfg;
mod declartions;
mod file_coords;
mod log;
mod parser;
mod scanner;
mod semantic_analyzer;
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

    info!("Compiling and running {}...", file_name);

    let tokens = scanner::scan(&src);
    let ast = parser::parse(tokens);
    let extracted_declarations = declartions::extract_declarations(&ast);
    let _contains_main = semantic_analyzer::analyze(&ast, &extracted_declarations);
    // TODO add some optimizations that can be done on an AST

    let cfgs = cfg::build_cfgs(ast);
    for (func, cfg) in &cfgs {
        println!("Blocks for function '{func}'");
        let mut i = 0;
        for block in &cfg.blocks {
            match block {
                cfg::CfgBlock::Start(cfg_block_start) => {
                    println!(
                        "{i}: Start - successor {} - {:?}",
                        cfg_block_start.successor,
                        cfg_block_start.args.first()
                    )
                }
                cfg::CfgBlock::Basic(cfg_block_basic) => {
                    println!(
                        "{i}: Basic - successor {} - {:?}",
                        cfg_block_basic.successor,
                        cfg_block_basic.stmts.first()
                    )
                }
                cfg::CfgBlock::Condition(cfg_block_condition) => {
                    println!(
                        "{i}: Condition - true successor {} - false successor {} - {:?}",
                        cfg_block_condition.true_successor,
                        cfg_block_condition.false_successor,
                        cfg_block_condition.condition
                    )
                }
                cfg::CfgBlock::Exit => {
                    println!("{i}: Exit")
                }
            }
            i += 1;
        }
        println!();
    }

    Ok(())
}
