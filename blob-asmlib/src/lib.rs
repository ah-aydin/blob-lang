mod linker;
mod scanner;
mod token;

use blob_common::{error, info};
use std::{fs::File, io::Read};

pub const GENERAL_REG_COUNT: usize = 28;
pub const SP_REG: usize = GENERAL_REG_COUNT;
pub const LR_REG: usize = GENERAL_REG_COUNT + 1;
pub const REG_COUNT: usize = LR_REG + 1;

pub fn compile_instruction(ins: &str) -> Result<Vec<u8>, ()> {
    let tokens = scanner::scan(&ins)?;
    Ok(linker::link(tokens)?)
}

pub fn assemble_file(file_name: &str) -> Result<Vec<u8>, ()> {
    let mut file = match File::open(file_name) {
        Ok(file) => Ok(file),
        Err(_) => {
            error!("Failed to open file '{}'", file_name);
            Err(())
        }
    }?;

    let mut src = String::new();
    match file.read_to_string(&mut src) {
        Ok(_) => Ok(()),
        Err(_) => {
            error!("Failed to read contents of '{}'", file_name);
            Err(())
        }
    }?;

    info!("Assembling {}...", file_name);

    let tokens = scanner::scan(&src)?;
    Ok(linker::link(tokens)?)
}
