mod parser;
mod scanner;
mod token;

use blob_common::{error, info};
use std::{fs::File, io::Read};

pub fn assemble_file(file_name: &str) -> Result<(), i32> {
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

    info!("Assembling {}...\n", file_name);

    let _ = scanner::scan(&src);
    Ok(())
}
