use std::{env, path::Path};

use blob_asmlib::assemble_file;
use blob_common::{error, BUILD_FILE_PATH};
use blob_executable::{BlobExecutable, BLOB_EXECUTABLE_FILE_EXTENTION};

fn replace_extension(file_path: &str) -> String {
    let path = Path::new(file_path);
    if let Some(stem) = path.file_stem() {
        if let Some(stem_str) = stem.to_str() {
            return format!(
                "{}{}.{}",
                BUILD_FILE_PATH, stem_str, BLOB_EXECUTABLE_FILE_EXTENTION
            );
        }
    }

    error!("Failed to add extention to file");
    std::process::exit(1);
}

fn main() -> Result<(), i32> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: blob-asm [source_file]");
        return Err(1);
    }
    let file_name = args.get(1).unwrap();

    let program = assemble_file(&file_name);
    if program.is_err() {
        return Err(1);
    }

    match BlobExecutable::new(vec![], program.unwrap()).save(&replace_extension(&file_name)) {
        Ok(()) => Ok(()),
        Err(()) => Err(1),
    }
}
