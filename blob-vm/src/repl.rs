use blob_common::{error, info};
use blob_executable::{BlobExecutable, BLOB_EXECUTABLE_FILE_EXTENTION};

use crate::vm::VM;
use std::io::{self, Write};

pub struct REPL {
    command_buffer: Vec<String>,
    ci: usize,
    vm: VM,
}

impl REPL {
    pub fn new() -> REPL {
        REPL {
            command_buffer: vec![],
            ci: 0,
            vm: VM::new(),
        }
    }

    pub fn run(&mut self) {
        let mut buffer = String::with_capacity(64);
        let stdin = io::stdin();
        let mut stdout = io::stdout();
        println!("BlobVM 0.1");
        println!();
        loop {
            buffer.clear();
            print!(">>> ");
            stdout.flush().expect("Failed to flush stdout");

            stdin
                .read_line(&mut buffer)
                .expect("Failed to read input from user");
            let buffer = buffer.trim();
            let success = match buffer {
                ".history" => {
                    for command in &self.command_buffer {
                        println!("{command}");
                    }
                    true
                }
                ".registers" | ".regs" => {
                    self.vm.print_regs();
                    true
                }
                ".exit" | ".quit" => {
                    std::process::exit(0);
                }

                file if file.starts_with(".run_executable") => {
                    let split: Vec<&str> = file.split(" ").collect();
                    if split.len() != 2 {
                        error!("Expected one argument for file name");
                        false
                    } else {
                        let file = split.get(1).unwrap();
                        if !file.ends_with(BLOB_EXECUTABLE_FILE_EXTENTION) {
                            error!(
                                "Given file is not a '.{}' file",
                                BLOB_EXECUTABLE_FILE_EXTENTION
                            );
                        }
                        let executable = BlobExecutable::load_from_file(&file);
                        match executable {
                            Ok(executable) => {
                                info!("Running executable file '{file}'");
                                self.vm.set_and_run_program(executable);
                                info!("Run complete!");
                                true
                            }
                            Err(_) => {
                                error!("Failed to load executable file '{file}'");
                                false
                            }
                        }
                    }
                }

                file if file.starts_with(".run_assembly") => {
                    let split: Vec<&str> = file.split(" ").collect();
                    if split.len() != 2 {
                        error!("Expected one argument for file name");
                        false
                    } else {
                        let file = split.get(1).unwrap();
                        match blob_asmlib::assemble_file(file) {
                            Ok(program) => {
                                info!("Running file '{file}'...");
                                self.vm
                                    .set_and_run_program(BlobExecutable::new(vec![], program));
                                info!("Run complete!");
                            }
                            Err(()) => {
                                error!("Failed!")
                            }
                        };
                        true
                    }
                }
                _ => {
                    error!("Invalid command");
                    false
                }
            };
            if success {
                self.command_buffer.push(buffer.to_string());
                self.ci = self.command_buffer.len();
            }
        }
    }
}
