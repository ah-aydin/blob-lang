use blob_common::{error, info};

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
        println!("BlobVM 0.0.1");
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
                file if file.starts_with(".run_file") => {
                    let split: Vec<&str> = file.split(" ").collect();
                    if split.len() != 2 {
                        error!("Expected one argument for file name");
                        false
                    } else {
                        let file = split.get(1).unwrap();
                        match blob_asmlib::assemble_file(file) {
                            Ok(program) => {
                                info!("Running file '{file}'...");
                                self.vm.set_program(program);
                                self.vm.run();
                                info!("Run complete!");
                            }
                            Err(()) => {
                                error!("Failed!")
                            }
                        };
                        true
                    }
                }
                ins => {
                    let bytes = blob_asmlib::compile_instruction(&ins);
                    if bytes.is_err() {
                        error!("Failed to compile instruction!");
                        false
                    } else {
                        let mut bytes = bytes.unwrap();
                        self.vm.add_bytes(&mut bytes);
                        self.vm.execute_instruction();
                        true
                    }
                }
            };
            if success {
                self.command_buffer.push(buffer.to_string());
                self.ci = self.command_buffer.len();
            }
        }
    }
}
