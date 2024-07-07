use crate::vm::VM;
use std::{
    io::{self, Write},
    num::ParseIntError,
};

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
                hex => {
                    let bytes = parse_hex(&hex);
                    if bytes.is_err() {
                        eprintln!("Invalid hex command");
                        false
                    } else {
                        let mut bytes = bytes.unwrap();
                        if bytes.len() != 0 {
                            self.vm.add_bytes(&mut bytes);
                            self.vm.execute_instruction();
                            true
                        } else {
                            false
                        }
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

fn parse_hex(hex: &str) -> Result<Vec<u8>, ParseIntError> {
    let hex: String = hex.chars().filter(|c| !c.is_whitespace()).collect();
    let mut v = Vec::with_capacity(4);
    let mut i = 0;
    while i < hex.len() {
        v.push(u8::from_str_radix(&hex[i..(i + 2)], 16)?);
        i += 2;
    }

    if v.len() > 1 && v.len() <= 4 {
        return Ok(v);
    }
    Ok(vec![])
}
