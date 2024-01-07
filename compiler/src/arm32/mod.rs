#[macro_use]
mod assembly;

use std::{
    fs::File,
    io::Write,
};

use ast::stmt::Stmt;

use crate::{common::CompileError, add};

use self::assembly::{Arm32Condition, Arm32Ins, Arm32Reg};

const ARM32_WORD_SIZE: u32 = 4;

pub struct Arm32Compiler {
    stmts: Vec<Stmt>,
}

impl Arm32Compiler {
    pub fn new(stmts: Vec<Stmt>) -> Arm32Compiler {
        Arm32Compiler { stmts }
    }

    pub fn compile(&self) -> Result<(), CompileError> {
        self.stmts.iter().for_each(|stmt| println!("{:?}", stmt));

        // TODO remove this test code when writing the compiler
        let mut file =
            File::create("build/output.s").expect("Why the heck did the file did not get created");
        file.write(b".text\n").unwrap();
        file.write(b".global main\n").unwrap();
        file.write(b"main:\n").unwrap();
        let instructions: Vec<Arm32Ins> = vec![
            push!(IP, LR),
            mov!(R0, "#44"),
            add!(R0, R0, "#2"),
            pop!(IP, LR),
            branch_exchange!(LR)
        ];
        instructions.iter().for_each(|instruction| {
            file.write(format!("{}\n", instruction).as_bytes()).unwrap();
        });

        Ok(())
    }
}
