mod assembly;

use ast::stmt::Stmt;

use crate::common::CompileError;

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
        Ok(())
    }
}
