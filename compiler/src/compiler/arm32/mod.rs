mod assembly;

use ast::stmt::Stmt;

const ARM32_WORD_SIZE: u32 = 4;

#[derive(Debug)]
pub enum CompileError {
    Failed,
}

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
