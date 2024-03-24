use crate::ast::stmt::Stmt;

use self::{arm32::Arm32Compiler, common::CompileError};

pub mod arm32;
pub mod common;

pub fn compile(stmts: Vec<Stmt>, file_name: &str) -> Result<(), CompileError> {
    Arm32Compiler::new().compile(stmts, file_name)
}
