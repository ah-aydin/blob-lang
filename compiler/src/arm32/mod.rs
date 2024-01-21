#[macro_use]
mod assembly;

use std::{collections::HashMap, fs::File, io::Write, process::Command};

use ast::{
    expr::Expr,
    stmt::{Stmt, StmtFuncDecl},
};

use crate::common::{CompileError, FuncData};

use self::assembly::Arm32Ins;

const ARM32_WORD_SIZE: u32 = 4;
type CompilerResult = Result<(), CompileError>;

pub struct Arm32Compiler {
    instructions: Vec<Arm32Ins>,
    functions: HashMap<String, FuncData>,
}

impl Arm32Compiler {
    pub fn new() -> Arm32Compiler {
        Arm32Compiler {
            instructions: vec![],
            functions: HashMap::new(),
        }
    }

    fn reset(&mut self) {
        self.instructions.clear();
        self.functions.clear();
    }

    fn link(&self, file_name: &str) -> CompilerResult {
        let mut file = File::create(format!("build/{}.s", file_name))
            .expect("Why the heck did the file did not get created");
        file.write(b".text\n").unwrap();
        file.write(b".global main\n").unwrap();
        self.instructions.iter().for_each(|instruction| {
            file.write(format!("{}\n", instruction).as_bytes()).unwrap();
        });

        let which_gcc = match std::env::consts::ARCH {
            "arm" | "armv7" => "gcc",
            "x86" | "x86_64" => "arm-linux-gnueabihf-gcc -static",
            _ => {
                eprintln!("Unsupported architecture: {}", std::env::consts::ARCH);
                std::process::exit(1);
            }
        };
        let gcc_command = format!(
            "{} ./build/{}.s -o ./build/{}",
            which_gcc, file_name, file_name
        );
        match Command::new("sh").arg("-c").arg(gcc_command).output() {
            Ok(_) => Ok(()),
            Err(_) => {
                eprintln!("[ERROR] {} is not installed.", which_gcc);
                Err(CompileError::Failed)
            }
        }
    }

    pub fn compile(&mut self, stmts: Vec<Stmt>, file_name: &str) -> CompilerResult {
        self.reset();

        for stmt in &stmts {
            let _ = match stmt {
                Stmt::FuncDecl(stmt_func_decl) => self.func(&stmt_func_decl)?,
                _ => unreachable!("Got unexpected global statement"),
            };
        }

        self.link(file_name)
    }

    fn func(&mut self, func_decl: &StmtFuncDecl) -> CompilerResult {
        let func_name = func_decl.name.as_str();
        self.functions
            .insert(String::from(func_name), FuncData::from_stmt(func_decl));
        self.instructions.push(label!(func_name));

        // Prologue
        self.instructions.push(push!(FP, LR));

        let stmts = match *func_decl.body.clone() {
            Stmt::Block(stmts) => stmts,
            _ => unreachable!()
        };
        self.block_stmt(&stmts)?;

        // Epilogue
        self.instructions.push(pop!(FP, PC));
        Ok(())
    }

    fn block_stmt(&mut self, stmts: &Vec<Stmt>) -> CompilerResult {
        for stmt in stmts {
            match stmt {
                Stmt::ExprStmt(expr) => self.expr_stmt(expr),
                Stmt::Block(stmts) => self.block_stmt(stmts),
                Stmt::Return(expr) => self.return_stmt(expr),
                Stmt::If(_) => todo!(),
                Stmt::IfElse(_) => todo!(),
                Stmt::VarDecl(_) => todo!(),
                Stmt::Assign(_) => todo!(),
                Stmt::While(_) => todo!(),
                Stmt::FuncDecl(_) => {
                    unreachable!("Did not expect a function decleration inside a block")
                }
            }?;
        }

        Ok(())
    }

    fn expr_stmt(&mut self, expr: &Expr) -> CompilerResult {
        self.expr(expr)?;
        Ok(())
    }

    fn return_stmt(&mut self, expr: &Expr) -> CompilerResult {
        self.expr(expr)?;
        Ok(())
    }

    fn expr(&mut self, expr: &Expr) -> CompilerResult {
        match expr {
            Expr::Number(number) => self.i32_expr(number),
            Expr::Identifier(_) => todo!(),
            Expr::UnaryOp(_) => todo!(),
            Expr::BinaryOp(_) => todo!(),
            Expr::Call(_) => todo!(),
        }
    }

    fn i32_expr(&mut self, number: &str) -> CompilerResult {
        self.instructions.push(ldr!(R0, number));
        Ok(())
    }
}
