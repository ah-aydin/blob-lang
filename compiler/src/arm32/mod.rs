#[macro_use]
mod assembly;
mod builtin_instructions;

use std::{collections::HashMap, fs::File, io::Write, process::Command};

use ast::{
    expr::{Expr, ExprBinaryOp, ExprBooleanOp, ExprCall, ExprUnaryOp},
    op_type::{BinaryOpType, BooleanOpType, UnaryOpType},
    stmt::{Stmt, StmtFuncDecl},
};

use crate::common::{CompileError, FuncData};

use self::assembly::Arm32Ins;

const ARM32_WORD_SIZE: usize = 4;

type CompilerResult = Result<(), CompileError>;

pub struct Arm32Compiler {
    instructions: Vec<Arm32Ins>,
    functions: HashMap<String, FuncData>,
    func_label_count: usize,
    current_func: String,
}

impl Arm32Compiler {
    pub fn new() -> Arm32Compiler {
        Arm32Compiler {
            instructions: vec![],
            functions: HashMap::new(),
            func_label_count: 0,
            current_func: String::new(),
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
        let output = Command::new("sh")
            .arg("-c")
            .arg(gcc_command)
            .output()
            .expect(&format!("[ERROR] {} is not installed", which_gcc));

        if !output.status.success() {
            eprintln!(
                "Command failed with: {}",
                String::from_utf8_lossy(&output.stderr)
            );
            return Err(CompileError::Failed);
        }
        Ok(())
    }

    pub fn compile(&mut self, stmts: Vec<Stmt>, file_name: &str) -> CompilerResult {
        self.reset();

        self.instructions
            .append(&mut builtin_instructions::div_instructions());

        for stmt in &stmts {
            self.func_label_count = 0;
            let _ = match stmt {
                Stmt::FuncDecl(stmt_func_decl) => self.func(&stmt_func_decl)?,
                _ => unreachable!("Got unexpected global statement"),
            };
        }

        self.link(file_name)
    }

    fn func(&mut self, func_decl: &StmtFuncDecl) -> CompilerResult {
        let func_name = func_decl.name.as_str();
        self.current_func = String::from(func_name);
        self.functions
            .insert(String::from(func_name), FuncData::from_stmt(func_decl));
        self.instructions.push(self.gen_func_label(func_name));

        // Prologue
        self.instructions.push(push!(FP, LR));

        let stmts = match *func_decl.body.clone() {
            Stmt::Block(stmts) => stmts,
            _ => unreachable!(),
        };
        self.block_stmt(&stmts)?;

        // Epilogue
        if func_name == "main" {
            self.instructions
                .append(&mut builtin_instructions::get_exit_instructions());
        }
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
            Expr::UnaryOp(unary_op) => self.unary_expr(unary_op),
            Expr::BinaryOp(binary_op) => self.binary_expr(binary_op),
            Expr::BooleanOp(boolean_op) => self.boolean_expr(boolean_op),
            Expr::Call(call) => self.call(call),
        }
    }

    fn i32_expr(&mut self, number: &str) -> CompilerResult {
        self.instructions.push(ldr!(R0, number));
        Ok(())
    }

    fn unary_expr(&mut self, unary_op: &ExprUnaryOp) -> CompilerResult {
        self.expr(&*unary_op.term)?;
        match unary_op.op_type {
            UnaryOpType::Negate => {
                self.instructions.push(neg!(R0, R0));
            }
            UnaryOpType::Not => {
                self.instructions.push(cmp!(R0, "#0"));
                self.instructions.push(mov!(R0, "#1", Eq));
                self.instructions.push(mov!(R0, "#0", Ne));
            }
        };
        Ok(())
    }

    fn binary_expr(&mut self, binary_op: &ExprBinaryOp) -> CompilerResult {
        self.expr(&binary_op.left_term)?;
        self.instructions.push(push!(R0, IP));
        self.expr(&binary_op.right_term)?;
        self.instructions.push(pop!(R1, IP));
        match binary_op.op_type {
            BinaryOpType::Add => self.instructions.push(add!(R0, R0, R1)),
            BinaryOpType::Subtract => self.instructions.push(sub!(R0, R1, R0)),
            BinaryOpType::Multiply => self.instructions.push(mul!(R0, R0, R1)),
            BinaryOpType::Divide => self
                .instructions
                .append(&mut builtin_instructions::goto_div_instructions()),
        };
        Ok(())
    }

    fn boolean_expr(&mut self, boolean_op: &ExprBooleanOp) -> CompilerResult {
        if boolean_op.op_type != BooleanOpType::And || boolean_op.op_type != BooleanOpType::Or {
            self.expr(&boolean_op.left_term)?;
            self.instructions.push(push!(R0, IP));
            self.expr(&boolean_op.right_term)?;
            self.instructions.push(pop!(R1, IP));
        }
        match boolean_op.op_type {
            BooleanOpType::And => todo!(),
            BooleanOpType::Or => todo!(),
            BooleanOpType::Equal => self.instructions.append(&mut vec![
                cmp!(R1, R0),
                mov!(R0, "#1", Eq),
                mov!(R0, "#0", Ne),
            ]),
            BooleanOpType::NotEqual => self.instructions.append(&mut vec![
                cmp!(R1, R0),
                mov!(R0, "#1", Ne),
                mov!(R0, "#0", Eq),
            ]),
            BooleanOpType::Greater => self.instructions.append(&mut vec![
                cmp!(R1, R0),
                mov!(R0, "#1", Gt),
                mov!(R0, "#0", Le),
            ]),
            BooleanOpType::GreaterOrEqual => self.instructions.append(&mut vec![
                cmp!(R1, R0),
                mov!(R0, "#1", Ge),
                mov!(R0, "#0", Lt),
            ]),
            BooleanOpType::Less => self.instructions.append(&mut vec![
                cmp!(R1, R0),
                mov!(R0, "#1", Lt),
                mov!(R0, "#0", Ge),
            ]),

            BooleanOpType::LessOrEqual => self.instructions.append(&mut vec![
                cmp!(R1, R0),
                mov!(R0, "#1", Le),
                mov!(R0, "#0", Gt),
            ]),
        };
        Ok(())
    }

    fn call(&mut self, call: &ExprCall) -> CompilerResult {
        let func_name = call.name.as_str();
        match call.args.len() {
            0 => self.instructions.push(bl!(func_name)),
            1 => {
                self.expr(call.args.get(0).unwrap())?;
                self.instructions.push(bl!(func_name));
            }
            // 2 => {}
            // 3 => {}
            2 | 3 | 4 => {
                self.instructions.push(sub!(SP, SP, "#16"));
                for i in 0..call.args.len() {
                    self.expr(&call.args.get(0).unwrap())?;
                    let offset = format!("{}", i * ARM32_WORD_SIZE)
                        .parse::<String>()
                        .unwrap();
                    self.instructions.push(str!(R0, [SP, offset]));
                }
                self.instructions.push(pop!(R0, R1, R2, R3));
                self.instructions.push(bl!(func_name));
            }
            _ => {
                todo!("Implement function calls with more then 4 arguments")
            }
        }
        Ok(())
    }

    /////////////////////////////////////////////////////////////////
    /// Label generators
    /////////////////////////////////////////////////////////////////
    fn gen_func_label(&self, func_name: &str) -> Arm32Ins {
        Arm32Ins::Label(String::from(func_name))
    }

    fn gen_in_func_label(&mut self, s: &str) -> Arm32Ins {
        self.func_label_count += 1;
        Arm32Ins::Label(format!(
            "__{}__{}_{}_",
            self.current_func, s, self.func_label_count
        ))
    }
}
