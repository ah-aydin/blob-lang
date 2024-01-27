#[macro_use]
mod assembly;
mod builtin_instructions;

use self::assembly::Arm32Ins;
use crate::common::CompileError;
use ast::{
    blob_type::BlobType,
    expr::{Expr, ExprBinaryOp, ExprBooleanOp, ExprCall, ExprUnaryOp},
    op_type::{BinaryOpType, BooleanOpType, UnaryOpType},
    stmt::{Stmt, StmtFuncDecl, StmtIf, StmtIfElse},
};
use std::{fs::File, io::Write, process::Command};

const ARM32_WORD_SIZE: usize = 4;

type CompilerResult = Result<(), CompileError>;

#[derive(Default)]
struct FuncEnv {
    name: String,
    label_count: usize,
    arg_count: usize,
    local_vars: Vec<(String, BlobType)>,
}

impl FuncEnv {
    /// Returns the offset from the FP (frame pointer) of the variable
    fn get_var_offset(&self, param_name: &str) -> i32 {
        let index = self
            .local_vars
            .iter()
            .position(|(name, _)| name == param_name)
            .expect("Value is not present in the local variable list");
        match self.arg_count {
            0 => None,
            1 | 2 => match index {
                0 => Some(-2 * ARM32_WORD_SIZE as i32),
                1 => Some(-1 * ARM32_WORD_SIZE as i32),
                _ => None,
            },
            3 | 4 => match index {
                0 => Some(-4 * ARM32_WORD_SIZE as i32),
                1 => Some(-3 * ARM32_WORD_SIZE as i32),
                2 => Some(-2 * ARM32_WORD_SIZE as i32),
                3 => Some(-1 * ARM32_WORD_SIZE as i32),
                _ => None,
            },
            _ => todo!("Implement functions with more then 4 arguments"),
        }
        .unwrap_or(self.get_non_param_offset(index))
    }

    fn get_non_param_offset(&self, index: usize) -> i32 {
        let after_args_offset = match self.arg_count {
            0 => 0,
            1 | 2 => -2 * ARM32_WORD_SIZE as i32,
            3 | 4 => -4 * ARM32_WORD_SIZE as i32,
            _ => todo!("Implement functions with more then 4 argumetns"),
        };
        -(((index - 3) * ARM32_WORD_SIZE) as i32) + after_args_offset
    }
}

pub struct Arm32Compiler {
    instructions: Vec<Arm32Ins>,
    current_func_env: FuncEnv
}

impl Arm32Compiler {
    pub fn new() -> Arm32Compiler {
        Arm32Compiler {
            instructions: vec![],
            current_func_env: FuncEnv::default()
        }
    }

    fn reset(&mut self) {
        self.instructions.clear();
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
            self.current_func_env.label_count = 0;
            let _ = match stmt {
                Stmt::FuncDecl(stmt_func_decl) => self.func(&stmt_func_decl)?,
                _ => unreachable!("Got unexpected global statement"),
            };
        }

        self.link(file_name)
    }

    fn stmt(&mut self, stmt: &Stmt) -> CompilerResult {
        match stmt {
            Stmt::Block(stmts) => self.block_stmt(stmts),
            Stmt::ExprStmt(expr) => self.expr_stmt(expr),
            Stmt::Return(expr) => self.return_stmt(expr),
            Stmt::If(iff) => self.if_stmt(iff),
            Stmt::IfElse(if_else) => self.if_else_stmt(if_else),
            Stmt::VarDecl(_) => todo!(),
            Stmt::Assign(_) => todo!(),
            Stmt::While(_) => todo!(),
            Stmt::FuncDecl(_) => {
                unreachable!("Did not expect a function decleration inside a block")
            }
        }
    }

    fn func(&mut self, func_decl: &StmtFuncDecl) -> CompilerResult {
        let func_name = func_decl.name.as_str();
        let is_main = func_name == "main";
        // Setup environment
        self.current_func_env.name = String::from(func_name);
        self.current_func_env.arg_count = func_decl.args.len();
        self.current_func_env.local_vars = func_decl.args.clone();
        // TODO put `.global` in here for the function name
        self.emit(self.gen_func_label(func_name));

        // Prologue
        self.emit(push!(FP, LR));
        self.emit(mov!(FP, SP));
        match func_decl.args.len() {
            0 => {}
            1 => self.emit(push!(R0, R1)), // Maintain 8-byte stack allignment
            2 => self.emit(push!(R0, R1)),
            3 => self.emit(push!(R0, R1, R2, R3)), // Maintain 8-byte stack allignment
            4 => self.emit(push!(R0, R1, R2, R3)),
            _ => todo!("Implement function calls with more then 4 arguments"),
        }

        self.stmt(&func_decl.body)?;

        // Epilogue
        if is_main {
            self.instructions
                .append(&mut builtin_instructions::get_exit_instructions());
        } else {
            self.emit(mov!(SP, FP));
            // self.emit(mov!(R0, #0));
        }
        self.emit(pop!(FP, PC));
        Ok(())
    }

    fn block_stmt(&mut self, stmts: &Vec<Stmt>) -> CompilerResult {
        for stmt in stmts {
            self.stmt(stmt)?;
        }

        Ok(())
    }

    fn expr_stmt(&mut self, expr: &Expr) -> CompilerResult {
        self.expr(expr)?;
        Ok(())
    }

    fn return_stmt(&mut self, expr: &Expr) -> CompilerResult {
        self.expr(expr)?;
        self.emit(mov!(SP, FP));
        self.emit(pop!(FP, PC));
        Ok(())
    }

    fn if_stmt(&mut self, iff: &StmtIf) -> CompilerResult {
        let if_end_label = self.gen_in_func_label("ifEnd");
        self.expr(&iff.condition)?;
        self.emit_multiple(&mut vec![cmp!(R0, #0), b!(if_end_label.get_label(), Eq)]);
        self.stmt(&iff.clause)?;
        self.emit(if_end_label);
        Ok(())
    }

    fn if_else_stmt(&mut self, if_else: &StmtIfElse) -> CompilerResult {
        let if_else_end_label = self.gen_in_func_label("ifElseEnd");
        let if_else_false_label = self.gen_in_func_label("ifElseFalse");
        self.expr(&if_else.condition)?;
        self.emit_multiple(&mut vec![
            cmp!(R0, #0),
            b!(if_else_false_label.get_label(), Eq),
        ]);
        self.stmt(&if_else.if_clause)?;
        self.emit(b!(if_else_end_label.get_label()));
        self.emit(if_else_false_label);
        self.stmt(&if_else.else_clause)?;
        self.emit(if_else_end_label);
        Ok(())
    }

    fn expr(&mut self, expr: &Expr) -> CompilerResult {
        match expr {
            Expr::Number(number) => self.i32_expr(number),
            Expr::Identifier(name) => self.identifier_expr(name),
            Expr::UnaryOp(unary_op) => self.unary_expr(unary_op),
            Expr::BinaryOp(binary_op) => self.binary_expr(binary_op),
            Expr::BooleanOp(boolean_op) => self.boolean_expr(boolean_op),
            Expr::Call(call) => self.call(call),
        }
    }

    fn i32_expr(&mut self, number: &str) -> CompilerResult {
        self.emit(ldr!(R0, number));
        Ok(())
    }

    fn identifier_expr(&mut self, name: &str) -> CompilerResult {
        let offset = self.current_func_env.get_var_offset(name);
        self.emit(ldr!(R0, [FP, #offset]));
        Ok(())
    }

    fn unary_expr(&mut self, unary_op: &ExprUnaryOp) -> CompilerResult {
        self.expr(&*unary_op.term)?;
        match unary_op.op_type {
            UnaryOpType::Negate => {
                self.emit(neg!(R0, R0));
            }
            UnaryOpType::Not => {
                self.emit(cmp!(R0, #0));
                self.emit(mov!(R0, #1, Eq));
                self.emit(mov!(R0, #0, Ne));
            }
        };
        Ok(())
    }

    fn binary_expr(&mut self, binary_op: &ExprBinaryOp) -> CompilerResult {
        self.expr(&binary_op.left_term)?;
        self.emit(push!(R0, IP));
        self.expr(&binary_op.right_term)?;
        self.emit(pop!(R1, IP));
        match binary_op.op_type {
            BinaryOpType::Add => self.emit(add!(R0, R0, R1)),
            BinaryOpType::Subtract => self.emit(sub!(R0, R1, R0)),
            BinaryOpType::Multiply => self.emit(mul!(R0, R0, R1)),
            BinaryOpType::Divide => self
                .instructions
                .append(&mut builtin_instructions::goto_div_instructions()),
        };
        Ok(())
    }

    fn boolean_expr(&mut self, boolean_op: &ExprBooleanOp) -> CompilerResult {
        if boolean_op.op_type != BooleanOpType::And && boolean_op.op_type != BooleanOpType::Or {
            self.expr(&boolean_op.left_term)?;
            self.emit(push!(R0, IP));
            self.expr(&boolean_op.right_term)?;
            self.emit(pop!(R1, IP));
        }
        match boolean_op.op_type {
            BooleanOpType::And => {
                self.expr(&boolean_op.left_term)?;
                let and_end_label = self.gen_in_func_label("andEnd");
                self.emit_multiple(&mut vec![cmp!(R0, #0), b!(and_end_label.get_label(), Eq)]);
                self.expr(&boolean_op.right_term)?;
                self.emit(and_end_label);
            }
            BooleanOpType::Or => {
                self.expr(&boolean_op.left_term)?;
                let or_end_label = self.gen_in_func_label("orEnd");
                self.emit_multiple(&mut vec![cmp!(R0, #1), b!(or_end_label.get_label(), Eq)]);
                self.expr(&boolean_op.right_term)?;
                self.emit(or_end_label);
            }
            BooleanOpType::Equal => {
                self.emit_multiple(&mut vec![cmp!(R1, R0), mov!(R0, #1, Eq), mov!(R0, #0, Ne)])
            }
            BooleanOpType::NotEqual => {
                self.emit_multiple(&mut vec![cmp!(R1, R0), mov!(R0, #1, Ne), mov!(R0, #0, Eq)])
            }
            BooleanOpType::Greater => {
                self.emit_multiple(&mut vec![cmp!(R1, R0), mov!(R0, #1, Gt), mov!(R0, #0, Le)])
            }
            BooleanOpType::GreaterOrEqual => {
                self.emit_multiple(&mut vec![cmp!(R1, R0), mov!(R0, #1, Ge), mov!(R0, #0, Lt)])
            }
            BooleanOpType::Less => {
                self.emit_multiple(&mut vec![cmp!(R1, R0), mov!(R0, #1, Lt), mov!(R0, #0, Ge)])
            }

            BooleanOpType::LessOrEqual => {
                self.emit_multiple(&mut vec![cmp!(R1, R0), mov!(R0, #1, Le), mov!(R0, #0, Gt)])
            }
        };
        Ok(())
    }

    fn call(&mut self, call: &ExprCall) -> CompilerResult {
        let func_name = call.name.as_str();
        let arg_count = call.args.len();
        match arg_count {
            0 => self.emit(bl!(func_name)),
            1 => {
                self.expr(call.args.get(0).unwrap())?;
                self.emit(bl!(func_name));
            }
            2 => {
                self.emit(sub!(SP, SP, #8));
                for i in 0..2 {
                    self.expr(&call.args.get(i).unwrap())?;
                    let offset = format!("{}", i * ARM32_WORD_SIZE)
                        .parse::<String>()
                        .unwrap();
                    self.emit(str!(R0, [SP, offset]));
                }
                self.emit(pop!(R0, R1));
                self.emit(bl!(func_name));
            }
            3 => {
                self.emit(sub!(SP, SP, #12));
                for i in 0..3 {
                    self.expr(&call.args.get(i).unwrap())?;
                    let offset = format!("{}", i * ARM32_WORD_SIZE)
                        .parse::<String>()
                        .unwrap();
                    self.emit(str!(R0, [SP, offset]));
                }
                self.emit(pop!(R0, R1, R2));
                self.emit(bl!(func_name));
            }
            4 => {
                self.emit(sub!(SP, SP, #16));
                for i in 0..4 {
                    self.expr(&call.args.get(i).unwrap())?;
                    let offset = format!("{}", i * ARM32_WORD_SIZE)
                        .parse::<String>()
                        .unwrap();
                    self.emit(str!(R0, [SP, offset]));
                }
                self.emit(pop!(R0, R1, R2, R3));
                self.emit(bl!(func_name));
            }
            _ => {
                todo!("Implement function calls with more then 4 arguments");
                // self.emit(sub!(SP, SP, #16));
                // for i in 0..4 {
                //     self.expr(&call.args.get(i).unwrap())?;
                //     let offset = format!("{}", i * ARM32_WORD_SIZE)
                //         .parse::<String>()
                //         .unwrap();
                //     self.emit(str!(R0, [SP, offset]));
                // }
                // self.emit(pop!(R0, R1, R2, R3));
                // for i in 4..arg_count {
                //     self.expr(&call.args.get(i).unwrap())?;
                //     let offset = format!("{}", (i - 4) * ARM32_WORD_SIZE)
                //         .parse::<String>()
                //         .unwrap();
                //     self.emit(str!(R0, [SP, offset]));
                // }
                // let call_stack_size = (arg_count - 4) * ARM32_WORD_SIZE;
                // self.emit(bl!(func_name));
                // // Clear stack after call
                // self.emit(add!(SP, SP, #call_stack_size));
            }
        }
        Ok(())
    }

    fn emit(&mut self, ins: Arm32Ins) {
        self.instructions.push(ins);
    }

    fn emit_multiple(&mut self, ins: &mut Vec<Arm32Ins>) {
        self.instructions.append(ins);
    }

    /////////////////////////////////////////////////////////////////
    /// Label generators
    /////////////////////////////////////////////////////////////////
    fn gen_func_label(&self, func_name: &str) -> Arm32Ins {
        Arm32Ins::Label(String::from(func_name))
    }

    fn gen_in_func_label(&mut self, s: &str) -> Arm32Ins {
        self.current_func_env.label_count += 1;
        Arm32Ins::Label(format!(
            ".L_{}__{}_{}",
            self.current_func_env.name, s, self.current_func_env.label_count
        ))
    }
}
