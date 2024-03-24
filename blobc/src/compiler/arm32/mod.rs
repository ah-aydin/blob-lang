#[macro_use]
mod assembly;

use self::assembly::Arm32Ins;
use super::common::CompileError;
use crate::ast::{
    blob_type::BlobType,
    expr::{Expr, ExprBinaryOp, ExprBooleanOp, ExprCall, ExprUnaryOp},
    op_type::{BinaryOpType, BooleanOpType, UnaryOpType},
    stmt::{Stmt, StmtAssign, StmtFuncDecl, StmtIf, StmtIfElse, StmtVarDecl, StmtWhile},
    TreeWalker,
};
use std::{fs::File, io::Write, process::Command};

const WORD_SIZE: usize = 4;

type CompilerResult = Result<(), CompileError>;

#[derive(Debug)]
struct ScopeEnv {
    scope_stack_start: i32,
    vars: Vec<(String, BlobType)>,
    has_dead_stack_space: bool,
}

impl ScopeEnv {
    fn new(prev_scope_stack_size: i32) -> ScopeEnv {
        ScopeEnv {
            scope_stack_start: prev_scope_stack_size,
            vars: Vec::new(),
            has_dead_stack_space: false,
        }
    }

    fn get_scope_size(&self) -> i32 {
        let size = (self.vars.len() * WORD_SIZE) as i32;
        if self.has_dead_stack_space {
            return size + WORD_SIZE as i32;
        }
        size
    }

    fn add_var(&mut self, var: (String, BlobType)) {
        self.vars.push(var)
    }

    fn get_var_offset(&self, var_name: &str) -> Option<i32> {
        let index = self.vars.iter().position(|(name, _)| name == var_name)?;
        Some(((index + 1) * WORD_SIZE) as i32 + self.scope_stack_start)
    }

    fn flip_dead_stack_space(&mut self) {
        self.has_dead_stack_space = !self.has_dead_stack_space;
    }
}

#[derive(Default, Debug)]
struct FuncEnv {
    name: String,
    label_count: usize,
    arg_count: usize,
    args: Vec<(String, BlobType)>,
    scopes: Vec<ScopeEnv>,
}

impl FuncEnv {
    /// Returns the offset from the FP (frame pointer) of the variable
    fn get_var_offset(&self, param_name: &str) -> i32 {
        let index = self.args.iter().position(|(name, _)| name == param_name);

        if index.is_none() {
            return self.get_non_param_offset(param_name);
        }
        let index = index.unwrap();

        match self.arg_count {
            0 => unreachable!(),
            1 | 2 => match index {
                0 => -2 * WORD_SIZE as i32,
                1 => -1 * WORD_SIZE as i32,
                _ => unreachable!(),
            },
            3 | 4 => match index {
                0 => -4 * WORD_SIZE as i32,
                1 => -3 * WORD_SIZE as i32,
                2 => -2 * WORD_SIZE as i32,
                3 => -1 * WORD_SIZE as i32,
                _ => unreachable!(),
            },
            _ => match index {
                0 => -4 * WORD_SIZE as i32,
                1 => -3 * WORD_SIZE as i32,
                2 => -2 * WORD_SIZE as i32,
                3 => -1 * WORD_SIZE as i32,
                index => (WORD_SIZE * 2 + (index - 4) * WORD_SIZE) as i32,
            },
        }
    }

    fn get_non_param_offset(&self, param_name: &str) -> i32 {
        let args_stack_size = match self.arg_count {
            0 => 0,
            1 | 2 => 2 * WORD_SIZE as i32,
            _ => 4 * WORD_SIZE as i32,
        };
        for scope in self.scopes.iter().rev() {
            if let Some(offset) = scope.get_var_offset(param_name) {
                return -(offset + args_stack_size);
            }
        }
        unreachable!("'{}' does not exist in the local scope", param_name)
    }
}

pub struct Arm32Compiler {
    instructions: Vec<Arm32Ins>,
    current_func_env: FuncEnv,
}

impl Arm32Compiler {
    pub fn new() -> Arm32Compiler {
        Arm32Compiler {
            instructions: vec![],
            current_func_env: FuncEnv::default(),
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
            "x86" | "x86_64" => "arm-linux-gnueabihf-gcc -static -mcpu=cortex-a7",
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

    fn get_current_func_env(&mut self) -> &mut ScopeEnv {
        self.current_func_env.scopes.last_mut().unwrap()
    }

    pub fn compile(&mut self, stmts: Vec<Stmt>, file_name: &str) -> CompilerResult {
        self.reset();

        for stmt in &stmts {
            self.current_func_env.label_count = 0;
            let _ = match stmt {
                Stmt::FuncDecl(stmt_func_decl) => self.func(&stmt_func_decl)?,
                _ => unreachable!("Got unexpected global statement"),
            };
        }

        self.link(file_name)
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

impl TreeWalker<(), CompileError> for Arm32Compiler {
    fn func(&mut self, func_decl: &StmtFuncDecl) -> CompilerResult {
        let func_name = func_decl.name.as_str();
        // Setup environment
        self.current_func_env.name = String::from(func_name);
        self.current_func_env.arg_count = func_decl.args.len();
        self.current_func_env.args = func_decl.args.clone();
        self.current_func_env.scopes.clear();
        self.emit(self.gen_func_label(func_name));

        // Prologue
        self.emit(push!(FP, LR));
        self.emit(mov!(FP, SP));
        match func_decl.args.len() {
            0 => {}
            1 | 2 => self.emit(push!(R0, R1)),
            _ => self.emit(push!(R0, R1, R2, R3)),
        }

        self.stmt(&func_decl.body)?;

        Ok(())
    }

    fn block_stmt(&mut self, stmts: &Vec<Stmt>) -> CompilerResult {
        let prev_scope_size = match self.current_func_env.scopes.last() {
            Some(scope) => scope.get_scope_size() + scope.scope_stack_start,
            None => 0,
        };
        self.current_func_env
            .scopes
            .push(ScopeEnv::new(prev_scope_size));
        for stmt in stmts {
            self.stmt(stmt)?;
        }
        let scope_stack_size = self.current_func_env.scopes.pop().unwrap().get_scope_size();
        self.emit(add!(SP, SP, #scope_stack_size));
        Ok(())
    }

    fn expr_stmt(&mut self, expr: &Expr) -> CompilerResult {
        self.expr(expr)?;
        Ok(())
    }

    fn return_stmt(&mut self, expr: &Option<Expr>) -> CompilerResult {
        if expr.is_some() {
            self.expr(expr.as_ref().unwrap())?;
        }
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

    fn var_decl_stmt(&mut self, var_decl: &StmtVarDecl) -> CompilerResult {
        self.expr(&var_decl.to)?;

        let var = (var_decl.name.clone(), var_decl.blob_type.clone().unwrap());
        self.get_current_func_env().add_var(var);
        if self.get_current_func_env().has_dead_stack_space {
            self.emit(str!(R0, [SP, 0]));
        } else {
            self.emit(sub!(SP, SP, #8));
            self.emit(str!(R0, [SP, 4]));
        }
        self.get_current_func_env().flip_dead_stack_space();
        Ok(())
    }

    fn assign_stmt(&mut self, assign: &StmtAssign) -> CompilerResult {
        self.expr(&assign.to)?;
        let offset = self.current_func_env.get_var_offset(&assign.name);
        self.emit(str!(R0, [FP, offset]));
        Ok(())
    }

    fn while_stmt(&mut self, whilee: &StmtWhile) -> CompilerResult {
        let start_label = self.gen_in_func_label("whileStart");
        let end_label = self.gen_in_func_label("whileEnd");

        self.emit(start_label.clone());
        self.expr(&whilee.condition)?;
        self.emit_multiple(&mut vec![cmp!(R0, #0), b!(end_label.get_label(), Eq)]);
        self.stmt(&whilee.body)?;
        self.emit(b!(start_label.get_label()));
        self.emit(end_label);
        Ok(())
    }

    fn bool_expr(&mut self, b: bool) -> CompilerResult {
        if b {
            let one = "1";
            self.emit(ldr!(R0, one));
        } else {
            let zero = "0";
            self.emit(ldr!(R0, zero));
        }
        Ok(())
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
            BinaryOpType::Divide => self.emit(sdiv!(R0, R1, R0)),
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
                    let offset = format!("{}", i * WORD_SIZE).parse::<String>().unwrap();
                    self.emit(str!(R0, [SP, offset]));
                }
                self.emit(pop!(R0, R1));
                self.emit(bl!(func_name));
            }
            3 => {
                self.emit(sub!(SP, SP, #12));
                for i in 0..3 {
                    self.expr(&call.args.get(i).unwrap())?;
                    let offset = format!("{}", i * WORD_SIZE).parse::<String>().unwrap();
                    self.emit(str!(R0, [SP, offset]));
                }
                self.emit(pop!(R0, R1, R2));
                self.emit(bl!(func_name));
            }
            4 => {
                self.emit(sub!(SP, SP, #16));
                for i in 0..4 {
                    self.expr(&call.args.get(i).unwrap())?;
                    let offset = format!("{}", i * WORD_SIZE).parse::<String>().unwrap();
                    self.emit(str!(R0, [SP, offset]));
                }
                self.emit(pop!(R0, R1, R2, R3));
                self.emit(bl!(func_name));
            }
            arg_count => {
                // Make space on the stack for the extra arguments
                let extra_args_stack_size = match arg_count % 2 == 0 {
                    true => (arg_count - 4) * WORD_SIZE,
                    // Keep 8-byte stack allignment
                    false => (arg_count - 4) * WORD_SIZE + WORD_SIZE,
                };
                self.emit(sub!(SP, SP, #extra_args_stack_size));
                for i in 4..arg_count {
                    self.expr(&call.args.get(i).unwrap())?;
                    let offset = format!("{}", (i - 4) * WORD_SIZE)
                        .parse::<String>()
                        .unwrap();
                    self.emit(str!(R0, [SP, offset]));
                }

                // Emit first 4 args
                self.emit(sub!(SP, SP, #16));
                for i in 0..4 {
                    self.expr(&call.args.get(i).unwrap())?;
                    let offset = format!("{}", i * WORD_SIZE).parse::<String>().unwrap();
                    self.emit(str!(R0, [SP, offset]));
                }
                self.emit(pop!(R0, R1, R2, R3));

                self.emit(bl!(func_name));
                // Clear stack for extra arguments
                self.emit(add!(SP, SP, #extra_args_stack_size));
            }
        }
        Ok(())
    }
}
