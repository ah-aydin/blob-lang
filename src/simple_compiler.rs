use std::{fs::File, io::Write};

use crate::{
    ast::{
        expr::{Expr, ExprBinaryOp, ExprBool, ExprI64, ExprUnaryOp},
        op::{BinaryOp, UnaryOp},
        stmt::{Stmt, StmtBlock, StmtExpr, StmtFuncDecl, StmtIf, StmtIfElse, StmtReturn},
        Ast,
    },
    error, info,
};

pub fn compile(ast: &Ast, contains_main: bool, file_name: String) {
    let mut compiler = Compiler::new(&ast);
    compiler.compile();
    let mut instructions = compiler.instructions.join("\n");

    if contains_main {
        instructions = "section .text
\tglobal _start
"
        .to_owned()
            + &instructions;
        instructions = instructions
            + "
_start:
\tcall main
\tmov rdi, rax
\tmov rax, 60
\tsyscall
";
    } else {
        instructions = "
section .text
"
        .to_owned()
            + &instructions;
    }

    let mut file = match File::create(&file_name) {
        Ok(file) => file,
        Err(_) => {
            error!("Failed to create file '{}'", file_name);
            std::process::exit(1);
        }
    };
    let _ = file.write_all(instructions.as_bytes());
    info!("Compiled into assembly at '{}'", file_name);
}

struct Compiler<'a> {
    ast: &'a Ast,
    instructions: Vec<String>,
    label_counter: u64,
}

impl<'a> Compiler<'a> {
    fn new(ast: &Ast) -> Compiler {
        Compiler {
            ast,
            instructions: vec![],
            label_counter: 0,
        }
    }

    fn compile(&mut self) {
        for stmt in self.ast {
            match stmt {
                Stmt::FuncDecl(stmt_func_decl) => self.stmt_func_decl(&stmt_func_decl),
                Stmt::StructDecl(stmt_struct_decl) => todo!(),
                _ => unreachable!("Did not expect a non function Stmt at the top level"),
            };
        }
    }

    fn stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(stmt_expr) => self.stmt_expr(stmt_expr),
            Stmt::Block(stmt_block) => self.stmt_block(stmt_block),
            Stmt::Return(stmt_return) => self.stmt_return(stmt_return),
            Stmt::If(stmt_if) => self.stmt_if(stmt_if),
            Stmt::IfElse(stmt_if_else) => self.stmt_if_else(stmt_if_else),
            Stmt::VarDecl(stmt_var_decl) => todo!(),
            Stmt::Assign(stmt_assign) => todo!(),
            Stmt::While(stmt_while) => todo!(),
            Stmt::FuncDecl(_) => unreachable!(
                "Did not expect a function declaration inside another function or struct"
            ),
            Stmt::StructDecl(_) => unreachable!(
                "Did not expect a struct declaration inside another function or struct"
            ),
        }
    }

    fn expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Bool(expr_bool) => self.expr_bool(expr_bool),
            Expr::I64(expr_i64) => self.expr_i64(expr_i64),
            Expr::String(expr_string) => todo!(),
            Expr::Identifier(expr_identifier) => todo!(),
            Expr::BinaryOp(expr_binary_op) => self.expr_binary_op(expr_binary_op),
            Expr::UnaryOp(expr_unary_op) => self.expr_unary_op(expr_unary_op),
            Expr::Call(expr_call) => todo!(),
            Expr::StructInstance(expr_struct_instance) => todo!(),
            Expr::Get(expr_get_property) => todo!(),
        }
    }

    fn stmt_func_decl(&mut self, stmt_func_decl: &StmtFuncDecl) {
        self.add_label(&stmt_func_decl.ident);
        self.stmt(&stmt_func_decl.body);
    }

    fn stmt_expr(&mut self, stmt_expr: &StmtExpr) {
        self.expr(&stmt_expr.expr);
    }

    fn stmt_block(&mut self, stmt_block: &StmtBlock) {
        for stmt in &stmt_block.stmts {
            self.stmt(stmt);
        }
    }

    fn stmt_return(&mut self, stmt_return: &StmtReturn) {
        if let Some(expr) = &stmt_return.expr {
            self.expr(expr);
            self.add_instruction("mov rax, r8");
        }
        self.add_instruction("ret");
    }

    fn stmt_if(&mut self, stmt_if: &StmtIf) {
        let if_end_label = self.generate_label("ifEnd");

        self.expr(&stmt_if.condition);
        self.add_instruction("cmp r8, 1");
        self.add_instruction(&format!("jne {if_end_label}"));
        self.stmt(&stmt_if.body);
        self.add_label(&if_end_label);
    }

    fn stmt_if_else(&mut self, stmt_if_else: &StmtIfElse) {
        let if_end_else_start_label = self.generate_label("ifEndElseStart");
        let if_else_end_label = self.generate_label("ifElseEnd");

        self.expr(&stmt_if_else.condition);
        self.add_instruction("cmp r8, 1");
        self.add_instruction(&format!("jne {if_end_else_start_label}"));
        self.stmt(&stmt_if_else.if_body);
        self.add_instruction(&format!("jmp {if_else_end_label}"));
        self.add_label(&if_end_else_start_label);
        self.stmt(&stmt_if_else.else_body);
        self.add_label(&if_else_end_label);
    }

    fn expr_bool(&mut self, expr_bool: &ExprBool) {
        let value;
        if expr_bool.value {
            value = 1;
        } else {
            value = 0;
        }
        self.add_instruction(&format!("mov r8, {}", value));
    }

    fn expr_i64(&mut self, expr_i64: &ExprI64) {
        self.add_instruction(&format!("mov r8, {}", expr_i64.value));
    }

    fn expr_binary_op(&mut self, expr_binary_op: &ExprBinaryOp) {
        self.expr(&expr_binary_op.left);
        self.add_instruction("push r8");
        self.expr(&expr_binary_op.right);
        self.add_instruction("pop r9");
        // r9 will have the left operant, r8 will have the right operand
        match expr_binary_op.op {
            BinaryOp::Add => self.add_instruction("add r8, r9"),
            BinaryOp::Sub => {
                self.add_instruction("sub r8, r9");
                self.add_instruction("neg r8");
            }
            BinaryOp::Mul => self.add_instruction("imul r8, r9"),
            BinaryOp::Div => {
                self.add_instruction("mov rax, r9");
                self.add_instruction("idiv r8");
                self.add_instruction("mov r8, rax");
            }
            BinaryOp::BooleanOr => self.add_instruction("or r8, r9"),
            BinaryOp::BooleanAnd => self.add_instruction("and r8, r9"),
            BinaryOp::BitwiseOr => self.add_instruction("or r8, r9"),
            BinaryOp::BitwiseAnd => self.add_instruction("and r8, r9"),
            BinaryOp::Eq => {
                self.add_instruction("cmp r9, r8");
                self.add_instruction("sete al");
                self.add_instruction("movzx r8, al");
            }
            BinaryOp::Neq => {
                self.add_instruction("cmp r9, r8");
                self.add_instruction("setne al");
                self.add_instruction("movzx r8, al");
            }
            BinaryOp::Gt => {
                self.add_instruction("cmp r9, r8");
                self.add_instruction("setg al");
                self.add_instruction("movzx r8, al");
            }
            BinaryOp::Gte => {
                self.add_instruction("cmp r9, r8");
                self.add_instruction("setge al");
                self.add_instruction("movzx r8, al");
            }
            BinaryOp::Lt => {
                self.add_instruction("cmp r9, r8");
                self.add_instruction("setl al");
                self.add_instruction("movzx r8, al");
            }
            BinaryOp::Lte => {
                self.add_instruction("cmp r9, r8");
                self.add_instruction("setle al");
                self.add_instruction("movzx r8, al");
            }
        }
    }

    fn expr_unary_op(&mut self, expr_unary_op: &ExprUnaryOp) {
        // r8 will contain the operand
        match expr_unary_op.op {
            UnaryOp::Neg => {
                self.expr(&expr_unary_op.term);
                self.add_instruction("neg r8");
            }
            UnaryOp::Not => {
                self.expr(&expr_unary_op.term);
                self.add_instruction("not r8");
            }
        }
    }

    fn add_label(&mut self, label: &str) {
        self.instructions.push(format!("{}:", label));
    }

    fn generate_label(&mut self, prefix: &str) -> String {
        let label = format!("_{}_{}", prefix, self.label_counter);
        self.label_counter += 1;
        label
    }

    fn add_instruction(&mut self, instruction: &str) {
        self.instructions.push(format!("\t{}", instruction));
    }
}
