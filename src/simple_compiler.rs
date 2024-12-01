use std::{fs::File, io::Write};

use crate::{
    ast::{
        expr::{Expr, ExprBinaryOp, ExprBool, ExprI64},
        op::BinaryOp,
        stmt::{Stmt, StmtBlock, StmtExpr, StmtFuncDecl, StmtReturn},
        Ast,
    },
    error, info,
};

pub fn compile(ast: &Ast, contains_main: bool, file_name: String) {
    let mut compiler = Compiler::new(&ast);
    compiler.compile();
    let mut instructions = compiler.instructions.join("\n");

    if contains_main {
        instructions = "
section .text
\tglobal _start
"
        .to_owned()
            + &instructions;
        instructions = instructions
            + "
_start:
\tcall main
\tmov rdi, rax
\tmov rax,60
\tsyscall
";
    } else {
        todo!("Implement compilation without main");
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
}

impl<'a> Compiler<'a> {
    fn new(ast: &Ast) -> Compiler {
        Compiler {
            ast,
            instructions: vec![],
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
            Stmt::If(stmt_if) => todo!(),
            Stmt::IfElse(stmt_if_else) => todo!(),
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
            Expr::UnaryOp(expr_unary_op) => todo!(),
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

    fn expr_bool(&mut self, expr_bool: &ExprBool) {
        let value;
        if expr_bool.value {
            value = 1;
        } else {
            value = 0;
        }
        self.add_instruction(&format!("mova r8, {}", value));
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
            BinaryOp::BooleanOr => todo!(),
            BinaryOp::BooleanAnd => todo!(),
            BinaryOp::BitwiseOr => todo!(),
            BinaryOp::BitwiseAnd => todo!(),
            BinaryOp::Eq => todo!(),
            BinaryOp::Neq => todo!(),
            BinaryOp::Gt => todo!(),
            BinaryOp::Gte => todo!(),
            BinaryOp::Lt => todo!(),
            BinaryOp::Lte => todo!(),
        }
    }

    fn add_label(&mut self, label: &str) {
        self.instructions.push(format!("{}:", label));
    }

    fn add_instruction(&mut self, instruction: &str) {
        self.instructions.push(format!("\t{}", instruction));
    }
}
