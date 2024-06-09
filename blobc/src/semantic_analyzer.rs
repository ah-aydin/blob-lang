use std::fmt::Display;

use log::{error, info};

use crate::{
    ast::{
        btype::BType,
        expr::{
            Expr, ExprBinaryOp, ExprBool, ExprCall, ExprI64, ExprIdenifier, ExprString, ExprUnaryOp,
        },
        stmt::{
            Stmt, StmtAssign, StmtBlock, StmtExpr, StmtFuncDecl, StmtIf, StmtIfElse, StmtReturn,
            StmtVarDecl, StmtWhile,
        },
        Ast,
    },
    common::FileCoords,
};

#[derive(Debug, Clone, PartialEq, Eq)]
enum AnalyzerError {
    Type(String, FileCoords),
    Undefined(String, FileCoords),
    Tombstone,
}

impl Display for AnalyzerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnalyzerError::Type(msg, file_coords) | AnalyzerError::Undefined(msg, file_coords) => {
                write!(f, "{}:{} {}", file_coords.line, file_coords.col, msg)
            }
            AnalyzerError::Tombstone => unreachable!("Tombstone AnalyzerError cannot be printed"),
        }
    }
}

#[derive(Debug, Clone)]
struct Func {
    ident: String,
    arg_types: Vec<BType>,
    ret_type: BType,
}

impl Func {
    fn new(ident: String, arg_types: Vec<BType>, ret_type: BType) -> Func {
        Func {
            ident,
            arg_types,
            ret_type,
        }
    }
}

#[derive(Debug, Clone)]
struct Var {
    ident: String,
    btype: BType,
}

impl Var {
    fn new(ident: String, btype: BType) -> Var {
        Var { ident, btype }
    }
}

#[derive(Debug, Clone)]
struct Env {
    vars: Vec<Var>,
    funcs: Vec<Func>,
}

impl Env {
    fn new() -> Self {
        Env {
            vars: vec![],
            funcs: vec![],
        }
    }
}

type AnalyzerRetType = Result<BType, AnalyzerError>;

struct Analyzer<'a> {
    ast: &'a Ast,
    envs: Vec<Env>,
    current_func_ret_type: BType,
}

impl<'a> Analyzer<'a> {
    fn new(ast: &Ast) -> Analyzer {
        Analyzer {
            ast,
            envs: vec![Env::new()],
            current_func_ret_type: BType::None,
        }
    }

    fn analyze(&mut self) -> Result<(), ()> {
        for stmt in self.ast {
            let result = match stmt {
                Stmt::FuncDecl(stmt_func_decl) => self.stmt_func_decl(&stmt_func_decl),
                _ => unreachable!("Did not expect a non function Stmt at the top level"),
            };
            if result.is_err() {
                return Err(());
            }
        }
        Ok(())
    }

    fn stmt(&mut self, stmt: &Stmt) -> AnalyzerRetType {
        match stmt {
            Stmt::Expr(stmt_expr) => self.stmt_expr(stmt_expr),
            Stmt::Block(stmt_block) => self.stmt_block(stmt_block),
            Stmt::Return(stmt_return) => self.stmt_return(stmt_return),
            Stmt::If(stmt_if) => self.stmt_if(stmt_if),
            Stmt::IfElse(stmt_if_else) => self.stmt_if_else(stmt_if_else),
            Stmt::VarDecl(stmt_var_decl) => self.stmt_var_decl(stmt_var_decl),
            Stmt::Assign(stmt_assign) => self.stmt_assign(stmt_assign),
            Stmt::While(stmt_while) => self.stmt_while(stmt_while),
            Stmt::FuncDecl(_) => unreachable!("Did not expect a function inside another function"),
        }
    }

    fn expr(&mut self, expr: &Expr) -> AnalyzerRetType {
        match expr {
            Expr::Bool(expr_bool) => self.expr_bool(expr_bool),
            Expr::I64(expr_i64) => self.expr_i64(expr_i64),
            Expr::String(expr_string) => self.expr_string(&expr_string),
            Expr::Identifier(expr_identifier) => self.expr_identifier(expr_identifier),
            Expr::BinaryOp(expr_binary_op) => self.expr_binary_op(expr_binary_op),
            Expr::UnaryOp(expr_unary_op) => self.expr_unary_op(expr_unary_op),
            Expr::Call(expr_call) => self.expr_call(expr_call),
        }
    }

    fn stmt_func_decl(&mut self, stmt_func_decl: &StmtFuncDecl) -> AnalyzerRetType {
        let ret_type = stmt_func_decl.ret_type;
        self.current_func_ret_type = ret_type;
        // Insert func data to the root environment
        (&mut self.envs.get_mut(0).unwrap().funcs).push(Func::new(
            stmt_func_decl.ident.clone(),
            stmt_func_decl.args.iter().map(|arg| arg.btype).collect(),
            ret_type,
        ));

        self.stmt(&stmt_func_decl.body)?;
        Ok(ret_type)
    }

    fn stmt_expr(&mut self, stmt_expr: &StmtExpr) -> AnalyzerRetType {
        self.expr(&stmt_expr.expr)
    }

    fn stmt_block(&mut self, stmt_block: &StmtBlock) -> AnalyzerRetType {
        self.envs.push(Env::new());
        let mut errored = false;
        for stmt in &stmt_block.stmts {
            match self.stmt(stmt) {
                Err(err) => {
                    errored = true;
                    println!("[ERROR] {}", err);
                }

                _ => {}
            }
        }
        self.envs.pop();
        if errored {
            return Err(AnalyzerError::Tombstone);
        }
        Ok(BType::None)
    }

    fn stmt_return(&mut self, stmt_return: &StmtReturn) -> AnalyzerRetType {
        let expr_maybe = &stmt_return.expr;
        let expr_type = match expr_maybe {
            Some(expr) => self.expr(expr)?,
            None => BType::None,
        };
        if expr_type != self.current_func_ret_type {
            return Err(AnalyzerError::Type(
                format!(
                    "Expected return type '{:?}' by got '{:?}'",
                    self.current_func_ret_type, expr_type
                ),
                expr_maybe.as_ref().unwrap().get_file_coords(),
            ));
        }
        Ok(expr_type)
    }

    fn stmt_if(&mut self, stmt_if: &StmtIf) -> AnalyzerRetType {
        todo!()
    }

    fn stmt_if_else(&mut self, stmt_if_else: &StmtIfElse) -> AnalyzerRetType {
        todo!()
    }

    fn stmt_var_decl(&mut self, stmt_var_decl: &StmtVarDecl) -> AnalyzerRetType {
        todo!()
    }

    fn stmt_assign(&mut self, stmt_assign: &StmtAssign) -> AnalyzerRetType {
        todo!()
    }

    fn stmt_while(&mut self, stmt_while: &StmtWhile) -> AnalyzerRetType {
        todo!()
    }

    fn expr_bool(&mut self, _expr_bool: &ExprBool) -> AnalyzerRetType {
        Ok(BType::Bool)
    }

    fn expr_i64(&mut self, _expr_i64: &ExprI64) -> AnalyzerRetType {
        Ok(BType::I64)
    }

    fn expr_string(&mut self, _expr_string: &ExprString) -> AnalyzerRetType {
        Ok(BType::Str)
    }

    fn expr_identifier(&mut self, expr_identifier: &ExprIdenifier) -> AnalyzerRetType {
        todo!()
    }

    fn expr_binary_op(&mut self, expr_binary_op: &ExprBinaryOp) -> AnalyzerRetType {
        todo!()
    }

    fn expr_unary_op(&mut self, expr_unary_op: &ExprUnaryOp) -> AnalyzerRetType {
        todo!()
    }

    fn expr_call(&mut self, expr_call: &ExprCall) -> AnalyzerRetType {
        todo!()
    }
}

pub fn analyze(ast: &Ast) {
    info!("Analyzing semantics...");
    match Analyzer::new(ast).analyze() {
        Err(_) => {
            error!("Anaysis failed!");
            std::process::exit(1);
        }
        _ => {
            info!("Analysis complete!");
        }
    }
}
