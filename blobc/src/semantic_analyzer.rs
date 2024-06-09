use std::fmt::Display;

use log::{error, info, warn};

use crate::{
    ast::{
        btype::BType,
        expr::{
            Expr, ExprBinaryOp, ExprBool, ExprCall, ExprI64, ExprIdenifier, ExprString, ExprUnaryOp,
        },
        op::OpBTypes,
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
    Defined(String, FileCoords),
    Undefined(String, FileCoords),
    Tombstone,
}

impl Display for AnalyzerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnalyzerError::Type(msg, file_coords)
            | AnalyzerError::Undefined(msg, file_coords)
            | AnalyzerError::Defined(msg, file_coords) => {
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
                    error!("{}", err);
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
        let condition_btype = self.expr(&stmt_if.condition)?;
        if condition_btype != BType::Bool {
            return Err(AnalyzerError::Type(
                format!(
                    "Expected if condition to have a 'Bool' result but got '{:?}'",
                    condition_btype
                ),
                stmt_if.condition.get_file_coords(),
            ));
        }
        self.stmt(&stmt_if.body)?;
        Ok(BType::None)
    }

    fn stmt_if_else(&mut self, stmt_if_else: &StmtIfElse) -> AnalyzerRetType {
        let condition_btype = self.expr(&stmt_if_else.condition)?;
        if condition_btype != BType::Bool {
            return Err(AnalyzerError::Type(
                format!(
                    "Expected if condition to have a 'Bool' result but got '{:?}'",
                    condition_btype
                ),
                stmt_if_else.condition.get_file_coords(),
            ));
        }
        self.stmt(&stmt_if_else.if_body)?;
        self.stmt(&stmt_if_else.else_body)?;
        Ok(BType::None)
    }

    fn stmt_var_decl(&mut self, stmt_var_decl: &StmtVarDecl) -> AnalyzerRetType {
        if self
            .envs
            .last()
            .unwrap()
            .vars
            .iter()
            .any(|var| var.ident == stmt_var_decl.ident)
        {
            return Err(AnalyzerError::Defined(
                format!("Var '{}' is already defined", stmt_var_decl.ident),
                stmt_var_decl.expr.get_file_coords(),
            ));
        }

        let var_btype = stmt_var_decl.btype;
        let expr_btype = self.expr(&stmt_var_decl.expr)?;

        if var_btype != BType::None && var_btype != expr_btype {
            return Err(AnalyzerError::Type(
                format!(
                    "Var has type '{:?}' but right expression has '{:?}'",
                    var_btype, expr_btype
                ),
                stmt_var_decl.expr.get_file_coords(),
            ));
        }

        (&mut self.envs.last_mut().unwrap().vars).push(Var {
            ident: stmt_var_decl.ident.clone(),
            btype: expr_btype,
        });

        Ok(BType::None)
    }

    fn stmt_assign(&mut self, stmt_assign: &StmtAssign) -> AnalyzerRetType {
        todo!("stmt_assign");
        Ok(BType::None)
    }

    fn stmt_while(&mut self, stmt_while: &StmtWhile) -> AnalyzerRetType {
        let condition_btype = self.expr(&stmt_while.condition)?;
        if condition_btype != BType::Bool {
            return Err(AnalyzerError::Type(
                format!(
                    "Expected while condition to have a 'Bool' result but got '{:?}'",
                    condition_btype
                ),
                stmt_while.condition.get_file_coords(),
            ));
        }
        self.stmt(&stmt_while.body)?;
        Ok(BType::None)
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
        let ident = &expr_identifier.ident;
        for env in self.envs.iter().rev() {
            let var_maybe = env.vars.iter().filter(|var| var.ident == *ident).last();
            if let Some(var) = var_maybe {
                return Ok(var.btype);
            }
        }

        return Err(AnalyzerError::Undefined(
            format!("'{:?}' is undefined", ident),
            expr_identifier.file_coords,
        ));
    }

    fn expr_binary_op(&mut self, expr_binary_op: &ExprBinaryOp) -> AnalyzerRetType {
        let left_btype = self.expr(&expr_binary_op.left)?;
        let right_btype = self.expr(&expr_binary_op.right)?;

        let supported_btypes = expr_binary_op.op.get_supported_btypes();
        if !supported_btypes.contains(&left_btype) {
            return Err(AnalyzerError::Type(
                format!(
                    "Op '{:?}' expects '{:?}' types but it got '{:?}' in the left term",
                    expr_binary_op.op, supported_btypes, left_btype
                ),
                expr_binary_op.file_coords,
            ));
        }
        if !supported_btypes.contains(&right_btype) {
            return Err(AnalyzerError::Type(
                format!(
                    "Op '{:?}' expects '{:?}' types but it got '{:?}' in the right term",
                    expr_binary_op.op, supported_btypes, right_btype
                ),
                expr_binary_op.file_coords,
            ));
        }

        if left_btype != right_btype {
            return Err(AnalyzerError::Type(
                format!(
                    "Mismatched types '{:?}' and '{:?}'",
                    left_btype, right_btype
                ),
                expr_binary_op.file_coords,
            ));
        }

        Ok(expr_binary_op.op.get_result_btype())
    }

    fn expr_unary_op(&mut self, expr_unary_op: &ExprUnaryOp) -> AnalyzerRetType {
        let btype = self.expr(&expr_unary_op.term)?;

        let supported_btypes = expr_unary_op.op.get_supported_btypes();
        if !supported_btypes.contains(&btype) {
            return Err(AnalyzerError::Type(
                format!(
                    "Op '{:?}' expects '{:?}' types but it got '{:?}'",
                    expr_unary_op.op, supported_btypes, btype
                ),
                expr_unary_op.file_coords,
            ));
        }

        Ok(expr_unary_op.op.get_result_btype())
    }

    fn expr_call(&mut self, expr_call: &ExprCall) -> AnalyzerRetType {
        todo!("expr_call")
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
