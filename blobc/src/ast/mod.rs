use derive_new::new;

use self::{
    expr::{Expr, ExprBinaryOp, ExprBooleanOp, ExprCall, ExprUnaryOp},
    stmt::{
        Stmt, StmtAssign, StmtExpr, StmtFuncDecl, StmtIf, StmtIfElse, StmtReturn, StmtVarDecl,
        StmtWhile,
    },
};

pub mod blob_type;
pub mod expr;
pub mod op_type;
pub mod stmt;

#[derive(Debug, Clone, PartialEq, Eq, new)]
pub struct FileCoords {
    pub line: usize,
    pub col: usize,
}

pub trait AstWalker<T: Default, E: Default> {
    fn walk(&mut self, stmts: &Vec<Stmt>) -> Result<T, E> {
        for stmt in stmts {
            let _ = match stmt {
                Stmt::FuncDecl(stmt_func_decl) => self.func(&stmt_func_decl)?,
                _ => unreachable!("Got unexpected global statement {:?}", stmt),
            };
        }
        Ok(T::default())
    }

    fn stmt(&mut self, stmt: &Stmt) -> Result<T, E> {
        match stmt {
            Stmt::Block(stmts) => self.block_stmt(stmts),
            Stmt::Expr(expr) => self.expr_stmt(expr),
            Stmt::Return(returnn) => self.return_stmt(returnn),
            Stmt::If(iff) => self.if_stmt(iff),
            Stmt::IfElse(if_else) => self.if_else_stmt(if_else),
            Stmt::VarDecl(var_decl) => self.var_decl_stmt(var_decl),
            Stmt::Assign(assign) => self.assign_stmt(assign),
            Stmt::While(whilee) => self.while_stmt(whilee),
            Stmt::FuncDecl(_) => {
                unreachable!("Did not expect a function decleration inside a block")
            }
        }
    }

    fn expr(&mut self, expr: &Expr) -> Result<T, E> {
        match expr {
            Expr::Bool(b) => self.bool_expr(b.clone()),
            Expr::Number(number) => self.i32_expr(number),
            Expr::Identifier(name) => self.identifier_expr(name),
            Expr::UnaryOp(unary_op) => self.unary_expr(unary_op),
            Expr::BinaryOp(binary_op) => self.binary_expr(binary_op),
            Expr::BooleanOp(boolean_op) => self.boolean_expr(boolean_op),
            Expr::Call(call) => self.call(call),
        }
    }

    fn func(&mut self, func_decl: &StmtFuncDecl) -> Result<T, E>;
    fn block_stmt(&mut self, stmts: &Vec<Stmt>) -> Result<T, E>;
    fn expr_stmt(&mut self, expr: &StmtExpr) -> Result<T, E>;
    fn return_stmt(&mut self, expr: &StmtReturn) -> Result<T, E>;
    fn if_stmt(&mut self, iff: &StmtIf) -> Result<T, E>;
    fn if_else_stmt(&mut self, if_else: &StmtIfElse) -> Result<T, E>;
    fn var_decl_stmt(&mut self, var_decl: &StmtVarDecl) -> Result<T, E>;
    fn assign_stmt(&mut self, assign: &StmtAssign) -> Result<T, E>;
    fn while_stmt(&mut self, whilee: &StmtWhile) -> Result<T, E>;
    fn bool_expr(&mut self, b: bool) -> Result<T, E>;
    fn i32_expr(&mut self, number: &str) -> Result<T, E>;
    fn identifier_expr(&mut self, name: &str) -> Result<T, E>;
    fn unary_expr(&mut self, unary_op: &ExprUnaryOp) -> Result<T, E>;
    fn binary_expr(&mut self, binary_op: &ExprBinaryOp) -> Result<T, E>;
    fn boolean_expr(&mut self, boolean_op: &ExprBooleanOp) -> Result<T, E>;
    fn call(&mut self, call: &ExprCall) -> Result<T, E>;
}
