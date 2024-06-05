use self::{
    expr::{Expr, ExprBinaryOp, ExprBooleanOp, ExprCall, ExprCmpOp, ExprUnaryOp},
    stmt::{
        Stmt, StmtAssign, StmtBlock, StmtExpr, StmtFuncDecl, StmtIf, StmtIfElse, StmtReturn,
        StmtVarDecl, StmtWhile,
    },
};

pub mod btype;
pub mod expr;
pub mod op;
pub mod stmt;

pub trait AstWalker<OkT: Default, ErrT: Default> {
    fn walk(&mut self, stmts: Vec<Stmt>) -> Result<OkT, ErrT> {
        for stmt in stmts {
            match stmt {
                Stmt::FuncDecl(stmt_func_decl) => self.stmt_func_decl(&stmt_func_decl),
                _ => unreachable!("Did not expect a non function Stmt at the top level"),
            }?;
        }
        Ok(OkT::default())
    }

    fn stmt(&mut self, stmt: &Stmt) -> Result<OkT, ErrT> {
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

    fn expr(&mut self, expr: &Expr) -> Result<OkT, ErrT> {
        match expr {
            Expr::Bool(b) => self.expr_bool(b.clone()),
            Expr::I64(i) => self.expr_i64(i.clone()),
            Expr::Identifier(name) => self.expr_identifier(name),
            Expr::BinaryOp(expr_binary_op) => self.expr_binary_op(expr_binary_op),
            Expr::BooleanOp(expr_boolean_op) => self.expr_boolean_op(expr_boolean_op),
            Expr::CmpOp(expr_cmp_op) => self.expr_cmp_op(expr_cmp_op),
            Expr::UnaryOp(expr_unary_op) => self.expr_unary_op(expr_unary_op),
            Expr::Call(expr_call) => self.expr_call(expr_call),
        }
    }

    fn stmt_func_decl(&mut self, data: &StmtFuncDecl) -> Result<OkT, ErrT>;
    fn stmt_expr(&mut self, data: &StmtExpr) -> Result<OkT, ErrT> {
        self.expr(&data.expr)
    }
    fn stmt_block(&mut self, stmt_block: &StmtBlock) -> Result<OkT, ErrT>;
    fn stmt_return(&mut self, stmt_return: &StmtReturn) -> Result<OkT, ErrT>;
    fn stmt_if(&mut self, stmt_if: &StmtIf) -> Result<OkT, ErrT>;
    fn stmt_if_else(&mut self, stmt_if_else: &StmtIfElse) -> Result<OkT, ErrT>;
    fn stmt_var_decl(&mut self, stmt_var_decl: &StmtVarDecl) -> Result<OkT, ErrT>;
    fn stmt_assign(&mut self, stmt_assign: &StmtAssign) -> Result<OkT, ErrT>;
    fn stmt_while(&mut self, stmt_while: &StmtWhile) -> Result<OkT, ErrT>;

    fn expr_bool(&mut self, b: bool) -> Result<OkT, ErrT>;
    fn expr_i64(&mut self, i: i64) -> Result<OkT, ErrT>;
    fn expr_identifier(&mut self, name: &str) -> Result<OkT, ErrT>;
    fn expr_binary_op(&mut self, expr_binary_op: &ExprBinaryOp) -> Result<OkT, ErrT>;
    fn expr_boolean_op(&mut self, expr_boolean_op: &ExprBooleanOp) -> Result<OkT, ErrT>;
    fn expr_cmp_op(&mut self, expr_cmp_op: &ExprCmpOp) -> Result<OkT, ErrT>;
    fn expr_unary_op(&mut self, expr_unary_op: &ExprUnaryOp) -> Result<OkT, ErrT>;
    fn expr_call(&mut self, expr_call: &ExprCall) -> Result<OkT, ErrT>;
}
