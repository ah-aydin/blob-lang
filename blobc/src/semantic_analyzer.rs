use crate::ast::{
    btype::BType,
    expr::{
        Expr, ExprBinaryOp, ExprBool, ExprCall, ExprCmpOp, ExprI64, ExprIdenifier, ExprString,
        ExprUnaryOp,
    },
    stmt::{
        Stmt, StmtAssign, StmtBlock, StmtExpr, StmtFuncDecl, StmtIf, StmtIfElse, StmtReturn,
        StmtVarDecl, StmtWhile,
    },
    Ast,
};

#[derive(Debug, Clone, PartialEq, Eq)]
enum AnalyzerError {}

struct Analyzer<'a> {
    ast: &'a Ast,
}

impl<'a> Analyzer<'a> {
    fn new(ast: &Ast) -> Analyzer {
        Analyzer { ast }
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

    fn stmt(&mut self, stmt: &Stmt) -> Result<BType, AnalyzerError> {
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

    fn expr(&mut self, expr: &Expr) -> Result<BType, AnalyzerError> {
        match expr {
            Expr::Bool(expr_bool) => self.expr_bool(expr_bool),
            Expr::I64(expr_i64) => self.expr_i64(expr_i64),
            Expr::String(expr_string) => self.expr_string(&expr_string),
            Expr::Identifier(expr_identifier) => self.expr_identifier(expr_identifier),
            Expr::BinaryOp(expr_binary_op) => self.expr_binary_op(expr_binary_op),
            Expr::CmpOp(expr_cmp_op) => self.expr_cmp_op(expr_cmp_op),
            Expr::UnaryOp(expr_unary_op) => self.expr_unary_op(expr_unary_op),
            Expr::Call(expr_call) => self.expr_call(expr_call),
        }
    }
    fn stmt_func_decl(&mut self, stmt_func_decl: &StmtFuncDecl) -> Result<BType, AnalyzerError> {
        todo!()
    }

    fn stmt_expr(&mut self, stmt_expr: &StmtExpr) -> Result<BType, AnalyzerError> {
        self.expr(&stmt_expr.expr)
    }

    fn stmt_block(&mut self, stmt_block: &StmtBlock) -> Result<BType, AnalyzerError> {
        todo!()
    }

    fn stmt_return(&mut self, stmt_return: &StmtReturn) -> Result<BType, AnalyzerError> {
        todo!()
    }

    fn stmt_if(&mut self, stmt_if: &StmtIf) -> Result<BType, AnalyzerError> {
        todo!()
    }

    fn stmt_if_else(&mut self, stmt_if_else: &StmtIfElse) -> Result<BType, AnalyzerError> {
        todo!()
    }

    fn stmt_var_decl(&mut self, stmt_var_decl: &StmtVarDecl) -> Result<BType, AnalyzerError> {
        todo!()
    }

    fn stmt_assign(&mut self, stmt_assign: &StmtAssign) -> Result<BType, AnalyzerError> {
        todo!()
    }

    fn stmt_while(&mut self, stmt_while: &StmtWhile) -> Result<BType, AnalyzerError> {
        todo!()
    }

    fn expr_bool(&mut self, expr_bool: &ExprBool) -> Result<BType, AnalyzerError> {
        todo!()
    }

    fn expr_i64(&mut self, expr_i64: &ExprI64) -> Result<BType, AnalyzerError> {
        todo!()
    }

    fn expr_string(&mut self, expr_string: &ExprString) -> Result<BType, AnalyzerError> {
        todo!()
    }

    fn expr_identifier(&mut self, expr_identifier: &ExprIdenifier) -> Result<BType, AnalyzerError> {
        todo!()
    }

    fn expr_binary_op(&mut self, expr_binary_op: &ExprBinaryOp) -> Result<BType, AnalyzerError> {
        todo!()
    }

    fn expr_cmp_op(&mut self, expr_cmp_op: &ExprCmpOp) -> Result<BType, AnalyzerError> {
        todo!()
    }

    fn expr_unary_op(&mut self, expr_unary_op: &ExprUnaryOp) -> Result<BType, AnalyzerError> {
        todo!()
    }

    fn expr_call(&mut self, expr_call: &ExprCall) -> Result<BType, AnalyzerError> {
        todo!()
    }
}

pub fn analyze(ast: &Ast) {
    match Analyzer::new(ast).analyze() {
        Err(_) => std::process::exit(1),
        _ => {}
    }
}
