use crate::ast::{
    btype::BType,
    expr::Expr,
    stmt::{Stmt, StmtExpr},
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

    fn walk(&mut self) -> Result<(), ()> {
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
            Expr::BitwiseOp(expr_bitwise_op) => self.expr_bitwise_op(expr_bitwise_op),
            Expr::BooleanOp(expr_boolean_op) => self.expr_boolean_op(expr_boolean_op),
            Expr::CmpOp(expr_cmp_op) => self.expr_cmp_op(expr_cmp_op),
            Expr::UnaryOp(expr_unary_op) => self.expr_unary_op(expr_unary_op),
            Expr::Call(expr_call) => self.expr_call(expr_call),
        }
    }
    fn stmt_func_decl(
        &mut self,
        stmt_func_decl: &crate::ast::stmt::StmtFuncDecl,
    ) -> Result<BType, AnalyzerError> {
        todo!()
    }

    fn stmt_expr(&mut self, stmt_expr: &StmtExpr) -> Result<BType, AnalyzerError> {
        self.expr(&stmt_expr.expr)
    }

    fn stmt_block(
        &mut self,
        stmt_block: &crate::ast::stmt::StmtBlock,
    ) -> Result<BType, AnalyzerError> {
        todo!()
    }

    fn stmt_return(
        &mut self,
        stmt_return: &crate::ast::stmt::StmtReturn,
    ) -> Result<BType, AnalyzerError> {
        todo!()
    }

    fn stmt_if(&mut self, stmt_if: &crate::ast::stmt::StmtIf) -> Result<BType, AnalyzerError> {
        todo!()
    }

    fn stmt_if_else(
        &mut self,
        stmt_if_else: &crate::ast::stmt::StmtIfElse,
    ) -> Result<BType, AnalyzerError> {
        todo!()
    }

    fn stmt_var_decl(
        &mut self,
        stmt_var_decl: &crate::ast::stmt::StmtVarDecl,
    ) -> Result<BType, AnalyzerError> {
        todo!()
    }

    fn stmt_assign(
        &mut self,
        stmt_assign: &crate::ast::stmt::StmtAssign,
    ) -> Result<BType, AnalyzerError> {
        todo!()
    }

    fn stmt_while(
        &mut self,
        stmt_while: &crate::ast::stmt::StmtWhile,
    ) -> Result<BType, AnalyzerError> {
        todo!()
    }

    fn expr_bool(
        &mut self,
        expr_bool: &crate::ast::expr::ExprBool,
    ) -> Result<BType, AnalyzerError> {
        todo!()
    }

    fn expr_i64(&mut self, expr_i64: &crate::ast::expr::ExprI64) -> Result<BType, AnalyzerError> {
        todo!()
    }

    fn expr_string(
        &mut self,
        expr_string: &crate::ast::expr::ExprString,
    ) -> Result<BType, AnalyzerError> {
        todo!()
    }

    fn expr_identifier(
        &mut self,
        expr_identifier: &crate::ast::expr::ExprIdenifier,
    ) -> Result<BType, AnalyzerError> {
        todo!()
    }

    fn expr_binary_op(
        &mut self,
        expr_binary_op: &crate::ast::expr::ExprBinaryOp,
    ) -> Result<BType, AnalyzerError> {
        todo!()
    }

    fn expr_bitwise_op(
        &mut self,
        expr_bitwise_op: &crate::ast::expr::ExprBitwiseOp,
    ) -> Result<BType, AnalyzerError> {
        todo!()
    }

    fn expr_boolean_op(
        &mut self,
        expr_boolean_op: &crate::ast::expr::ExprBooleanOp,
    ) -> Result<BType, AnalyzerError> {
        todo!()
    }

    fn expr_cmp_op(
        &mut self,
        expr_cmp_op: &crate::ast::expr::ExprCmpOp,
    ) -> Result<BType, AnalyzerError> {
        todo!()
    }

    fn expr_unary_op(
        &mut self,
        expr_unary_op: &crate::ast::expr::ExprUnaryOp,
    ) -> Result<BType, AnalyzerError> {
        todo!()
    }

    fn expr_call(
        &mut self,
        expr_call: &crate::ast::expr::ExprCall,
    ) -> Result<BType, AnalyzerError> {
        todo!()
    }
}

pub fn analyze(ast: &Ast) {
    match Analyzer::new(ast).walk() {
        Err(_) => std::process::exit(1),
        _ => {}
    }
}
