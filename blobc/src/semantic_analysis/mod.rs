use crate::ast::{stmt::Stmt, AstWalker};

// TODO finish implementing this
pub fn analyze(stmts: &Vec<Stmt>) -> Result<(), ()> {
    let mut analyzer = Analyzer::new(stmts);
    analyzer.analyze();
    match analyzer.errored {
        true => Err(()),
        false => Ok(()),
    }
}

#[derive(Debug, Default)]
enum AnalyzerError {
    Undefined,
}

struct Analyzer<'a> {
    stmts: &'a Vec<Stmt>,
    errored: bool,
}

impl<'a> Analyzer<'a> {
    fn new(stmts: &Vec<Stmt>) -> Analyzer {
        Analyzer {
            stmts,
            errored: false,
        }
    }

    pub fn analyze(&mut self) {
        self.walk(&self.stmts);
    }
}

impl<'a> AstWalker<(), AnalyzerError> for Analyzer<'a> {
    fn func(&mut self, func_decl: &crate::ast::stmt::StmtFuncDecl) -> Result<(), AnalyzerError> {
        todo!()
    }

    fn block_stmt(&mut self, stmts: &Vec<Stmt>) -> Result<(), AnalyzerError> {
        todo!()
    }

    fn expr_stmt(&mut self, expr: &crate::ast::expr::Expr) -> Result<(), AnalyzerError> {
        todo!()
    }

    fn return_stmt(&mut self, expr: &Option<crate::ast::expr::Expr>) -> Result<(), AnalyzerError> {
        todo!()
    }

    fn if_stmt(&mut self, iff: &crate::ast::stmt::StmtIf) -> Result<(), AnalyzerError> {
        todo!()
    }

    fn if_else_stmt(
        &mut self,
        if_else: &crate::ast::stmt::StmtIfElse,
    ) -> Result<(), AnalyzerError> {
        todo!()
    }

    fn var_decl_stmt(
        &mut self,
        var_decl: &crate::ast::stmt::StmtVarDecl,
    ) -> Result<(), AnalyzerError> {
        todo!()
    }

    fn assign_stmt(&mut self, assign: &crate::ast::stmt::StmtAssign) -> Result<(), AnalyzerError> {
        todo!()
    }

    fn while_stmt(&mut self, whilee: &crate::ast::stmt::StmtWhile) -> Result<(), AnalyzerError> {
        todo!()
    }

    fn bool_expr(&mut self, b: bool) -> Result<(), AnalyzerError> {
        todo!()
    }

    fn i32_expr(&mut self, number: &str) -> Result<(), AnalyzerError> {
        todo!()
    }

    fn identifier_expr(&mut self, name: &str) -> Result<(), AnalyzerError> {
        todo!()
    }

    fn unary_expr(
        &mut self,
        unary_op: &crate::ast::expr::ExprUnaryOp,
    ) -> Result<(), AnalyzerError> {
        todo!()
    }

    fn binary_expr(
        &mut self,
        binary_op: &crate::ast::expr::ExprBinaryOp,
    ) -> Result<(), AnalyzerError> {
        todo!()
    }

    fn boolean_expr(
        &mut self,
        boolean_op: &crate::ast::expr::ExprBooleanOp,
    ) -> Result<(), AnalyzerError> {
        todo!()
    }

    fn call(&mut self, call: &crate::ast::expr::ExprCall) -> Result<(), AnalyzerError> {
        todo!()
    }
}
