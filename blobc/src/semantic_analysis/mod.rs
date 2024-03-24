use crate::ast::{
    expr::{Expr, ExprBinaryOp, ExprBooleanOp, ExprCall, ExprUnaryOp},
    stmt::{Stmt, StmtAssign, StmtFuncDecl, StmtIf, StmtIfElse, StmtVarDecl, StmtWhile},
    AstWalker,
};

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
    #[default]
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
        // let _ = self.walk(&self.stmts);
    }
}

impl<'a> AstWalker<(), AnalyzerError> for Analyzer<'a> {
    fn func(&mut self, func_decl: &StmtFuncDecl) -> Result<(), AnalyzerError> {
        todo!("Implement semantic analysis for func")
    }

    fn block_stmt(&mut self, stmts: &Vec<Stmt>) -> Result<(), AnalyzerError> {
        todo!("Implement semantic analysis for block")
    }

    fn expr_stmt(&mut self, expr: &Expr) -> Result<(), AnalyzerError> {
        todo!("Implement semantic analysis for expr stmt")
    }

    fn return_stmt(&mut self, expr: &Option<Expr>) -> Result<(), AnalyzerError> {
        todo!("Implement semantic analysis for return stmt")
    }

    fn if_stmt(&mut self, iff: &StmtIf) -> Result<(), AnalyzerError> {
        todo!("Implement semantic analysis for if stmt")
    }

    fn if_else_stmt(&mut self, if_else: &StmtIfElse) -> Result<(), AnalyzerError> {
        todo!("Implement semantic analysis for if else stmt")
    }

    fn var_decl_stmt(&mut self, var_decl: &StmtVarDecl) -> Result<(), AnalyzerError> {
        todo!("Implement semantic analysis for var decl stmt")
    }

    fn assign_stmt(&mut self, assign: &StmtAssign) -> Result<(), AnalyzerError> {
        todo!("Implement semantic analysis for assign stmt")
    }

    fn while_stmt(&mut self, whilee: &StmtWhile) -> Result<(), AnalyzerError> {
        todo!("Implement semantic analysis for while stmt")
    }

    fn bool_expr(&mut self, b: bool) -> Result<(), AnalyzerError> {
        todo!("Implement semantic analysis for bool expr")
    }

    fn i32_expr(&mut self, number: &str) -> Result<(), AnalyzerError> {
        todo!("Implement semantic analysis for i32 expr")
    }

    fn identifier_expr(&mut self, name: &str) -> Result<(), AnalyzerError> {
        todo!("Implement semantic analysis for identifier expr")
    }

    fn unary_expr(&mut self, unary_op: &ExprUnaryOp) -> Result<(), AnalyzerError> {
        todo!("Implement semantic analysis for unary expr")
    }

    fn binary_expr(&mut self, binary_op: &ExprBinaryOp) -> Result<(), AnalyzerError> {
        todo!("Implement semantic analysis for binary expr")
    }

    fn boolean_expr(&mut self, boolean_op: &ExprBooleanOp) -> Result<(), AnalyzerError> {
        todo!("Implement semantic analysis for boolean expr")
    }

    fn call(&mut self, call: &ExprCall) -> Result<(), AnalyzerError> {
        todo!("Implement semantic analysis for call expr")
    }
}
