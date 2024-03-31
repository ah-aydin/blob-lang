mod symbols;

use derive_new::new;

use crate::ast::{
    blob_type::BlobType,
    expr::{Expr, ExprBinaryOp, ExprBooleanOp, ExprCall, ExprUnaryOp},
    stmt::{
        Stmt, StmtAssign, StmtExpr, StmtFuncDecl, StmtIf, StmtIfElse, StmtReturn, StmtVarDecl,
        StmtWhile,
    },
    AstWalker,
};

use self::symbols::{FuncData, VarData};

pub fn analyze(stmts: &Vec<Stmt>) -> Result<(), ()> {
    let mut analyzer = Analyzer::new(stmts);
    analyzer.analyze();
    match analyzer.errored {
        true => Err(()),
        false => Ok(()),
    }
}

struct Scope {
    vars: Vec<VarData>,
}

impl Scope {
    fn new(vars: Vec<VarData>) -> Scope {
        Scope { vars }
    }

    fn add_var(&mut self, var_data: VarData) -> bool {
        if self
            .vars
            .iter()
            .find(|vd| vd.name == var_data.name)
            .is_some()
        {
            return false;
        }
        self.vars.push(var_data);
        true
    }
}

#[derive(Debug, Default)]
enum AnalyzerError {
    #[default]
    Undefined,
    TypeMismatch,
}

struct Analyzer<'a> {
    stmts: &'a Vec<Stmt>,
    funcs: Vec<FuncData>,
    scopes: Vec<Scope>,
    errored: bool,
    current_func_ret_type: BlobType,
}

impl<'a> Analyzer<'a> {
    fn new(stmts: &Vec<Stmt>) -> Analyzer {
        let funcs = stmts
            .iter()
            .map(|stmt| match stmt {
                Stmt::FuncDecl(func_decl) => FuncData::new(func_decl),
                _ => unreachable!("Got unexpected global Stmt {:?}", stmt),
            })
            .collect();
        Analyzer {
            stmts,
            funcs,
            scopes: vec![],
            errored: false,
            current_func_ret_type: BlobType::None,
        }
    }

    pub fn analyze(&mut self) {
        for func in &self.funcs {
            let mut args: Vec<String> = vec![];
            for arg_data in &func.args {
                if args.contains(&arg_data.name) {
                    eprintln!(
                        "[ERROR] Line {}: function argument '{}' is already defined",
                        func.line, arg_data.name
                    );
                    self.errored = true;
                    break;
                }
                args.push(arg_data.name.clone());
            }
        }

        // let _ = self.walk(&self.stmts);
    }
}

impl<'a> AstWalker<BlobType, AnalyzerError> for Analyzer<'a> {
    fn func(&mut self, func_decl: &StmtFuncDecl) -> Result<BlobType, AnalyzerError> {
        self.scopes
            .push(Scope::new(VarData::from_func_decl_args(&func_decl.args)));
        self.current_func_ret_type = func_decl.return_type.clone().unwrap_or(BlobType::None);
        self.stmt(&func_decl.body)?;
        self.scopes.pop();
        Ok(BlobType::None)
    }

    fn block_stmt(&mut self, stmts: &Vec<Stmt>) -> Result<BlobType, AnalyzerError> {
        self.scopes.push(Scope::new(vec![]));
        for stmt in stmts {
            self.stmt(stmt)?;
        }
        self.scopes.pop();
        Ok(BlobType::None)
    }

    fn expr_stmt(&mut self, expr: &StmtExpr) -> Result<BlobType, AnalyzerError> {
        self.expr(&expr.expr)
    }

    fn return_stmt(&mut self, returnn: &StmtReturn) -> Result<BlobType, AnalyzerError> {
        let expr_type = match &returnn.expr {
            Some(e) => self.expr(e)?,
            None => BlobType::None,
        };

        if expr_type != self.current_func_ret_type {
            eprintln!(
                "Expected return type '{:?}' but got '{:?}'",
                self.current_func_ret_type, expr_type
            );
            self.errored = true;
            return Err(AnalyzerError::TypeMismatch);
        }

        Ok(expr_type)
    }

    fn if_stmt(&mut self, iff: &StmtIf) -> Result<BlobType, AnalyzerError> {
        todo!("Implement semantic analysis for if stmt")
    }

    fn if_else_stmt(&mut self, if_else: &StmtIfElse) -> Result<BlobType, AnalyzerError> {
        todo!("Implement semantic analysis for if else stmt")
    }

    fn var_decl_stmt(&mut self, var_decl: &StmtVarDecl) -> Result<BlobType, AnalyzerError> {
        todo!("Implement semantic analysis for var decl stmt")
    }

    fn assign_stmt(&mut self, assign: &StmtAssign) -> Result<BlobType, AnalyzerError> {
        todo!("Implement semantic analysis for assign stmt")
    }

    fn while_stmt(&mut self, whilee: &StmtWhile) -> Result<BlobType, AnalyzerError> {
        todo!("Implement semantic analysis for while stmt")
    }

    fn bool_expr(&mut self, b: bool) -> Result<BlobType, AnalyzerError> {
        todo!("Implement semantic analysis for bool expr")
    }

    fn i32_expr(&mut self, number: &str) -> Result<BlobType, AnalyzerError> {
        todo!("Implement semantic analysis for i32 expr")
    }

    fn identifier_expr(&mut self, name: &str) -> Result<BlobType, AnalyzerError> {
        todo!("Implement semantic analysis for identifier expr")
    }

    fn unary_expr(&mut self, unary_op: &ExprUnaryOp) -> Result<BlobType, AnalyzerError> {
        todo!("Implement semantic analysis for unary expr")
    }

    fn binary_expr(&mut self, binary_op: &ExprBinaryOp) -> Result<BlobType, AnalyzerError> {
        todo!("Implement semantic analysis for binary expr")
    }

    fn boolean_expr(&mut self, boolean_op: &ExprBooleanOp) -> Result<BlobType, AnalyzerError> {
        todo!("Implement semantic analysis for boolean expr")
    }

    fn call(&mut self, call: &ExprCall) -> Result<BlobType, AnalyzerError> {
        todo!("Implement semantic analysis for call expr")
    }
}
