mod symbols;

use crate::ast::{
    blob_type::BlobType,
    expr::{ExprBinaryOp, ExprBooleanOp, ExprCall, ExprUnaryOp},
    op_type::UnaryOpType,
    stmt::{
        Stmt, StmtAssign, StmtExpr, StmtFuncDecl, StmtIf, StmtIfElse, StmtReturn, StmtVarDecl,
        StmtWhile,
    },
    AstWalker,
};

use self::symbols::{FuncData, VarData};

// TODO add checks for unused variables and functions
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

    fn contains_var(&self, name: &str) -> bool {
        self.vars.iter().find(|var| var.name == name).is_some()
    }

    fn get_var_data(&self, name: &str) -> Option<BlobType> {
        self.vars
            .iter()
            .find(|var| var.name == name)
            .map(|var| var.blob_type.clone())
    }
}

#[derive(Debug, Default)]
enum AnalyzerError {
    ArgumentMismatch,
    TypeMismatch,
    #[default]
    Undefined,
    UndeclaredVariable,
    UndefinedFunction,
    UnsupportedOpration,
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
        // Check function arguments
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

        let _ = self.walk(&self.stmts);
    }

    fn scope_contains_var(&self, name: &str) -> bool {
        self.scopes.last().unwrap().contains_var(name)
    }

    fn get_var_type(&self, name: &str) -> Option<BlobType> {
        for scope in self.scopes.iter().rev() {
            let var_type = scope.get_var_data(name);
            if var_type.is_some() {
                return var_type;
            }
        }
        None
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
                "[ERROR] Line {}: Expected return type '{:?}' but got '{:?}'",
                returnn.line, self.current_func_ret_type, expr_type
            );
            self.errored = true;
            return Err(AnalyzerError::TypeMismatch);
        }

        Ok(expr_type)
    }

    fn if_stmt(&mut self, iff: &StmtIf) -> Result<BlobType, AnalyzerError> {
        let condition_type = self.expr(&iff.condition)?;
        if condition_type != BlobType::Bool {
            eprintln!(
                "[ERROR] Line {}: Expected boolean expression for if condition but got '{:?}'",
                iff.line, condition_type
            );
            self.errored = true;
            return Err(AnalyzerError::TypeMismatch);
        }
        self.stmt(&iff.clause)?;
        Ok(BlobType::None)
    }

    fn if_else_stmt(&mut self, if_else: &StmtIfElse) -> Result<BlobType, AnalyzerError> {
        let condition_type = self.expr(&if_else.condition)?;
        if condition_type != BlobType::Bool {
            eprintln!(
                "[ERROR] Line {}: Expected boolean expression for if condition but got '{:?}'",
                if_else.line, condition_type
            );
            self.errored = true;
            return Err(AnalyzerError::TypeMismatch);
        }
        self.stmt(&if_else.if_clause)?;
        self.stmt(&if_else.else_clause)?;
        Ok(BlobType::None)
    }

    fn var_decl_stmt(&mut self, var_decl: &StmtVarDecl) -> Result<BlobType, AnalyzerError> {
        if self.scope_contains_var(&var_decl.name) {
            eprintln!(
                "[ERROR] Line {}: Variable '{}' is allready defined in this scope",
                var_decl.line, var_decl.name
            );
            self.errored = true;
            return Err(AnalyzerError::TypeMismatch);
        }

        let expr_type = self.expr(&var_decl.to)?;
        let var_type = var_decl.blob_type.clone().unwrap_or(expr_type.clone());
        if var_type != expr_type {
            eprintln!(
                "[ERROR] Line {}: Variable '{:?}' has type '{:?}' but got '{:?}'",
                var_decl.line, var_decl.name, var_type, expr_type
            );
            self.errored = true;
            return Err(AnalyzerError::TypeMismatch);
        }
        self.scopes
            .last_mut()
            .unwrap()
            .add_var(VarData::new(var_decl.name.clone(), expr_type.clone()));
        Ok(expr_type)
    }

    fn assign_stmt(&mut self, assign: &StmtAssign) -> Result<BlobType, AnalyzerError> {
        let var_data = self.get_var_type(&assign.name);
        if var_data.is_none() {
            eprintln!(
                "[ERROR] Line {}: Variable '{:?}' is not defined.",
                assign.line, assign.name
            );
            self.errored = true;
            return Err(AnalyzerError::TypeMismatch);
        }
        let var_type = var_data.unwrap();

        let expr_type = self.expr(&assign.to)?;
        if var_type != expr_type {
            eprintln!(
                "[ERROR] Line {}: Variable '{:?}' has type '{:?}' but got '{:?}'",
                assign.line, assign.name, var_type, expr_type
            );
            self.errored = true;
            return Err(AnalyzerError::TypeMismatch);
        }
        Ok(expr_type)
    }

    fn while_stmt(&mut self, whilee: &StmtWhile) -> Result<BlobType, AnalyzerError> {
        let condition_type = self.expr(&whilee.condition)?;
        if condition_type != BlobType::Bool {
            eprintln!(
                "[ERROR] Line {}: Expected boolean expression for if condition but got '{:?}'",
                whilee.line, condition_type
            );
            self.errored = true;
            return Err(AnalyzerError::TypeMismatch);
        }
        self.stmt(&whilee.body)?;
        Ok(BlobType::None)
    }

    fn bool_expr(&mut self, _booll: bool) -> Result<BlobType, AnalyzerError> {
        Ok(BlobType::Bool)
    }

    fn i32_expr(&mut self, _number: &str) -> Result<BlobType, AnalyzerError> {
        Ok(BlobType::I32)
    }

    fn identifier_expr(&mut self, name: &str) -> Result<BlobType, AnalyzerError> {
        match self.get_var_type(name) {
            Some(t) => Ok(t),
            None => {
                eprintln!("[ERROR] Variable with name '{:?}' is not declated", name);
                self.errored = true;
                Err(AnalyzerError::UndeclaredVariable)
            }
        }
    }

    fn unary_expr(&mut self, unary_op: &ExprUnaryOp) -> Result<BlobType, AnalyzerError> {
        let expr_type = self.expr(&unary_op.term)?;
        match (&unary_op.op_type, expr_type.clone()) {
            (UnaryOpType::Negate, BlobType::I32) | (UnaryOpType::Not, BlobType::Bool) => {
                Ok(expr_type)
            }
            (op_type, blob_type) => {
                eprintln!(
                    "[ERROR] Line {}: Cannot apply operation '{:?}' on type '{:?}'",
                    unary_op.line, op_type, blob_type
                );
                self.errored = true;
                Err(AnalyzerError::UnsupportedOpration)
            }
        }
    }

    fn binary_expr(&mut self, binary_op: &ExprBinaryOp) -> Result<BlobType, AnalyzerError> {
        let left_type = self.expr(&binary_op.left_term)?;
        let right_type = self.expr(&binary_op.right_term)?;

        if left_type != right_type {
            eprintln!("[ERROR] Line {}: Cannot apply operation '{:?}' on mismatched types '{:?}' and '{:?}'",
                      binary_op.line, binary_op.op_type, left_type, right_type);
            self.errored = true;
            return Err(AnalyzerError::TypeMismatch);
        }
        if left_type != BlobType::I32 {
            eprintln!(
                "[ERROR] Line {}: Cannot apply operation '{:?}' on type '{:?}'",
                binary_op.line, binary_op.op_type, left_type
            );
            self.errored = true;
            return Err(AnalyzerError::UnsupportedOpration);
        }

        Ok(left_type)
    }

    fn boolean_expr(&mut self, boolean_op: &ExprBooleanOp) -> Result<BlobType, AnalyzerError> {
        let left_type = self.expr(&boolean_op.left_term)?;
        let right_type = self.expr(&boolean_op.right_term)?;

        if left_type != right_type {
            eprintln!("[ERROR] Line {}: Cannot apply operation '{:?}' on mismatched types '{:?}' and '{:?}'",
                      boolean_op.line, boolean_op.op_type, left_type, right_type);
            self.errored = true;
            return Err(AnalyzerError::TypeMismatch);
        }
        if boolean_op.op_type.is_boolean_specific() && left_type != BlobType::Bool {
            eprintln!(
                "[ERROR] Line {}: Cannot apply operation '{:?}' on type '{:?}'",
                boolean_op.line, boolean_op.op_type, left_type
            );
            self.errored = true;
            return Err(AnalyzerError::UnsupportedOpration);
        }

        Ok(BlobType::Bool)
    }

    fn call(&mut self, call: &ExprCall) -> Result<BlobType, AnalyzerError> {
        let func = self.funcs.iter().find(|func| func.name == call.name);
        if func.is_none() {
            eprintln!(
                "[ERROR] Line {}: Function '{:?}' is not defined",
                call.line, call.name
            );
            self.errored = true;
            return Err(AnalyzerError::UndefinedFunction);
        }

        let args = func.unwrap().args.clone();
        let return_type = func.unwrap().return_type.clone();

        if args.len() != call.args.len() {
            eprintln!(
                "[ERROR] Line {}: Function '{:?}' takes {} arguments but {} where given",
                call.line,
                call.name,
                args.len(),
                call.args.len()
            );
            self.errored = true;
            return Err(AnalyzerError::ArgumentMismatch);
        }
        for i in 0..args.len() {
            let expected_type = args.get(i).unwrap().blob_type.clone();
            let expr_type = self.expr(&call.args.get(i).unwrap())?;
            if expected_type != expr_type {
                eprintln!(
                    "[ERROR] Line {}: Arugment '{}' is of type '{:?}' but expression of type '{:?}' was given",
                    call.line,
                    args.get(i).unwrap().name,
                    expected_type,
                    expr_type
                );
                self.errored = true;
                return Err(AnalyzerError::ArgumentMismatch);
            }
        }
        Ok(return_type.unwrap_or(BlobType::None))
    }
}
