use std::{collections::HashMap, fmt::Display};

use log::{error, info, warn};

use crate::{
    ast::{
        btype::BType,
        expr::{
            Expr, ExprBinaryOp, ExprBool, ExprCall, ExprI32, ExprIdenifier, ExprString,
            ExprStructInstance, ExprUnaryOp,
        },
        op::OpBTypes,
        stmt::{
            Stmt, StmtAssign, StmtBlock, StmtExpr, StmtFuncDecl, StmtIf, StmtIfElse, StmtReturn,
            StmtStructDecl, StmtVarDecl, StmtWhile, VarTypeInfo,
        },
        Ast,
    },
    common::FileCoords,
};

#[derive(Debug, Clone, PartialEq, Eq)]
enum AnalyzerError {
    ErrorFC(String, FileCoords),
    Error(String),
    Tombstone,
}

impl Display for AnalyzerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnalyzerError::ErrorFC(msg, file_coords) => {
                write!(f, "{}:{} {}", file_coords.line, file_coords.col, msg)
            }
            AnalyzerError::Error(msg) => {
                write!(f, "{}", msg)
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
struct Struct {
    ident: String,
    fields: Vec<VarTypeInfo>,
}

impl Struct {
    fn new(ident: String, fields: Vec<VarTypeInfo>) -> Struct {
        Struct { ident, fields }
    }
}

#[derive(Debug, Clone)]
struct Var {
    ident: String,
    btype: BType,
}

impl Var {
    fn from_var_type_info(var_type_info: &VarTypeInfo) -> Var {
        Var {
            ident: var_type_info.ident.clone(),
            btype: var_type_info.btype.clone(),
        }
    }
}

#[derive(Debug, Clone)]
struct Env {
    funcs: Vec<Func>,
    structs: Vec<Struct>,
    vars: Vec<VarTypeInfo>,
}

impl Env {
    fn new() -> Env {
        Env {
            funcs: vec![],
            structs: vec![],
            vars: vec![],
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
                Stmt::StructDecl(stmt_struct_decl) => self.stmt_struct_decl(&stmt_struct_decl),
                _ => unreachable!("Did not expect a non function Stmt at the top level"),
            };
            if result.is_err() {
                error!("{}", result.err().unwrap());
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
            Stmt::FuncDecl(_) => unreachable!(
                "Did not expect a function declaration inside another function or struct"
            ),
            Stmt::StructDecl(_) => unreachable!(
                "Did not expect a struct declaration inside another function or struct"
            ),
        }
    }

    fn expr(&mut self, expr: &Expr) -> AnalyzerRetType {
        match expr {
            Expr::Bool(expr_bool) => self.expr_bool(expr_bool),
            Expr::I32(expr_i32) => self.expr_i32(expr_i32),
            Expr::String(expr_string) => self.expr_string(&expr_string),
            Expr::Identifier(expr_identifier) => self.expr_identifier(expr_identifier),
            Expr::BinaryOp(expr_binary_op) => self.expr_binary_op(expr_binary_op),
            Expr::UnaryOp(expr_unary_op) => self.expr_unary_op(expr_unary_op),
            Expr::Call(expr_call) => self.expr_call(expr_call),
            Expr::StructInstance(expr_struct_instance) => {
                self.expr_struct_instance(&expr_struct_instance)
            }
        }
    }

    fn stmt_func_decl(&mut self, stmt_func_decl: &StmtFuncDecl) -> AnalyzerRetType {
        let ret_type = stmt_func_decl.ret_type.clone();
        self.current_func_ret_type = ret_type.clone();
        if self
            .envs
            .get(0)
            .unwrap()
            .funcs
            .iter()
            .any(|func_data| func_data.ident == stmt_func_decl.ident)
        {
            return Err(AnalyzerError::Error(format!(
                "Function '{}' is already defined",
                stmt_func_decl.ident
            )));
        }
        // Insert func data to the root environment
        (&mut self.envs.get_mut(0).unwrap().funcs).push(Func::new(
            stmt_func_decl.ident.clone(),
            stmt_func_decl
                .args
                .iter()
                .map(|arg| arg.btype.clone())
                .collect(),
            ret_type.clone(),
        ));

        let mut func_env = Env::new();
        func_env.vars = stmt_func_decl.args.clone();

        self.envs.push(func_env);
        self.stmt(&stmt_func_decl.body)?;
        self.envs.pop();

        if self.current_func_ret_type != BType::None {
            self.check_last_ret_stmt(&stmt_func_decl.body)?;
        }

        Ok(ret_type)
    }

    fn stmt_struct_decl(&mut self, stmt_struct_decl: &StmtStructDecl) -> AnalyzerRetType {
        if self
            .envs
            .get(0)
            .unwrap()
            .structs
            .iter()
            .any(|sturct_data| sturct_data.ident == stmt_struct_decl.ident)
        {
            return Err(AnalyzerError::Error(format!(
                "Struct '{}' is already defined",
                stmt_struct_decl.ident
            )));
        }
        // Insert func data to the root environment
        (&mut self.envs.get_mut(0).unwrap().structs).push(Struct::new(
            stmt_struct_decl.ident.clone(),
            stmt_struct_decl.fields.clone(),
        ));

        Ok(BType::None)
    }

    fn check_last_ret_stmt(&mut self, stmt: &Stmt) -> Result<(), AnalyzerError> {
        match stmt {
            Stmt::Block(stmt_block) => match stmt_block.stmts.last() {
                Some(last_stmt) => self.check_last_ret_stmt(last_stmt),
                None => Err(AnalyzerError::Error(format!(
                    "Missing return statement on required function"
                ))),
            },
            Stmt::Return(_) => Ok(()),
            Stmt::If(stmt_if) => Err(AnalyzerError::ErrorFC(
                "Not all paths leads to a return statement".to_string(),
                stmt_if.condition.get_file_coords(),
            )),
            Stmt::IfElse(if_else_stmt) => {
                if self.check_last_ret_stmt(&if_else_stmt.if_body).is_ok()
                    && self.check_last_ret_stmt(&if_else_stmt.else_body).is_ok()
                {
                    Ok(())
                } else {
                    Err(AnalyzerError::ErrorFC(
                        "Both branches should have a return statement".to_string(),
                        if_else_stmt.condition.get_file_coords(),
                    ))
                }
            }
            Stmt::VarDecl(stmt_var_decl) => Err(AnalyzerError::ErrorFC(
                "Last statment must be for return".to_string(),
                stmt_var_decl.expr.get_file_coords(),
            )),
            Stmt::Assign(stmt_assign) => Err(AnalyzerError::ErrorFC(
                "Last statment must be for return".to_string(),
                stmt_assign.expr.get_file_coords(),
            )),
            Stmt::While(stmt_while) => Err(AnalyzerError::ErrorFC(
                "Last statment must be for return".to_string(),
                stmt_while.condition.get_file_coords(),
            )),
            Stmt::Expr(_) => unreachable!("Shouldn't be processing expressions here"),
            Stmt::FuncDecl(_) => unreachable!(
                "Did not expect a function declaration inside another function or struct"
            ),
            Stmt::StructDecl(_) => unreachable!(
                "Did not expect a struct declaration inside another function or struct"
            ),
        }
    }

    fn stmt_expr(&mut self, stmt_expr: &StmtExpr) -> AnalyzerRetType {
        self.expr(&stmt_expr.expr)
    }

    fn stmt_block(&mut self, stmt_block: &StmtBlock) -> AnalyzerRetType {
        self.envs.push(Env::new());
        let mut errored = false;
        let mut error = AnalyzerError::Tombstone;
        for stmt in &stmt_block.stmts {
            match self.stmt(stmt) {
                Err(err) => {
                    errored = true;
                    error = err;
                    error!("{}", error);
                }

                _ => {}
            }
        }
        self.envs.pop();
        if errored {
            return Err(error);
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
            return Err(AnalyzerError::ErrorFC(
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
            return Err(AnalyzerError::ErrorFC(
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
            return Err(AnalyzerError::ErrorFC(
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
            return Err(AnalyzerError::ErrorFC(
                format!("Var '{}' is already defined", stmt_var_decl.ident),
                stmt_var_decl.expr.get_file_coords(),
            ));
        }

        let var_btype = &stmt_var_decl.btype;
        let expr_btype = self.expr(&stmt_var_decl.expr)?;

        if *var_btype != BType::None && *var_btype != expr_btype {
            return Err(AnalyzerError::ErrorFC(
                format!(
                    "Var has type '{:?}' but right expression has '{:?}'",
                    var_btype, expr_btype
                ),
                stmt_var_decl.expr.get_file_coords(),
            ));
        }

        (&mut self.envs.last_mut().unwrap().vars).push(VarTypeInfo {
            ident: stmt_var_decl.ident.clone(),
            btype: expr_btype,
        });

        Ok(BType::None)
    }

    fn stmt_assign(&mut self, stmt_assign: &StmtAssign) -> AnalyzerRetType {
        let ident = &stmt_assign.ident;
        let mut found = false;
        let mut var_btype = BType::None;
        for env in self.envs.iter().rev() {
            let var_maybe = env.vars.iter().filter(|var| var.ident == *ident).last();
            if let Some(var) = var_maybe {
                found = true;
                var_btype = var.btype.clone();
                break;
            }
        }

        if !found {
            return Err(AnalyzerError::ErrorFC(
                format!("{:?} is undefined", ident),
                stmt_assign.expr.get_file_coords(),
            ));
        }

        let expr_btype = self.expr(&stmt_assign.expr)?;
        if var_btype != expr_btype {
            return Err(AnalyzerError::ErrorFC(
                format!(
                    "Variable {} has type '{:?}' but '{:?}' was given",
                    ident, var_btype, expr_btype
                ),
                stmt_assign.expr.get_file_coords(),
            ));
        }

        Ok(BType::None)
    }

    fn stmt_while(&mut self, stmt_while: &StmtWhile) -> AnalyzerRetType {
        let condition_btype = self.expr(&stmt_while.condition)?;
        if condition_btype != BType::Bool {
            return Err(AnalyzerError::ErrorFC(
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

    fn expr_i32(&mut self, _expr_i32: &ExprI32) -> AnalyzerRetType {
        Ok(BType::I32)
    }

    fn expr_string(&mut self, _expr_string: &ExprString) -> AnalyzerRetType {
        Ok(BType::Str)
    }

    fn expr_identifier(&mut self, expr_identifier: &ExprIdenifier) -> AnalyzerRetType {
        let ident = &expr_identifier.ident;
        for env in self.envs.iter().rev() {
            let var_maybe = env.vars.iter().filter(|var| var.ident == *ident).last();
            if let Some(var) = var_maybe {
                return Ok(var.btype.clone());
            }
        }

        Err(AnalyzerError::ErrorFC(
            format!("'{:?}' is undefined", ident),
            expr_identifier.file_coords,
        ))
    }

    fn expr_binary_op(&mut self, expr_binary_op: &ExprBinaryOp) -> AnalyzerRetType {
        let left_btype = self.expr(&expr_binary_op.left)?;
        let right_btype = self.expr(&expr_binary_op.right)?;

        let supported_btypes = expr_binary_op.op.get_supported_btypes();
        if !supported_btypes.contains(&left_btype) {
            return Err(AnalyzerError::ErrorFC(
                format!(
                    "Op '{:?}' expects '{:?}' types but it got '{:?}' in the left term",
                    expr_binary_op.op, supported_btypes, left_btype
                ),
                expr_binary_op.file_coords,
            ));
        }
        if !supported_btypes.contains(&right_btype) {
            return Err(AnalyzerError::ErrorFC(
                format!(
                    "Op '{:?}' expects '{:?}' types but it got '{:?}' in the right term",
                    expr_binary_op.op, supported_btypes, right_btype
                ),
                expr_binary_op.file_coords,
            ));
        }

        if left_btype != right_btype {
            return Err(AnalyzerError::ErrorFC(
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
            return Err(AnalyzerError::ErrorFC(
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
        let func_data = self
            .envs
            .get(0)
            .unwrap()
            .funcs
            .iter()
            .find(|func| func.ident == expr_call.name);
        if func_data.is_none() {
            return Err(AnalyzerError::ErrorFC(
                format!("Function '{}' is not defined", expr_call.name),
                expr_call.file_coords,
            ));
        }
        let func_data = func_data.unwrap().clone();

        if func_data.arg_types.len() != expr_call.args.len() {
            return Err(AnalyzerError::ErrorFC(
                format!(
                    "Function '{} 'takes {} arguments but {} where given",
                    func_data.ident,
                    func_data.arg_types.len(),
                    expr_call.args.len()
                ),
                expr_call.file_coords,
            ));
        }

        for (i, (expected_btype, expr)) in func_data
            .arg_types
            .iter()
            .zip(expr_call.args.iter())
            .enumerate()
        {
            let expr_btype = self.expr(expr)?;
            if expr_btype != *expected_btype {
                return Err(AnalyzerError::ErrorFC(
                    format!(
                        "Function arg {} expecetd type {:?} but got {:?}",
                        i + 1,
                        expected_btype,
                        expr_btype
                    ),
                    expr_call.file_coords,
                ));
            }
        }

        Ok(func_data.ret_type)
    }

    fn expr_struct_instance(
        &mut self,
        expr_struct_instance: &ExprStructInstance,
    ) -> AnalyzerRetType {
        let struct_data = self
            .envs
            .get(0)
            .unwrap()
            .structs
            .iter()
            .find(|func| func.ident == expr_struct_instance.ident);
        if struct_data.is_none() {
            return Err(AnalyzerError::ErrorFC(
                format!("Function '{}' is not defined", expr_struct_instance.ident),
                expr_struct_instance.file_coords,
            ));
        }
        let struct_data = struct_data.unwrap().clone();

        if struct_data.fields.len() != expr_struct_instance.fields.len() {
            return Err(AnalyzerError::ErrorFC(
                format!(
                    "Struct '{} 'takes {} arguments but {} where given",
                    struct_data.ident,
                    struct_data.fields.len(),
                    expr_struct_instance.fields.len()
                ),
                expr_struct_instance.file_coords,
            ));
        }

        let mut missing_args: Vec<String> = Vec::with_capacity(0);
        let mut mismatched_types: HashMap<String, (BType, BType)> = HashMap::with_capacity(0);
        for field in struct_data.fields {
            match expr_struct_instance.fields.get(&field.ident) {
                Some(expr) => {
                    let expr_btype = self.expr(expr)?;
                    if expr_btype != field.btype {
                        mismatched_types.insert(
                            field.ident.clone(),
                            (field.btype.clone(), expr_btype.clone()),
                        );
                    }
                }
                None => missing_args.push(field.ident.clone()),
            }
        }

        if !missing_args.is_empty() {
            return Err(AnalyzerError::ErrorFC(
                format!(
                    "Missing fields in sturct '{}': {}",
                    struct_data.ident,
                    missing_args.join(", ")
                ),
                expr_struct_instance.file_coords,
            ));
        }
        if !mismatched_types.is_empty() {
            return Err(AnalyzerError::ErrorFC(
                format!(
                    "Mismached fields in sturct '{}': {}",
                    struct_data.ident,
                    mismatched_types
                        .iter()
                        .map(|entry| format!(
                            "field '{}' is of type '{:?}' but got '{:?}'",
                            entry.0, entry.1 .0, entry.1 .1
                        ))
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                expr_struct_instance.file_coords,
            ));
        }

        Ok(BType::Struct(expr_struct_instance.ident.clone()))
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
