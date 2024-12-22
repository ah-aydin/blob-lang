use std::{collections::HashMap, fmt::Display};

use crate::ast::btype::{BType, BTypeWrapper};
use crate::ast::expr::{ExprDeref, ExprRef};
use crate::declartions::{Declarations, StructDecl};
use crate::file_coords::FileCoords;
use crate::{error, info};

use crate::ast::{
    expr::{
        Expr, ExprBinaryOp, ExprBool, ExprCall, ExprGet, ExprI64, ExprIdenifier,
        ExprStructInstance, ExprUnaryOp,
    },
    op::OpBTypes,
    stmt::{
        Stmt, StmtAssign, StmtBlock, StmtExpr, StmtFuncDecl, StmtIf, StmtIfElse, StmtReturn,
        StmtStructDecl, StmtVarDecl, StmtWhile, VarTypeInfo,
    },
    Ast,
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
struct Env {
    vars: Vec<VarTypeInfo>,
}

impl Env {
    fn new() -> Env {
        Env { vars: vec![] }
    }
}

type AnalyzerRetType = Result<BTypeWrapper, AnalyzerError>;

struct Analyzer<'a> {
    ast: &'a Ast,
    declarations: &'a Declarations,
    envs: Vec<Env>,
    current_func_ret_type_wrapper: BTypeWrapper,
    contains_main: bool,
}

impl<'a> Analyzer<'a> {
    fn new(ast: &'a Ast, declarations: &'a Declarations) -> Analyzer<'a> {
        Analyzer {
            ast,
            declarations,
            envs: vec![],
            current_func_ret_type_wrapper: BTypeWrapper::void(),
            contains_main: false,
        }
    }

    fn analyze(&mut self) -> Result<bool, ()> {
        for stmt in self.ast {
            let result = match stmt {
                Stmt::FuncDecl(stmt_func_decl) => self.stmt_func_decl(&stmt_func_decl),
                Stmt::StructDecl(stmt_struct_decl) => self.stmt_struct_decl(&stmt_struct_decl),
                _ => unreachable!(
                    "Did not expect a non function or non struct Stmt at the top level"
                ),
            };
            if result.is_err() {
                error!("{}", result.err().unwrap());
                return Err(());
            }
        }
        Ok(self.contains_main)
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
            Expr::I64(expr_i64) => self.expr_i64(expr_i64),
            Expr::Identifier(expr_identifier) => self.expr_identifier(expr_identifier),
            Expr::BinaryOp(expr_binary_op) => self.expr_binary_op(expr_binary_op),
            Expr::UnaryOp(expr_unary_op) => self.expr_unary_op(expr_unary_op),
            Expr::Ref(expr_ref) => self.expr_ref(expr_ref),
            Expr::Deref(expr_deref) => self.expr_deref(expr_deref),
            Expr::Call(expr_call) => self.expr_call(expr_call),
            Expr::StructInstance(expr_struct_instance) => {
                self.expr_struct_instance(expr_struct_instance)
            }
            Expr::Get(expr_get_property) => self.expr_get(expr_get_property),
        }
    }

    fn stmt_func_decl(&mut self, stmt_func_decl: &StmtFuncDecl) -> AnalyzerRetType {
        let ret_type = stmt_func_decl.ret_type.clone();
        self.current_func_ret_type_wrapper = ret_type.clone();
        if stmt_func_decl.ident == "main" {
            if stmt_func_decl.args.len() != 0 {
                return Err(AnalyzerError::Error(
                    "The 'main' function cannot have arguments".to_string(),
                ));
            }
            if !stmt_func_decl.ret_type.is_type(BType::I64) {
                return Err(AnalyzerError::Error(
                    "The 'main' function can only return type i64".to_string(),
                ));
            }
            self.contains_main = true;
        }

        let mut func_env = Env::new();
        func_env.vars = stmt_func_decl.args.clone();

        self.envs.push(func_env);
        self.stmt(&stmt_func_decl.body)?;
        self.envs.pop();

        if !self.current_func_ret_type_wrapper.is_type(BType::Void) {
            self.check_last_ret_stmt(&stmt_func_decl.body)?;
        }

        Ok(ret_type)
    }

    fn stmt_struct_decl(&mut self, _stmt_struct_decl: &StmtStructDecl) -> AnalyzerRetType {
        Ok(BTypeWrapper::void())
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
                stmt_assign.assign_to_expr.get_file_coords(),
            )),
            Stmt::While(stmt_while) => Err(AnalyzerError::ErrorFC(
                "Last statment must be for return".to_string(),
                stmt_while.condition.get_file_coords(),
            )),
            Stmt::Expr(_) => unreachable!("Shouldn't be processing expressions here"),
            Stmt::FuncDecl(_) => {
                unreachable!("Did not expect a function declaration inside another function")
            }
            Stmt::StructDecl(_) => {
                unreachable!("Did not expect a struct declaration inside another function")
            }
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
        Ok(BTypeWrapper::void())
    }

    fn stmt_return(&mut self, stmt_return: &StmtReturn) -> AnalyzerRetType {
        let expr_maybe = &stmt_return.expr;
        let expr_type_wrapper = match expr_maybe {
            Some(expr) => self.expr(expr)?,
            None => BTypeWrapper::void(),
        };
        if expr_type_wrapper != self.current_func_ret_type_wrapper {
            return Err(AnalyzerError::ErrorFC(
                format!(
                    "Expected return type '{:?}' by got '{:?}'",
                    self.current_func_ret_type_wrapper, expr_type_wrapper
                ),
                expr_maybe.as_ref().unwrap().get_file_coords(),
            ));
        }
        Ok(expr_type_wrapper)
    }

    fn stmt_if(&mut self, stmt_if: &StmtIf) -> AnalyzerRetType {
        let condition_btype_wrapper = self.expr(&stmt_if.condition)?;
        if !condition_btype_wrapper.is_type(BType::Bool) {
            return Err(AnalyzerError::ErrorFC(
                format!(
                    "Expected if condition to have a 'Bool' result but got '{:?}'",
                    condition_btype_wrapper
                ),
                stmt_if.condition.get_file_coords(),
            ));
        }
        self.stmt(&stmt_if.body)?;
        Ok(BTypeWrapper::void())
    }

    fn stmt_if_else(&mut self, stmt_if_else: &StmtIfElse) -> AnalyzerRetType {
        let condition_btype_wrapper = self.expr(&stmt_if_else.condition)?;
        if !condition_btype_wrapper.is_type(BType::Bool) {
            return Err(AnalyzerError::ErrorFC(
                format!(
                    "Expected if condition to have a 'Bool' result but got '{:?}'",
                    condition_btype_wrapper
                ),
                stmt_if_else.condition.get_file_coords(),
            ));
        }
        self.stmt(&stmt_if_else.if_body)?;
        self.stmt(&stmt_if_else.else_body)?;
        Ok(BTypeWrapper::void())
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

        let var_btype_wrapper = &stmt_var_decl.btype_wrapper;
        let expr_btype_wrapper = self.expr(&stmt_var_decl.expr)?;

        if !var_btype_wrapper.is_type(BType::Void) && *var_btype_wrapper != expr_btype_wrapper {
            return Err(AnalyzerError::ErrorFC(
                format!(
                    "Var has type '{:?}' but right expression has '{:?}'",
                    var_btype_wrapper, expr_btype_wrapper
                ),
                stmt_var_decl.expr.get_file_coords(),
            ));
        }

        (&mut self.envs.last_mut().unwrap().vars).push(VarTypeInfo {
            ident: stmt_var_decl.ident.clone(),
            btype_wrapper: expr_btype_wrapper,
        });

        Ok(BTypeWrapper::void())
    }

    fn stmt_assign(&mut self, stmt_assign: &StmtAssign) -> AnalyzerRetType {
        let var_btype = self.expr(&stmt_assign.ident_expr)?;
        let expr_btype = self.expr(&stmt_assign.assign_to_expr)?;

        if var_btype != expr_btype {
            return Err(AnalyzerError::ErrorFC(
                format!(
                    "Variable '{}' has type '{:?}' but '{:?}' was given",
                    stmt_assign.ident_expr.get_file_coords(),
                    var_btype,
                    expr_btype
                ),
                stmt_assign.assign_to_expr.get_file_coords(),
            ));
        }

        Ok(BTypeWrapper::void())
    }

    fn stmt_while(&mut self, stmt_while: &StmtWhile) -> AnalyzerRetType {
        let condition_btype_wrapper = self.expr(&stmt_while.condition)?;
        if !condition_btype_wrapper.is_type(BType::Bool) {
            return Err(AnalyzerError::ErrorFC(
                format!(
                    "Expected while condition to have a 'Bool' result but got '{:?}'",
                    condition_btype_wrapper
                ),
                stmt_while.condition.get_file_coords(),
            ));
        }
        self.stmt(&stmt_while.body)?;
        Ok(BTypeWrapper::void())
    }

    fn expr_bool(&mut self, _expr_bool: &ExprBool) -> AnalyzerRetType {
        Ok(BTypeWrapper::new(BType::Bool))
    }

    fn expr_i64(&mut self, _expr_i64: &ExprI64) -> AnalyzerRetType {
        Ok(BTypeWrapper::new(BType::I64))
    }

    fn expr_identifier(&mut self, expr_identifier: &ExprIdenifier) -> AnalyzerRetType {
        let ident = &expr_identifier.ident;
        for env in self.envs.iter().rev() {
            let var_maybe = env.vars.iter().filter(|var| var.ident == *ident).last();
            if let Some(var) = var_maybe {
                return Ok(var.btype_wrapper.clone());
            }
        }

        Err(AnalyzerError::ErrorFC(
            format!("'{:?}' is undefined", ident),
            expr_identifier.file_coords,
        ))
    }

    fn expr_binary_op(&mut self, expr_binary_op: &ExprBinaryOp) -> AnalyzerRetType {
        let left_btype_wrapper = self.expr(&expr_binary_op.left)?;
        let right_btype_wrapper = self.expr(&expr_binary_op.right)?;

        let supported_btype_wrappers = expr_binary_op.op.get_supported_btype_wrappers();
        if !supported_btype_wrappers.contains(&left_btype_wrapper) {
            return Err(AnalyzerError::ErrorFC(
                format!(
                    "Op '{:?}' expects '{:?}' types but it got '{:?}' in the left term",
                    expr_binary_op.op, supported_btype_wrappers, left_btype_wrapper
                ),
                expr_binary_op.file_coords,
            ));
        }
        if !supported_btype_wrappers.contains(&right_btype_wrapper) {
            return Err(AnalyzerError::ErrorFC(
                format!(
                    "Op '{:?}' expects '{:?}' types but it got '{:?}' in the right term",
                    expr_binary_op.op, supported_btype_wrappers, right_btype_wrapper
                ),
                expr_binary_op.file_coords,
            ));
        }

        if left_btype_wrapper != right_btype_wrapper {
            return Err(AnalyzerError::ErrorFC(
                format!(
                    "Mismatched types '{:?}' and '{:?}'",
                    left_btype_wrapper, right_btype_wrapper
                ),
                expr_binary_op.file_coords,
            ));
        }

        Ok(expr_binary_op.op.get_result_btype_wrapper())
    }

    fn expr_unary_op(&mut self, expr_unary_op: &ExprUnaryOp) -> AnalyzerRetType {
        let btype = self.expr(&expr_unary_op.term)?;

        let supported_btypes = expr_unary_op.op.get_supported_btype_wrappers();
        if !supported_btypes.contains(&btype) {
            return Err(AnalyzerError::ErrorFC(
                format!(
                    "Op '{:?}' expects '{:?}' types but it got '{:?}'",
                    expr_unary_op.op, supported_btypes, btype
                ),
                expr_unary_op.file_coords,
            ));
        }

        Ok(expr_unary_op.op.get_result_btype_wrapper())
    }

    fn expr_ref(&mut self, expr_ref: &ExprRef) -> AnalyzerRetType {
        let term_btype_wrapper = self.expr(&expr_ref.term)?;
        Ok(term_btype_wrapper.into_ref())
    }

    fn expr_deref(&mut self, expr_deref: &ExprDeref) -> AnalyzerRetType {
        let term_btype_wrapper = self.expr(&expr_deref.term)?;
        Ok(term_btype_wrapper.into_deref())
    }

    fn expr_call(&mut self, expr_call: &ExprCall) -> AnalyzerRetType {
        let func_decl = self.declarations.funcs.get(&expr_call.name);
        if func_decl.is_none() {
            return Err(AnalyzerError::ErrorFC(
                format!("Function '{}' is not defined", expr_call.name),
                expr_call.file_coords,
            ));
        }
        let func_decl = func_decl.unwrap();

        if func_decl.arg_types.len() != expr_call.args.len() {
            return Err(AnalyzerError::ErrorFC(
                format!(
                    "Function '{} 'takes {} arguments but {} where given",
                    expr_call.name,
                    func_decl.arg_types.len(),
                    expr_call.args.len()
                ),
                expr_call.file_coords,
            ));
        }

        for (i, (expected_btype, expr)) in func_decl
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

        Ok(func_decl.ret_type.clone())
    }

    fn expr_struct_instance(
        &mut self,
        expr_struct_instance: &ExprStructInstance,
    ) -> AnalyzerRetType {
        let struct_decl = self.declarations.structs.get(&expr_struct_instance.ident);
        if struct_decl.is_none() {
            return Err(AnalyzerError::ErrorFC(
                format!("Function '{}' is not defined", expr_struct_instance.ident),
                expr_struct_instance.file_coords,
            ));
        }
        let struct_decl = struct_decl.unwrap();

        if struct_decl.fields.len() != expr_struct_instance.fields.len() {
            return Err(AnalyzerError::ErrorFC(
                format!(
                    "Struct '{} 'takes {} arguments but {} where given",
                    expr_struct_instance.ident,
                    struct_decl.fields.len(),
                    expr_struct_instance.fields.len()
                ),
                expr_struct_instance.file_coords,
            ));
        }

        let mut missing_args: Vec<String> = Vec::with_capacity(0);
        let mut mismatched_types: HashMap<String, (BTypeWrapper, BTypeWrapper)> =
            HashMap::with_capacity(0);
        for field in &struct_decl.fields {
            match expr_struct_instance.fields.get(&field.ident) {
                Some(expr) => {
                    let expr_btype = self.expr(expr)?;
                    if expr_btype != field.btype_wrapper {
                        mismatched_types.insert(
                            field.ident.clone(),
                            (field.btype_wrapper.clone(), expr_btype.clone()),
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
                    expr_struct_instance.ident,
                    missing_args.join(", ")
                ),
                expr_struct_instance.file_coords,
            ));
        }
        if !mismatched_types.is_empty() {
            return Err(AnalyzerError::ErrorFC(
                format!(
                    "Mismached fields in sturct '{}': {}",
                    expr_struct_instance.ident,
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

        Ok(BTypeWrapper::new(BType::Struct(
            expr_struct_instance.ident.clone(),
        )))
    }

    fn expr_get(&mut self, expr_get: &ExprGet) -> AnalyzerRetType {
        // Returns list of identifiers and if the identifier in question is getting dereferenced or not
        fn get_idents(expr: &Expr) -> Vec<(String, bool)> {
            match expr {
                Expr::Identifier(expr_identifier) => vec![(expr_identifier.ident.clone(), false)],
                Expr::Get(expr_get) => {
                    let mut v: Vec<(String, bool)> = vec![(expr_get.property.clone(), false)];
                    v.append(&mut get_idents(&expr_get.ident));
                    v
                }
                Expr::Deref(expr_deref) => {
                    let mut v = get_idents(&expr_deref.term);
                    v.first_mut().unwrap().1 = true;
                    v
                }
                _ => unreachable!(
                    "Got unexpected Expr type while getting assign to identifier: '{:?}'",
                    expr
                ),
            }
        }

        let mut idents = get_idents(&Expr::Get(expr_get.clone()));
        let ident = idents.pop().unwrap();

        for env in self.envs.iter().rev() {
            let var = env.vars.iter().filter(|var| var.ident == *ident.0).last();
            if var.is_none() {
                continue;
            }
            let var = var.unwrap();

            if let BType::Struct(struct_name) = &var.btype_wrapper.btype {
                if var.btype_wrapper.is_ref && !ident.1 {
                    return Err(AnalyzerError::ErrorFC(
                        format!(
                            "Try dereferncing struct instance '{}' to access it's contents",
                            ident.0
                        ),
                        expr_get.file_coords,
                    ));
                }

                let mut struct_decl = self.get_struct_info(&struct_name);
                let mut btype_wrapper = BTypeWrapper::void();
                while let Some(ident) = idents.pop() {
                    let field_maybe = struct_decl
                        .fields
                        .iter()
                        .filter(|field| field.ident == *ident.0)
                        .last();
                    if field_maybe.is_none() {
                        return Err(AnalyzerError::ErrorFC(
                            format!(
                                "'{:?}' does not have a field named '{}'",
                                struct_name, ident.0
                            ),
                            expr_get.file_coords,
                        ));
                    }

                    let field = field_maybe.unwrap();
                    if let BType::Struct(struct_name) = &field.btype_wrapper.btype {
                        if field.btype_wrapper.is_ref && !ident.1 {
                            return Err(AnalyzerError::ErrorFC(
                                format!(
                                    "Try dereferncing struct instance '{}' to access it's contents",
                                    ident.0
                                ),
                                expr_get.file_coords,
                            ));
                        }
                        struct_decl = self.get_struct_info(&struct_name);
                    } else {
                        btype_wrapper = field.btype_wrapper.clone();
                        break;
                    }
                }
                if !idents.is_empty() {
                    return Err(AnalyzerError::ErrorFC(format!("Cannot get properties from basic data types. Trying to get a property from '{:?}'", btype_wrapper), expr_get.file_coords));
                }
                println!(
                    "Get expr type: {:?} - {}",
                    btype_wrapper, expr_get.file_coords
                );
                return Ok(btype_wrapper);
            } else {
                return Err(AnalyzerError::ErrorFC(
                    format!("'{:?}' is not a struct", ident),
                    expr_get.file_coords,
                ));
            }
        }

        Err(AnalyzerError::ErrorFC(
            format!("'{:?}' is undefined", ident),
            expr_get.file_coords,
        ))
    }

    //////////////////////////////////////////////////////////
    /// Helper methods
    //////////////////////////////////////////////////////////

    fn get_struct_info(&self, ident: &str) -> &StructDecl {
        self.declarations.structs.get(ident).expect(&format!(
            "How did a variable with a non existing struct type {} got created?",
            ident
        ))
    }
}

pub fn analyze(ast: &Ast, extracted_declarations: &Declarations) -> bool {
    info!("Analyzing semantics...");
    match Analyzer::new(ast, extracted_declarations).analyze() {
        Err(_) => {
            error!("Anaysis failed!");
            std::process::exit(1);
        }
        Ok(contains_main) => {
            info!("Analysis complete!");
            contains_main
        }
    }
}
