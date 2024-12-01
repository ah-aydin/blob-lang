use std::collections::HashMap;

use crate::ast::{
    btype::BType,
    stmt::{Stmt, StmtFuncDecl, StmtStructDecl, VarTypeInfo},
    Ast,
};

#[derive(Debug)]
pub struct StructDecl {
    pub fields: Vec<VarTypeInfo>,
}

#[derive(Debug)]
pub struct FuncDecl {
    pub args: Vec<VarTypeInfo>,
    pub ret_type: BType,
}

#[derive(Debug)]
pub struct Declarations {
    pub structs: HashMap<String, StructDecl>,
    pub funcs: HashMap<String, FuncDecl>,
}

pub fn extract_declarations(ast: &Ast) -> Declarations {
    let mut structs = HashMap::new();
    let mut funcs = HashMap::new();

    for stmt in ast {
        match stmt {
            Stmt::StructDecl(stmt_struct_decl) => {
                structs.insert(
                    stmt_struct_decl.ident.clone(),
                    extract_struct_declaration(stmt_struct_decl),
                );
            }
            Stmt::FuncDecl(stmt_func_decl) => {
                funcs.insert(
                    stmt_func_decl.ident.clone(),
                    extract_func_declaration(stmt_func_decl),
                );
            }
            _ => {}
        }
    }

    Declarations { structs, funcs }
}

fn extract_struct_declaration(stmt_struct_decl: &StmtStructDecl) -> StructDecl {
    StructDecl {
        fields: stmt_struct_decl.fields.clone(),
    }
}

fn extract_func_declaration(stmt_func_decl: &StmtFuncDecl) -> FuncDecl {
    FuncDecl {
        args: stmt_func_decl.args.clone(),
        ret_type: stmt_func_decl.ret_type.clone(),
    }
}
