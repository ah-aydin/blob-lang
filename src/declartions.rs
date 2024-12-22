use std::collections::HashMap;

use crate::{
    ast::{
        btype::BTypeWrapper,
        stmt::{Stmt, StmtFuncDecl, StmtStructDecl, VarTypeInfo},
        Ast,
    },
    error,
};

#[derive(Debug)]
pub struct StructDecl {
    pub fields: Vec<VarTypeInfo>,
}

#[derive(Debug)]
pub struct FuncDecl {
    pub arg_idents: Vec<String>,
    pub arg_types: Vec<BTypeWrapper>,
    pub ret_type: BTypeWrapper,
}

#[derive(Debug)]
pub struct Declarations {
    pub structs: HashMap<String, StructDecl>,
    pub funcs: HashMap<String, FuncDecl>,
}

pub fn extract_declarations(ast: &Ast) -> Declarations {
    let mut errored = false;

    let mut structs = HashMap::new();
    let mut funcs = HashMap::new();

    for stmt in ast {
        match stmt {
            Stmt::StructDecl(stmt_struct_decl) => {
                if structs.contains_key(&stmt_struct_decl.ident) {
                    error!("Struct '{}' is already defined", stmt_struct_decl.ident);
                    errored = true;
                    continue;
                }
                structs.insert(
                    stmt_struct_decl.ident.clone(),
                    extract_struct_declaration(stmt_struct_decl),
                );
            }
            Stmt::FuncDecl(stmt_func_decl) => {
                if funcs.contains_key(&stmt_func_decl.ident) {
                    error!("Function '{}' is already defined", stmt_func_decl.ident);
                    errored = true;
                    continue;
                }
                funcs.insert(
                    stmt_func_decl.ident.clone(),
                    extract_func_declaration(stmt_func_decl),
                );
            }
            _ => {}
        }
    }

    if errored {
        std::process::exit(1);
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
        arg_idents: stmt_func_decl
            .args
            .iter()
            .map(|t| t.ident.clone())
            .collect(),
        arg_types: stmt_func_decl
            .args
            .iter()
            .map(|t| t.btype_wrapper.clone())
            .collect(),
        ret_type: stmt_func_decl.ret_type.clone(),
    }
}
