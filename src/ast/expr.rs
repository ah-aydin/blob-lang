use std::collections::HashMap;

use crate::file_coords::FileCoords;

use super::op::{BinaryOp, UnaryOp};

#[derive(Debug, Clone)]
pub struct ExprBool {
    pub value: bool,
    pub file_coords: FileCoords,
}

#[derive(Debug, Clone)]
pub struct ExprI64 {
    pub value: i64,
    pub file_coords: FileCoords,
}

#[derive(Debug, Clone)]
pub struct ExprIdenifier {
    pub ident: String,
    pub file_coords: FileCoords,
}

#[derive(Debug, Clone)]
pub struct ExprStructInstance {
    pub ident: String,
    pub fields: HashMap<String, Expr>,
    pub file_coords: FileCoords,
}

#[derive(Debug, Clone)]
pub struct ExprBinaryOp {
    pub left: Box<Expr>,
    pub op: BinaryOp,
    pub right: Box<Expr>,
    pub file_coords: FileCoords,
}

#[derive(Debug, Clone)]
pub struct ExprUnaryOp {
    pub op: UnaryOp,
    pub term: Box<Expr>,
    pub file_coords: FileCoords,
}

#[derive(Debug, Clone)]
pub struct ExprRef {
    pub term: Box<Expr>,
    pub file_coords: FileCoords,
}

#[derive(Debug, Clone)]
pub struct ExprDeref {
    pub term: Box<Expr>,
    pub file_coords: FileCoords,
}

#[derive(Debug, Clone)]
pub struct ExprCall {
    pub name: String,
    pub args: Vec<Expr>,
    pub file_coords: FileCoords,
}

#[derive(Debug, Clone)]
pub struct ExprGet {
    pub ident: Box<Expr>,
    pub property: String,
    pub file_coords: FileCoords,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Bool(ExprBool),
    I64(ExprI64),
    Identifier(ExprIdenifier),
    StructInstance(ExprStructInstance),

    BinaryOp(ExprBinaryOp),
    UnaryOp(ExprUnaryOp),

    Ref(ExprRef),
    Deref(ExprDeref),

    Call(ExprCall),

    Get(ExprGet),
}

impl Expr {
    pub fn get_file_coords(&self) -> FileCoords {
        match self {
            Expr::Bool(expr) => expr.file_coords,
            Expr::I64(expr) => expr.file_coords,
            Expr::Identifier(expr) => expr.file_coords,
            Expr::StructInstance(expr) => expr.file_coords,
            Expr::BinaryOp(expr) => expr.file_coords,
            Expr::UnaryOp(expr) => expr.file_coords,
            Expr::Ref(expr) => expr.file_coords,
            Expr::Deref(expr) => expr.file_coords,
            Expr::Call(expr) => expr.file_coords,
            Expr::Get(expr) => expr.file_coords,
        }
    }

    pub fn is_assignable(&self) -> bool {
        match self {
            Expr::Identifier(_) | Expr::Get(_) |  Expr::Deref(_) => true,
            _ => false,
        }
    }
}
