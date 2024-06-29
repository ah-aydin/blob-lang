use std::collections::HashMap;

use crate::common::FileCoords;

use super::op::{BinaryOp, UnaryOp};

#[derive(Debug, Clone)]
pub struct ExprBool {
    pub value: bool,
    pub file_coords: FileCoords,
}

#[derive(Debug, Clone)]
pub struct ExprI32 {
    pub value: i32,
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
pub struct ExprString {
    pub value: String,
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
pub struct ExprCall {
    pub name: String,
    pub args: Vec<Expr>,
    pub file_coords: FileCoords,
}

#[derive(Debug, Clone)]
pub struct ExprGetProperty {
    pub ident: String,
    pub property: String,
    pub file_coords: FileCoords,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Bool(ExprBool),
    I32(ExprI32),
    Identifier(ExprIdenifier),
    StructInstance(ExprStructInstance),
    String(ExprString),

    BinaryOp(ExprBinaryOp),
    UnaryOp(ExprUnaryOp),

    Call(ExprCall),

    GetProperty(ExprGetProperty),
}

impl Expr {
    pub fn get_file_coords(&self) -> FileCoords {
        match self {
            Expr::Bool(expr) => expr.file_coords,
            Expr::I32(expr) => expr.file_coords,
            Expr::Identifier(expr) => expr.file_coords,
            Expr::StructInstance(expr) => expr.file_coords,
            Expr::String(expr) => expr.file_coords,
            Expr::BinaryOp(expr) => expr.file_coords,
            Expr::UnaryOp(expr) => expr.file_coords,
            Expr::Call(expr) => expr.file_coords,
            Expr::GetProperty(expr) => expr.file_coords,
        }
    }
}
