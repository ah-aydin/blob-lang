use crate::common::FileCoords;

use super::op::{BinaryOp, BitwiseOp, BooleanOp, CmpOp, UnaryOp};

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
pub struct ExprBitwiseOp {
    pub left: Box<Expr>,
    pub op: BitwiseOp,
    pub right: Box<Expr>,
    pub file_coords: FileCoords,
}

#[derive(Debug, Clone)]
pub struct ExprBooleanOp {
    pub left: Box<Expr>,
    pub op: BooleanOp,
    pub right: Box<Expr>,
    pub file_coords: FileCoords,
}

#[derive(Debug, Clone)]
pub struct ExprCmpOp {
    pub left: Box<Expr>,
    pub op: CmpOp,
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
pub enum Expr {
    Bool(ExprBool),
    I64(ExprI64),
    Identifier(ExprIdenifier),
    String(ExprString),

    BinaryOp(ExprBinaryOp),
    BitwiseOp(ExprBitwiseOp),
    BooleanOp(ExprBooleanOp),
    CmpOp(ExprCmpOp),
    UnaryOp(ExprUnaryOp),

    Call(ExprCall),
}
