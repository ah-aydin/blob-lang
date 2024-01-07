use derive_new::new;

use super::op_type::{BinaryOpType, UnaryOpType};

#[derive(Debug, Clone, PartialEq, Eq, new)]
pub struct ExprUnaryOp {
    pub op_type: UnaryOpType,
    pub term: Box<Expr>,
    pub line: usize
}

#[derive(Debug, Clone, PartialEq, Eq, new)]
pub struct ExprBinaryOp {
    pub left_term: Box<Expr>,
    pub op_type: BinaryOpType,
    pub right_term: Box<Expr>,
    pub line: usize
}

#[derive(Debug, Clone, PartialEq, Eq, new)]
pub struct ExprCall {
    pub name: String,
    pub args: Vec<Expr>,
    pub line: usize
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Number(String),
    Identifier(String),
    UnaryOp(ExprUnaryOp),
    BinaryOp(ExprBinaryOp),
    Call(ExprCall),
}
