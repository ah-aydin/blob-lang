use derive_new::new;

use super::op_type::{BinaryOpType, UnaryOpType};

#[derive(Debug, Clone, PartialEq, Eq, new)]
pub struct ExprUnaryOp {
    op_type: UnaryOpType,
    expr: Box<Expr>,
    line: usize
}

#[derive(Debug, Clone, PartialEq, Eq, new)]
pub struct ExprBinaryOp {
    left_expr: Box<Expr>,
    op_type: BinaryOpType,
    right_expr: Box<Expr>,
    line: usize
}

#[derive(Debug, Clone, PartialEq, Eq, new)]
pub struct ExprCall {
    name: String,
    args: Vec<Expr>,
    line: usize
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Number(String),
    Identifier(String),
    UnaryOp(ExprUnaryOp),
    BinaryOp(ExprBinaryOp),
    Call(ExprCall),
}
