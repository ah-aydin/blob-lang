use super::op::{BinaryOp, BooleanOp, CmpOp, UnaryOp};

#[derive(Debug, Clone)]
pub struct ExprBinaryOp {
    pub left: Box<Expr>,
    pub op: BinaryOp,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct ExprBooleanOp {
    pub left: Box<Expr>,
    pub op: BooleanOp,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct ExprCmpOp {
    pub left: Box<Expr>,
    pub op: CmpOp,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct ExprUnaryOp {
    pub op: UnaryOp,
    pub term: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct ExprCall {
    pub name: String,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Bool(bool),
    I64(i64),
    Identifier(String),

    BinaryOp(ExprBinaryOp),
    BooleanOp(ExprBooleanOp),
    CmpOp(ExprCmpOp),
    UnaryOp(ExprUnaryOp),

    Call(ExprCall),
}
