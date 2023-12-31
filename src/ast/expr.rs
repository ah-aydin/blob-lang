use super::op_type::{BinaryOpType, UnaryOpType};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Number(String),
    Identifier(String),

    /// ```rust
    /// (Op: UnaryOpType, Term: Box<Expr>)
    /// ```
    UnaryOp(UnaryOpType, Box<Expr>),
    /// ```rust
    /// (LeftTerm: Box<Expr>, Op: BinaryOpType, RightTerm: Box<Expr>)
    /// ```
    BinaryOp(Box<Expr>, BinaryOpType, Box<Expr>),

    /// ```rust
    /// (CalleeName: String, Args: Vec<Expr>)
    /// ```
    Call(String, Vec<Expr>),
}
