use super::op_type::{BinaryOpType, UnaryOpType};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    I32(i32),
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
    Call(Box<Expr>, Vec<Expr>),

    /// ```rust
    /// (name: Expr, expr: Expr)
    /// ```
    Assign(String, Box<Expr>),
}
