use super::{expr::Expr, blob_type::BlobType};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    ExprStmt(Expr),

    Block(Vec<Stmt>),
    Return(Expr),

    /// ```rust
    /// (name, Vec<(arg_name, blob_type), return_type, body)
    /// ```
    FuncDecl(String, Vec<(String, BlobType)>, Option<BlobType>, Box<Stmt>),

    /// ```rust
    /// (condition, if_clause)
    /// ```
    If(Expr, Box<Stmt>),
    /// ```rust
    /// (condition, if_clause, else_clause)
    /// ```
    IfElse(Expr, Box<Stmt>, Box<Stmt>),

    /// ```rust
    /// (name, blob_type, expr)
    /// ```
    VarDecl(String, Option<BlobType>,Expr),

    /// ```rust
    /// (name, expr)
    /// ```
    Assign(String, Expr),

    /// ```rust
    /// (condition, Body)
    /// ```
    While(Expr, Box<Stmt>),
}
