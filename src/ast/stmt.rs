use super::expr::Expr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    ExprStmt(Expr),

    Block(Vec<Stmt>),
    Return(Expr),

    /// ```rust
    /// (name, arg_names, body)
    /// ```
    FunctionDecl(String, Vec<String>, Box<Stmt>),

    /// ```rust
    /// (condition, if_clause)
    /// ```
    If(Expr, Box<Stmt>),
    /// ```rust
    /// (condition, if_clause, else_clause)
    /// ```
    IfElse(Expr, Box<Stmt>, Box<Stmt>),

    /// ```rust
    /// (name, expr)
    /// ```
    VarDecl(String, Expr),


    /// ```rust
    /// (name, expr)
    /// ```
    Assign(String, Expr),

    /// ```rust
    /// (condition, Body)
    /// ```
    While(Expr, Box<Stmt>),

    Empty,
}
