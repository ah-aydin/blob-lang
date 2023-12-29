use super::expr::Expr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    ExprStmt(Expr),

    Block(Vec<Stmt>),
    Return(Expr),

    /// ```rust
    /// (name: String, arg_names: Vec<String>, body: Stmt::Block)
    /// ```
    FunctionDecl(String, Vec<String>, Box<Stmt>),

    /// ```rust
    /// (condition: Expr, if_clause: Box<Stmt>)
    /// ```
    If(Expr, Box<Stmt>),
    /// ```rust
    /// (condition: Expr, if_clause: Box<Stmt>, else_clause: Box<Stmt>)
    /// ```
    IfElse(Expr, Box<Stmt>, Box<Stmt>),

    /// ```rust
    /// (name: Expr, expr: Expr)
    /// ```
    VarDecl(String, Expr),

    /// ```rust
    /// (condition: Expr, Body: Box<Stmt>)
    /// ```
    While(Expr, Box<Stmt>),
}
