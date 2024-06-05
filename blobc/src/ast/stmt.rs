use super::{btype::BType, expr::Expr};

#[derive(Debug, Clone)]
pub struct StmtExpr {
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct StmtBlock {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct StmtReturn {
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct FuncDeclArg {
    pub name: String,
    pub bytpe: BType,
}

#[derive(Debug, Clone)]
pub struct StmtFuncDecl {
    pub name: String,
    pub args: Vec<FuncDeclArg>,
}

#[derive(Debug, Clone)]
pub struct StmtIf {
    condition: Expr,
    clause: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub struct StmtIfElse {
    condition: Expr,
    if_clause: Box<Stmt>,
    else_clause: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub struct StmtVarDecl {
    pub name: String,
    pub btype: Option<BType>,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct StmtAssign {
    pub name: String,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct StmtWhile {
    pub condition: Expr,
    pub body: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Expr(StmtExpr),
    Block(StmtBlock),
    Return(StmtReturn),
    FuncDecl(StmtFuncDecl),
    If(StmtIf),
    IfElse(StmtIfElse),
    VarDecl(StmtVarDecl),
    Assign(StmtAssign),
    While(StmtWhile),
}
