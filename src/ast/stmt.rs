use derive_new::new;

use super::{blob_type::BlobType, expr::Expr};

#[derive(Debug, Clone, PartialEq, Eq, new)]
pub struct StmtFuncDecl {
    pub name: String,
    pub args: Vec<(String, BlobType)>,
    pub return_type: Option<BlobType>,
    pub body: Box<Stmt>,
    pub line: usize
}

#[derive(Debug, Clone, PartialEq, Eq, new)]
pub struct StmtIf {
    pub condition: Expr,
    pub clause: Box<Stmt>,
    pub line: usize
}

#[derive(Debug, Clone, PartialEq, Eq, new)]
pub struct StmtIfElse {
    pub condition: Expr,
    pub if_clause: Box<Stmt>,
    pub else_clause: Box<Stmt>,
    pub line: usize
}

#[derive(Debug, Clone, PartialEq, Eq, new)]
pub struct StmtVarDecl {
    pub name: String,
    pub blob_type: Option<BlobType>,
    pub to: Expr,
    pub line: usize
}

#[derive(Debug, Clone, PartialEq, Eq, new)]
pub struct StmtAssign {
    pub name: String,
    pub to: Expr,
    pub line: usize
}

#[derive(Debug, Clone, PartialEq, Eq, new)]
pub struct StmtWhile {
    pub condition: Expr,
    pub body: Box<Stmt>,
    pub line: usize
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    ExprStmt(Expr),
    Block(Vec<Stmt>),
    Return(Expr),
    FuncDecl(StmtFuncDecl),
    If(StmtIf),
    IfElse(StmtIfElse),
    VarDecl(StmtVarDecl),
    Assign(StmtAssign),
    While(StmtWhile),
}
