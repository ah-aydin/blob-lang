use super::{btype::BTypeWrapper, expr::Expr};

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
    pub expr: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct VarTypeInfo {
    pub ident: String,
    pub btype_wrapper: BTypeWrapper,
}

#[derive(Debug, Clone)]
pub struct StmtStructDecl {
    pub ident: String,
    pub fields: Vec<VarTypeInfo>,
}

#[derive(Debug, Clone)]
pub struct StmtFuncDecl {
    pub ident: String,
    pub args: Vec<VarTypeInfo>,
    pub ret_type: BTypeWrapper,
    pub body: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub struct StmtIf {
    pub condition: Expr,
    pub body: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub struct StmtIfElse {
    pub condition: Expr,
    pub if_body: Box<Stmt>,
    pub else_body: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub struct StmtVarDecl {
    pub ident: String,
    pub btype_wrapper: BTypeWrapper,
    pub expr: Expr,
}

#[derive(Debug, Clone)]
pub struct StmtAssign {
    pub ident_expr: Expr,
    pub assign_to_expr: Expr,
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
    StructDecl(StmtStructDecl),
    FuncDecl(StmtFuncDecl),
    If(StmtIf),
    IfElse(StmtIfElse),
    VarDecl(StmtVarDecl),
    Assign(StmtAssign),
    While(StmtWhile),
}

impl Stmt {
    pub fn is_block(&self) -> bool {
        match self {
            Stmt::Block(_) => true,
            _ => false,
        }
    }

    pub fn get_block_body(&self) -> Vec<Stmt> {
        match self {
            Stmt::Block(stmt_block) => stmt_block.stmts.clone(),
            _ => unreachable!("{:#?}", self),
        }
    }
}
