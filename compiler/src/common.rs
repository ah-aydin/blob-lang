use ast::blob_type::BlobType;

#[derive(Debug)]
pub enum CompileError {
    Failed,
}

#[derive(Debug)]
pub struct FuncData {
    pub name: String,
    pub args: Vec<(String, BlobType)>,
    pub return_type: Option<BlobType>,
}
impl FuncData {
    pub fn from_stmt(stmt: &ast::stmt::StmtFuncDecl) -> FuncData {
        FuncData {
            name: stmt.name.clone(),
            args: stmt.args.clone(),
            return_type: stmt.return_type.clone(),
        }
    }
}
