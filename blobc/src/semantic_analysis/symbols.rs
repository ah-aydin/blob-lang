use crate::ast::{blob_type::BlobType, stmt::StmtFuncDecl};

#[derive(Debug)]
pub struct FuncData {
    pub name: String,
    pub args: Vec<VarData>,
    pub return_type: Option<BlobType>,
    pub line: usize,
}

impl FuncData {
    pub fn new(func_decl: &StmtFuncDecl) -> FuncData {
        FuncData {
            name: func_decl.name.clone(),
            args: VarData::from_func_decl_args(&func_decl.args),
            return_type: func_decl.return_type.clone(),
            line: func_decl.line,
        }
    }
}

#[derive(Debug)]
pub struct VarData {
    pub name: String,
    pub blob_type: BlobType,
}

impl VarData {
    pub fn new(name: String, blob_type: BlobType) -> VarData {
        VarData { name, blob_type }
    }

    pub fn from_func_decl_args(args: &Vec<(String, BlobType)>) -> Vec<VarData> {
        args.iter()
            .map(|(name, blob_type)| VarData::new(name.clone(), blob_type.clone()))
            .collect::<Vec<VarData>>()
    }
}
