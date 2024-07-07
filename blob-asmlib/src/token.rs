use blob_bc::OpCode;
use blob_common::file_coords::FileCoords;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenType {
    Op(OpCode),
    Reg(u8),
    ImdVal(i32),
    LabelDecl(String),
    LabelUsg(String),
    NL,
    EOF,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub file_coords: FileCoords,
}
