use blob_bc::{DirectiveType, OpCode, SectionType};
use blob_common::file_coords::FileCoords;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenType {
    Op(OpCode),
    Reg(u8),
    ImdVal(i64),
    LabelDecl(String),
    LabelUsg(String),
    Directive(DirectiveType),
    Section(SectionType),
    String(String),

    LeftBracket,
    RightBracket,
    Equal,

    NL,
    EOF,
}

impl TokenType {
    pub fn get_reg(&self) -> Result<u8, ()> {
        match self {
            TokenType::Reg(reg) => Ok(*reg),
            _ => Err(()),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub file_coords: FileCoords,
}
