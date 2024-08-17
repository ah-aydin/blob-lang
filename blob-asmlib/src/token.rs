use blob_bc::{InsArgType, OpCode};
use blob_common::file_coords::FileCoords;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum DirectiveType {
    /// Null terminated string
    ASCIZ,
    /// Non null terminated string
    ASCI,
    /// 4-byte number
    WORD,
    /// 1-byte number
    BYTE,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum SectionType {
    DATA,
    TEXT,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenType {
    Op(OpCode),
    Reg(u8),
    ImdVal(i32),
    LabelDecl(String),
    LabelUsg(String),
    Directive(DirectiveType),
    Section(SectionType),
    String(String),

    LeftBracket,
    RightBracket,

    NL,
    EOF,
}

impl TokenType {
    pub fn get_ins_arg_type(&self) -> Option<InsArgType> {
        match self {
            TokenType::Reg(_) => Some(InsArgType::Reg),
            TokenType::ImdVal(_) => Some(InsArgType::Imd),
            TokenType::LabelUsg(_) => Some(InsArgType::Label),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub file_coords: FileCoords,
}
