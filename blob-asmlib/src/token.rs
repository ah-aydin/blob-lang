use blob_bc::OpCode;
use blob_common::file_coords::FileCoords;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum DirectiveType {
    /// Null terminated string
    ASCIIZ,
    /// Non null terminated string
    ASCII,
    /// 4-byte number
    WORD,
    /// 1-byte number
    BYTE,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
    Equal,

    NL,
    EOF,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub file_coords: FileCoords,
}
