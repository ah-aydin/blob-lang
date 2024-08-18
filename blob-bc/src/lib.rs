use std::fmt::Display;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[repr(u8)]
pub enum OpCode {
    Hlt,

    Load,
    LoadImd,
    LoadWord,
    LoadMemByte,
    LoadMemQuaterWord,
    LoadMemHalfWord,
    LoadMemWord,

    StrByte,
    StrByteImd,
    StrQuaterWord,
    StrQuaterWordImd,
    StrHalfWord,
    StrWord,

    Add,
    AddImd,
    Sub,
    SubImd,
    Mul,
    MulImd,
    Div,
    DivImd,

    Jmp,
    JmpF,
    JmpFImd,
    JmpB,
    JmpBImd,
    JCmp,
    JCmpF,
    JCmpFImd,
    JCmpB,
    JCmpBImd,

    Eq,
    EqImd,
    NEq,
    NEqImd,
    Gt,
    GtImd,
    Lt,
    LtImd,
    Ge,
    GeImd,
    Le,
    LeiImd,

    Push,
    Pop,

    Aloc,

    IGL,
}

pub enum OpCodeType {
    Misc,
    Load,
    Str,
    Math,
    Jmp,
    Cmp,
    Stack,
    Heap,
}

impl OpCode {
    pub fn get_type(&self) -> OpCodeType {
        match self {
            OpCode::Hlt => OpCodeType::Misc,

            OpCode::Load
            | OpCode::LoadImd
            | OpCode::LoadWord
            | OpCode::LoadMemByte
            | OpCode::LoadMemQuaterWord
            | OpCode::LoadMemHalfWord
            | OpCode::LoadMemWord => OpCodeType::Load,

            OpCode::StrByte
            | OpCode::StrByteImd
            | OpCode::StrQuaterWord
            | OpCode::StrQuaterWordImd
            | OpCode::StrHalfWord
            | OpCode::StrWord => OpCodeType::Str,

            OpCode::Add
            | OpCode::AddImd
            | OpCode::Sub
            | OpCode::SubImd
            | OpCode::Mul
            | OpCode::MulImd
            | OpCode::Div
            | OpCode::DivImd => OpCodeType::Math,

            OpCode::Jmp
            | OpCode::JmpF
            | OpCode::JmpFImd
            | OpCode::JmpB
            | OpCode::JmpBImd
            | OpCode::JCmp
            | OpCode::JCmpF
            | OpCode::JCmpFImd
            | OpCode::JCmpB
            | OpCode::JCmpBImd => OpCodeType::Jmp,

            OpCode::Eq
            | OpCode::EqImd
            | OpCode::NEq
            | OpCode::NEqImd
            | OpCode::Gt
            | OpCode::GtImd
            | OpCode::Lt
            | OpCode::LtImd
            | OpCode::Ge
            | OpCode::GeImd
            | OpCode::Le
            | OpCode::LeiImd => OpCodeType::Cmp,

            OpCode::Push | OpCode::Pop => OpCodeType::Stack,

            OpCode::Aloc => OpCodeType::Heap,

            OpCode::IGL => unreachable!("{:?} doesn't have an OpCodeType", self),
        }
    }

    pub fn get_imd_version(&self) -> OpCode {
        unsafe { std::mem::transmute::<u8, OpCode>(*self as u8 + 1) }
    }

    fn to_byte(&self) -> u8 {
        *self as u8
    }
}

impl From<u8> for OpCode {
    fn from(v: u8) -> OpCode {
        unsafe { std::mem::transmute::<u8, OpCode>(v) }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InsArg {
    Reg(u8),
    Imd(u16),
    Word(u64),
}

impl InsArg {
    pub fn to_bytes(&self) -> Vec<u8> {
        match self {
            InsArg::Reg(reg) => vec![*reg],
            InsArg::Imd(imd) => imd.to_be_bytes().to_vec(),
            InsArg::Word(word) => word.to_be_bytes().to_vec(),
        }
    }

    pub fn is_imd(&self) -> bool {
        match self {
            InsArg::Imd(_) => true,
            _ => false,
        }
    }
}

impl Display for InsArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InsArg::Reg(reg) => write!(f, "R{}", reg),
            InsArg::Imd(imd) => write!(f, "#{}", imd),
            InsArg::Word(word) => write!(f, ".word {}", word),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InsText {
    Arg0(OpCode),
    Arg1(OpCode, InsArg),
    Arg2(OpCode, InsArg, InsArg),
    Arg3(OpCode, InsArg, InsArg, InsArg),
    LabelDecl(String),
    LabelUsage(String),
}

impl InsText {
    pub fn to_bytes(&self) -> Vec<u8> {
        match self {
            InsText::Arg0(op) => vec![op.to_byte()],
            InsText::Arg1(op, arg) => vec![op.to_byte()]
                .into_iter()
                .chain(arg.to_bytes().into_iter())
                .collect(),
            InsText::Arg2(op, arg1, arg2) => vec![op.to_byte()]
                .into_iter()
                .chain(arg1.to_bytes().into_iter())
                .chain(arg2.to_bytes().into_iter())
                .collect(),
            InsText::Arg3(op, arg1, arg2, arg3) => vec![op.to_byte()]
                .into_iter()
                .chain(arg1.to_bytes().into_iter())
                .chain(arg2.to_bytes().into_iter())
                .chain(arg3.to_bytes().into_iter())
                .collect(),
            InsText::LabelDecl(_) | InsText::LabelUsage(_) => {
                unreachable!("A label instructions cannot be made into a byte array")
            }
        }
    }
}

impl Display for InsText {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InsText::Arg0(op) => write!(f, "{:?}", op),
            InsText::Arg1(op, arg) => write!(f, "{:?} {}", op, arg),
            InsText::Arg2(op, arg1, arg2) => write!(f, "{:?} {} {}", op, arg1, arg2),
            InsText::Arg3(op, arg1, arg2, arg3) => write!(f, "{:?} {} {} {}", op, arg1, arg2, arg3),
            InsText::LabelDecl(label) => write!(f, "{}:", label),
            InsText::LabelUsage(label) => write!(f, "{}", label),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum SectionType {
    Data,
    Text,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum DirectiveType {
    /// Null terminated string
    Asciiz,
    /// Non null terminated string
    Ascii,
    /// 8-byte
    Word,
    /// 4-byte
    HalfWord,
    /// 2-byte
    QuaterWord,
    /// 1-byte number
    Byte,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InsData {
    Directive(DirectiveType, String),
    Label(String),
}

impl InsData {
    pub fn to_bytes(&self) -> Vec<u8> {
        match self {
            InsData::Directive(directive_type, lexeme) => {
                self.directive_to_bytes(*directive_type, lexeme)
            }
            InsData::Label(_) => {
                unreachable!("A label instructions cannot be made into a byte array")
            }
        }
    }

    fn directive_to_bytes(&self, direcitve_type: DirectiveType, lexeme: &str) -> Vec<u8> {
        match direcitve_type {
            DirectiveType::Asciiz => [lexeme.as_bytes().to_vec(), vec![b'\0']]
                .into_iter()
                .flat_map(|v| v)
                .collect(),
            DirectiveType::Ascii => lexeme.as_bytes().to_vec(),
            DirectiveType::Word => lexeme.parse::<i64>().unwrap().to_be_bytes().to_vec(),
            DirectiveType::HalfWord => lexeme.parse::<i32>().unwrap().to_be_bytes().to_vec(),
            DirectiveType::QuaterWord => lexeme.parse::<i16>().unwrap().to_be_bytes().to_vec(),
            DirectiveType::Byte => lexeme.parse::<u8>().unwrap().to_be_bytes().to_vec(),
        }
    }
}

impl Display for InsData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InsData::Directive(dt, lexeme) => write!(f, "{:?} {}", dt, lexeme),
            InsData::Label(label) => write!(f, "{}", label),
        }
    }
}
