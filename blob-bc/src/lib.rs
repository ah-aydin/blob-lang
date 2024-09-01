use std::fmt::Display;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[repr(u8)]
pub enum OpCode {
    Hlt,

    Load,
    LoadImd,
    LoadMemRegByte,
    LoadMemRegQuaterWord,
    LoadMemRegHalfWord,
    LoadMemRegWord,

    StrByte,
    StrByteImd,
    StrQuaterWord,
    StrQuaterWordImd,
    StrHalfWord,
    StrWord,

    LShift,
    RShift,

    Add,
    AddImd,
    Sub,
    SubImd,
    Mul,
    MulImd,
    Div,
    DivImd,

    JmpTable,
    JmpTableImd,

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

#[derive(Debug)]
pub enum OpCodeType {
    Misc,
    Load,
    Str,
    Shift,
    Math,
    JmpTable,
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
            | OpCode::LoadMemRegByte
            | OpCode::LoadMemRegQuaterWord
            | OpCode::LoadMemRegHalfWord
            | OpCode::LoadMemRegWord => OpCodeType::Load,

            OpCode::StrByte
            | OpCode::StrByteImd
            | OpCode::StrQuaterWord
            | OpCode::StrQuaterWordImd
            | OpCode::StrHalfWord
            | OpCode::StrWord => OpCodeType::Str,

            OpCode::LShift | OpCode::RShift => OpCodeType::Shift,

            OpCode::Add
            | OpCode::AddImd
            | OpCode::Sub
            | OpCode::SubImd
            | OpCode::Mul
            | OpCode::MulImd
            | OpCode::Div
            | OpCode::DivImd => OpCodeType::Math,

            OpCode::JmpTable | OpCode::JmpTableImd => OpCodeType::JmpTable,

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
}

impl From<u8> for OpCode {
    fn from(v: u8) -> OpCode {
        unsafe { std::mem::transmute::<u8, OpCode>(v) }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InsArg {
    Reg(u8),
    Imd(i64),
    Mem(u8),
    Label(String),
}

impl InsArg {
    pub fn is_imd(&self) -> bool {
        match self {
            InsArg::Imd(_) => true,
            _ => false,
        }
    }

    pub unsafe fn get_imd_val(&self) -> i64 {
        match self {
            InsArg::Imd(val) => *val,
            _ => unreachable!(),
        }
    }

    pub fn is_label(&self) -> bool {
        match self {
            InsArg::Label(_) => true,
            _ => false,
        }
    }
}

impl Display for InsArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InsArg::Reg(reg) => write!(f, "R{}", reg),
            InsArg::Imd(imd) => write!(f, "#{}", imd),
            InsArg::Mem(mem) => write!(f, "[{}]", mem),
            InsArg::Label(label) => write!(f, "{}", label),
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
    String(DirectiveType, String),
    Number(DirectiveType, i64),
    Label(String),
}

impl Display for InsData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InsData::String(dt, lexeme) => write!(f, "{:?} {}", dt, lexeme),
            InsData::Number(dt, number) => write!(f, "{:?} {}", dt, number),
            InsData::Label(label) => write!(f, "{}", label),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum SectionType {
    Data,
    Text,
}
