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

impl OpCode {
    fn get_imd_version(&self) -> OpCode {
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InsText {
    Arg0(OpCode),
    Arg1(OpCode, InsArg),
    Arg2(OpCode, InsArg, InsArg),
    Arg3(OpCode, InsArg, InsArg, InsArg),
    Label(String),
}

impl InsText {
    pub fn from_ins_args(mut op_code: OpCode, args: Vec<InsArg>) -> InsText {
        if let Some(InsArg::Imd(_)) = args.last() {
            op_code = op_code.get_imd_version();
        }
        match args.len() {
            0 => InsText::Arg0(op_code),
            1 => InsText::Arg1(op_code, *args.get(0).unwrap()),
            2 => InsText::Arg2(op_code, *args.get(0).unwrap(), *args.get(1).unwrap()),
            3 => InsText::Arg3(
                op_code,
                *args.get(0).unwrap(),
                *args.get(1).unwrap(),
                *args.get(2).unwrap(),
            ),
            _ => unreachable!("How the heck did end up with more then 3 arguments?"),
        }
    }

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
            InsText::Label(_) => unreachable!("A label instructions cannot be made into a byte array"),
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

#[cfg(test)]
mod test_opcode {
    use super::*;

    #[test]
    fn it_gets_imd_variants() {
        assert_eq!(OpCode::Load.get_imd_version(), OpCode::LoadImd);
        assert_eq!(OpCode::Add.get_imd_version(), OpCode::AddImd);
        assert_eq!(OpCode::Mul.get_imd_version(), OpCode::MulImd);
        assert_eq!(OpCode::Div.get_imd_version(), OpCode::DivImd);
        assert_eq!(OpCode::JmpF.get_imd_version(), OpCode::JmpFImd);
        assert_eq!(OpCode::JmpB.get_imd_version(), OpCode::JmpBImd);
        assert_eq!(OpCode::JCmpF.get_imd_version(), OpCode::JCmpFImd);
        assert_eq!(OpCode::JCmpB.get_imd_version(), OpCode::JCmpBImd);
        assert_eq!(OpCode::Eq.get_imd_version(), OpCode::EqImd);
        assert_eq!(OpCode::NEq.get_imd_version(), OpCode::NEqImd);
        assert_eq!(OpCode::Gt.get_imd_version(), OpCode::GtImd);
        assert_eq!(OpCode::Lt.get_imd_version(), OpCode::LtImd);
        assert_eq!(OpCode::Ge.get_imd_version(), OpCode::GeImd);
        assert_eq!(OpCode::Le.get_imd_version(), OpCode::LeiImd);
    }
}
