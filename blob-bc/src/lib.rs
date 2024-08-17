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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InsArgType {
    Reg,
    Imd,
    Word,
    Memory,
}

macro_rules! arg_types {
    (reg) => {
        vec![InsArgType::Reg]
    };
    (reg, imd) => {
        vec![InsArgType::Reg, InsArgType::Imd]
    };
    (reg, imd, mem) => {
        vec![
            InsArgType::Reg,
            InsArgType::Imd,
            InsArgType::Word,
            InsArgType::Memory,
        ]
    };
}

impl OpCode {
    pub fn get_imd_version(&self) -> OpCode {
        unsafe { std::mem::transmute::<u8, OpCode>(*self as u8 + 1) }
    }

    pub fn get_word_version(&self) -> OpCode {
        unsafe { std::mem::transmute::<u8, OpCode>(*self as u8 + 2) }
    }

    pub fn get_mem_byte_version(&self) -> OpCode {
        unsafe { std::mem::transmute::<u8, OpCode>(*self as u8 + 3) }
    }

    pub fn get_mem_quater_word_version(&self) -> OpCode {
        unsafe { std::mem::transmute::<u8, OpCode>(*self as u8 + 4) }
    }

    pub fn get_mem_quater_half_version(&self) -> OpCode {
        unsafe { std::mem::transmute::<u8, OpCode>(*self as u8 + 5) }
    }

    pub fn get_mem_word_version(&self) -> OpCode {
        unsafe { std::mem::transmute::<u8, OpCode>(*self as u8 + 6) }
    }

    pub fn to_byte(&self) -> u8 {
        *self as u8
    }

    pub fn get_args_types(&self) -> Vec<Vec<InsArgType>> {
        match self {
            OpCode::Hlt => vec![],

            OpCode::Load
            | OpCode::LoadImd
            | OpCode::LoadWord
            | OpCode::LoadMemByte
            | OpCode::LoadMemQuaterWord
            | OpCode::LoadMemHalfWord
            | OpCode::LoadMemWord => {
                vec![arg_types!(reg), arg_types!(reg, imd, mem)]
            }

            OpCode::StrByte
            | OpCode::StrByteImd
            | OpCode::StrQuaterWord
            | OpCode::StrQuaterWordImd
            | OpCode::StrHalfWord
            | OpCode::StrWord => {
                vec![arg_types!(reg), arg_types!(reg, imd)]
            }

            OpCode::Add
            | OpCode::AddImd
            | OpCode::Sub
            | OpCode::SubImd
            | OpCode::Mul
            | OpCode::MulImd
            | OpCode::Div
            | OpCode::DivImd => vec![arg_types!(reg), arg_types!(reg), arg_types!(reg, imd)],

            OpCode::Jmp | OpCode::JCmp => vec![arg_types!(reg)],
            OpCode::JmpF
            | OpCode::JmpFImd
            | OpCode::JmpB
            | OpCode::JmpBImd
            | OpCode::JCmpF
            | OpCode::JCmpFImd
            | OpCode::JCmpB
            | OpCode::JCmpBImd => vec![arg_types!(reg, imd)],

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
            | OpCode::LeiImd => vec![arg_types!(reg), arg_types!(reg, imd)],

            OpCode::Push | OpCode::Pop | OpCode::Aloc => vec![arg_types!(reg)],

            OpCode::IGL => unreachable!("Requeted instruction arg types on illegal op code"),
        }
    }
}

impl From<u8> for OpCode {
    fn from(v: u8) -> OpCode {
        unsafe { std::mem::transmute::<u8, OpCode>(v) }
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InsArg {
    Reg(u8),
    Imd(u16),
}

impl InsArg {
    pub fn to_bytes(&self) -> Vec<u8> {
        match self {
            InsArg::Reg(reg) => vec![*reg],
            InsArg::Imd(imd) => vec![(imd >> 8) as u8, (imd & 0xFF) as u8],
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ins {
    N(OpCode),
    A(OpCode, InsArg),
    AA(OpCode, InsArg, InsArg),
    AAA(OpCode, InsArg, InsArg, InsArg),
}

impl Ins {
    pub fn from_ins_args(mut op_code: OpCode, args: Vec<InsArg>) -> Ins {
        if let Some(InsArg::Imd(_)) = args.last() {
            op_code = op_code.get_imd_version();
        }
        match args.len() {
            0 => Ins::N(op_code),
            1 => Ins::A(op_code, *args.get(0).unwrap()),
            2 => Ins::AA(op_code, *args.get(0).unwrap(), *args.get(1).unwrap()),
            3 => Ins::AAA(
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
            Ins::N(op) => vec![op.to_byte()],
            Ins::A(op, ins) => vec![op.to_byte()]
                .into_iter()
                .chain(ins.to_bytes().into_iter())
                .collect(),
            Ins::AA(op, ins1, ins2) => vec![op.to_byte()]
                .into_iter()
                .chain(ins1.to_bytes().into_iter())
                .chain(ins2.to_bytes().into_iter())
                .collect(),
            Ins::AAA(op, ins1, ins2, ins3) => vec![op.to_byte()]
                .into_iter()
                .chain(ins1.to_bytes().into_iter())
                .chain(ins2.to_bytes().into_iter())
                .chain(ins3.to_bytes().into_iter())
                .collect(),
        }
    }
}
