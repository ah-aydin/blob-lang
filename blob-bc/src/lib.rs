#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[repr(u8)]
pub enum OpCode {
    HLT,

    LOAD,
    LOADIMD,

    ADD,
    ADDIMD,
    SUB,
    SUBIMD,
    MUL,
    MULIMD,
    DIV,
    DIVIMD,

    JMP,
    JMPF,
    JMPFIMD,
    JMPB,
    JMPBIMD,
    JCMP,
    JCMPF,
    JCMPFIMD,
    JCMPB,
    JCMPBIMD,

    EQ,
    EQIMD,
    NEQ,
    NEQIMD,
    GT,
    GTIMD,
    LT,
    LTIMD,
    GE,
    GEIMD,
    LE,
    LEIMD,

    PUSH,
    POP,

    ALOC,

    IGL,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InsArgType {
    Reg,
    Imd,
    Label,
    Memory,
}

macro_rules! arg_types {
    (reg) => {
        vec![InsArgType::Reg]
    };
    (reg, imd) => {
        vec![InsArgType::Reg, InsArgType::Imd]
    };
    (reg, imd, label) => {
        vec![InsArgType::Reg, InsArgType::Imd, InsArgType::Label]
    };
}

impl OpCode {
    pub fn get_imd_version(&self) -> OpCode {
        unsafe { std::mem::transmute::<u8, OpCode>(*self as u8 + 1) }
    }

    pub fn to_byte(&self) -> u8 {
        *self as u8
    }

    pub fn get_args_types(&self) -> Vec<Vec<InsArgType>> {
        match self {
            OpCode::HLT => vec![],
            OpCode::LOAD | OpCode::LOADIMD => {
                vec![arg_types!(reg), arg_types!(reg, imd, label)]
            }
            OpCode::ADD
            | OpCode::ADDIMD
            | OpCode::SUB
            | OpCode::SUBIMD
            | OpCode::MUL
            | OpCode::MULIMD
            | OpCode::DIV
            | OpCode::DIVIMD => vec![arg_types!(reg), arg_types!(reg), arg_types!(reg, imd)],

            OpCode::JMP | OpCode::JCMP => vec![arg_types!(reg)],
            OpCode::JMPF
            | OpCode::JMPFIMD
            | OpCode::JMPB
            | OpCode::JMPBIMD
            | OpCode::JCMPF
            | OpCode::JCMPFIMD
            | OpCode::JCMPB
            | OpCode::JCMPBIMD => vec![arg_types!(reg, imd)],

            OpCode::EQ
            | OpCode::EQIMD
            | OpCode::NEQ
            | OpCode::NEQIMD
            | OpCode::GT
            | OpCode::GTIMD
            | OpCode::LT
            | OpCode::LTIMD
            | OpCode::GE
            | OpCode::GEIMD
            | OpCode::LE
            | OpCode::LEIMD => vec![arg_types!(reg), arg_types!(reg, imd)],

            OpCode::PUSH | OpCode::POP | OpCode::ALOC => vec![arg_types!(reg)],

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
        assert_eq!(OpCode::LOAD.get_imd_version(), OpCode::LOADIMD);
        assert_eq!(OpCode::ADD.get_imd_version(), OpCode::ADDIMD);
        assert_eq!(OpCode::MUL.get_imd_version(), OpCode::MULIMD);
        assert_eq!(OpCode::DIV.get_imd_version(), OpCode::DIVIMD);
        assert_eq!(OpCode::JMPF.get_imd_version(), OpCode::JMPFIMD);
        assert_eq!(OpCode::JMPB.get_imd_version(), OpCode::JMPBIMD);
        assert_eq!(OpCode::JCMPF.get_imd_version(), OpCode::JCMPFIMD);
        assert_eq!(OpCode::JCMPB.get_imd_version(), OpCode::JCMPBIMD);
        assert_eq!(OpCode::EQ.get_imd_version(), OpCode::EQIMD);
        assert_eq!(OpCode::NEQ.get_imd_version(), OpCode::NEQIMD);
        assert_eq!(OpCode::GT.get_imd_version(), OpCode::GTIMD);
        assert_eq!(OpCode::LT.get_imd_version(), OpCode::LTIMD);
        assert_eq!(OpCode::GE.get_imd_version(), OpCode::GEIMD);
        assert_eq!(OpCode::LE.get_imd_version(), OpCode::LEIMD);
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
