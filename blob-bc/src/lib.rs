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

    ALOC,

    IGL,
}

impl OpCode {
    pub fn get_imd(&self) -> OpCode {
        unsafe { std::mem::transmute::<u8, OpCode>(*self as u8 + 1) }
    }
}

impl From<u8> for OpCode {
    fn from(v: u8) -> OpCode {
        unsafe { std::mem::transmute::<u8, OpCode>(v) }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn it_gets_imd_variant() {
        assert_eq!(OpCode::ADD.get_imd(), OpCode::ADDIMD);
        assert_eq!(OpCode::SUB.get_imd(), OpCode::SUBIMD);
    }
}
