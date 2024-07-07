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
    fn it_gets_imd_variants() {
        assert_eq!(OpCode::LOAD.get_imd(), OpCode::LOADIMD);
        assert_eq!(OpCode::ADD.get_imd(), OpCode::ADDIMD);
        assert_eq!(OpCode::MUL.get_imd(), OpCode::MULIMD);
        assert_eq!(OpCode::DIV.get_imd(), OpCode::DIVIMD);
        assert_eq!(OpCode::JMPF.get_imd(), OpCode::JMPFIMD);
        assert_eq!(OpCode::JMPB.get_imd(), OpCode::JMPBIMD);
        assert_eq!(OpCode::JCMPF.get_imd(), OpCode::JCMPFIMD);
        assert_eq!(OpCode::JCMPB.get_imd(), OpCode::JCMPBIMD);
        assert_eq!(OpCode::EQ.get_imd(), OpCode::EQIMD);
        assert_eq!(OpCode::NEQ.get_imd(), OpCode::NEQIMD);
        assert_eq!(OpCode::GT.get_imd(), OpCode::GTIMD);
        assert_eq!(OpCode::LT.get_imd(), OpCode::LTIMD);
        assert_eq!(OpCode::GE.get_imd(), OpCode::GEIMD);
        assert_eq!(OpCode::LE.get_imd(), OpCode::LEIMD);
    }
}
