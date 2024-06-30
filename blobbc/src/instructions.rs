#[derive(Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum OpCode {
    HLT,

    LOADIMD,
    LOADREG,

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
    JMPB,
    JCMP,
    JCMPF,
    JCMPB,

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

    IGL,
}

impl From<u8> for OpCode {
    fn from(v: u8) -> OpCode {
        unsafe { std::mem::transmute::<u8, OpCode>(v) }
    }
}
