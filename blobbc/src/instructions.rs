#[derive(Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum OpCode {
    HLT,

    LOADIMD,
    LOADREG,

    ADD,
    SUB,
    MUL,
    DIV,

    JMP,
    JMPF,
    JMPB,
    JCMP,
    JCMPF,
    JCMPB,

    EQ,
    NEQ,
    GT,
    LT,
    GE,
    LE,

    IGL,
}

impl From<u8> for OpCode {
    fn from(v: u8) -> OpCode {
        unsafe { std::mem::transmute::<u8, OpCode>(v) }
    }
}
