use blob_bc::OpCode;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    Op { code: OpCode },
    Reg { num: u8 },
    ImdVal { val: i32 },
}
