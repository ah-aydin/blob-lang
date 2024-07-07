use blob_bc::OpCode;

#[derive(Debug, PartialEq)]
pub enum Token {
    Op { code: OpCode },
}
