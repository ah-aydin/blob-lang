use blobbc::OpCode;

#[derive(Debug, PartialEq)]
pub enum Token {
    Op { code: OpCode },
}
