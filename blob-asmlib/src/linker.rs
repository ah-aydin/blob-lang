use blob_executable::BlobExecutable;

use crate::token::Token;

struct Linker {
    tokens: Vec<Token>,
}

impl Linker {
    fn new(tokens: Vec<Token>) -> Linker {
        Linker { tokens }
    }

    fn link(&mut self) -> Result<BlobExecutable, ()> {
        todo!()
    }
}

pub fn link(tokens: Vec<Token>) -> Result<BlobExecutable, ()> {
    Linker::new(tokens).link()
}
