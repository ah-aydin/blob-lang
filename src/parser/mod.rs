mod scanner;
mod token;

use crate::{ast::stmt::Stmt, parser::token::TokenType};
use std::{
    fs::File,
    io::{BufReader, Read},
};

use self::{scanner::Scanner, token::Token};

const CHUNK_SIZE: usize = 0x1000;

pub struct Parser {
    scanner: Scanner,
    reader: BufReader<File>,
    tokens: Vec<Token>,
    token_index: usize,
}

impl Parser {
    pub fn new(file_path: &str) -> Result<Parser, std::io::Error> {
        Ok(Parser {
            scanner: Scanner::new(),
            reader: BufReader::new(File::open(file_path)?),
            tokens: vec![],
            token_index: 0,
        })
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, std::io::Error> {
        // TODO remove this testing code
        loop {
            let token = self.next_token()?;
            if token.token_type == TokenType::EOF {
                break;
            }
            println!("{:?}", token);
        }
        Ok(vec![])
    }

    fn next_token(&mut self) -> Result<&Token, std::io::Error> {
        self.token_index += 1;
        if self.token_index >= self.tokens.len() {
            let mut chunk: Vec<u8> = vec![0; CHUNK_SIZE];
            let bytes_read = self.reader.read(&mut chunk)?;

            if bytes_read == 0 {
                self.tokens.push(Token::eof());
                return Ok(self.tokens.last().unwrap());
            }

            self.tokens = self
                .scanner
                .scan(&String::from_utf8_lossy(&chunk[..bytes_read]));
            self.token_index = 0;
            return Ok(self.tokens.get(self.token_index).unwrap());
        }

        Ok(self.tokens.get(self.token_index).unwrap())
    }
}
