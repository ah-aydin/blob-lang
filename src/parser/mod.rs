mod scanner;
mod token;

use crate::{
    ast::{expr::Expr, op_type::UnaryOpType, stmt::Stmt},
    parser::token::TokenType,
};
use std::{
    fs::File,
    io::{BufReader, Read},
};

use self::{
    scanner::Scanner,
    token::{FileCoords, Token},
};

const CHUNK_SIZE: usize = 0x1000;

type StmtResult = Result<Stmt, ParserError>;
type ExprResult = Result<Expr, ParserError>;

#[derive(Debug)]
pub enum ParserError {
    IOError(String, FileCoords),
    MissingToken(String, FileCoords),
    EOF,
}

/// Top down parser.
///
/// Parsing rules
/// ```
/// // Statement grammar
/// expr_stmt-> exrp ";"
///
/// // Expression grammar
/// expr -> assignemnt
/// assignment -> IDENTIFIER "=" logic_or | logic_or
/// logic_or -> logic_and ("||" logic_and)?
/// logic_and -> comparison("&&" comparison)?
/// comparison -> term (("<" | ">" | "<=" | ">=" | "==" | "!=") term)?
/// term -> factor (("+" | "-") factor)*
/// factor -> unary (("*" | "/") unary)*
/// unary -> ("!" | "-") unary | call
/// call -> primary ("(" arguments? ")")?
/// arguments -> logic_or ("," logic_or)*
/// primary -> NUMBER | IDENTIFIER | "(" expression ")"
// ```
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

    pub fn parse(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mut stmts: Vec<Stmt> = Vec::new();
        loop {
            if self.peek_token()?.token_type == TokenType::EOF {
                break;
            }
            stmts.push(self.expr_stmt()?);
        }
        Ok(stmts)
    }

    fn expr_stmt(&mut self) -> StmtResult {
        let expr = self.expr()?;
        self.consume(TokenType::Semicolon, "Expected ';' after expression")?;
        Ok(Stmt::ExprStmt(expr))
    }

    fn expr(&mut self) -> ExprResult {
        self.assignment()
    }

    fn assignment(&mut self) -> ExprResult {
        match self.logic_or()? {
            Expr::Identifier(ident) => {
                if self.peek_token()?.token_type == TokenType::Equal {
                    self.consume(TokenType::Equal, "Expected '='")?;
                    let to_expr = self.logic_or()?;
                    return Ok(Expr::Assign(ident, Box::new(to_expr)));
                }
                Ok(Expr::Identifier(ident))
            }
            other => Ok(other),
        }
    }

    fn logic_or(&mut self) -> ExprResult {
        let possible_left_term = self.logic_and()?;
        if let Some(token_type) = self.match_token(vec![TokenType::PipePipe])? {
            let bin_op_type = token_type.get_bin_op_type();
            let right_term = self.logic_and()?;
            return Ok(Expr::BinaryOp(
                Box::new(possible_left_term),
                bin_op_type,
                Box::new(right_term),
            ));
        }
        Ok(possible_left_term)
    }

    fn logic_and(&mut self) -> ExprResult {
        let possible_left_term = self.comparison()?;
        if let Some(token_type) = self.match_token(vec![TokenType::AmpersandAmpersand])? {
            let bin_op_type = token_type.get_bin_op_type();
            let right_term = self.comparison()?;
            return Ok(Expr::BinaryOp(
                Box::new(possible_left_term),
                bin_op_type,
                Box::new(right_term),
            ));
        }
        Ok(possible_left_term)
    }

    fn comparison(&mut self) -> ExprResult {
        let possible_left_term = self.term()?;
        if let Some(token_type) = self.match_token(vec![
            TokenType::Less,
            TokenType::Greater,
            TokenType::LessEqual,
            TokenType::GreaterEqual,
            TokenType::EqualEqual,
            TokenType::BangEqual,
        ])? {
            let bin_op_type = token_type.get_bin_op_type();
            let right_term = self.term()?;
            return Ok(Expr::BinaryOp(
                Box::new(possible_left_term),
                bin_op_type,
                Box::new(right_term),
            ));
        }
        Ok(possible_left_term)
    }

    fn term(&mut self) -> ExprResult {
        let mut expr = self.factor()?;
        while let Some(token_type) = self.match_token(vec![TokenType::Plus, TokenType::Minus])? {
            let bin_op_type = token_type.get_bin_op_type();
            let right_term = self.factor()?;
            expr = Expr::BinaryOp(Box::new(expr), bin_op_type, Box::new(right_term));
        }
        Ok(expr)
    }

    fn factor(&mut self) -> ExprResult {
        let mut expr = self.unary()?;
        while let Some(token_type) = self.match_token(vec![TokenType::Star, TokenType::Slash])? {
            let bin_op_type = token_type.get_bin_op_type();
            let right_term = self.unary()?;
            expr = Expr::BinaryOp(Box::new(expr), bin_op_type, Box::new(right_term));
        }
        Ok(expr)
    }

    fn unary(&mut self) -> ExprResult {
        match self.peek_token()?.token_type {
            TokenType::Bang => {
                self.consume(TokenType::Bang, "Expected '!'")?;
                let term = self.unary()?;
                Ok(Expr::UnaryOp(UnaryOpType::Not, Box::new(term)))
            }
            TokenType::Minus => {
                self.consume(TokenType::Minus, "Expected '-'")?;
                let term = self.unary()?;
                Ok(Expr::UnaryOp(UnaryOpType::Negate, Box::new(term)))
            }
            _ => self.call(),
        }
    }

    fn call(&mut self) -> ExprResult {
        let possible_callee = self.primary()?;
        if self.peek_token()?.token_type == TokenType::LeftParen {
            self.consume(TokenType::LeftParen, "Expected opening '('")?;
            let args = self.arguments()?;
            self.consume(TokenType::RightParen, "Expected closing ')'")?;
            return Ok(Expr::Call(Box::new(possible_callee), args));
        }
        Ok(possible_callee)
    }

    fn arguments(&mut self) -> Result<Vec<Expr>, ParserError> {
        todo!("arguments")
    }

    fn primary(&mut self) -> ExprResult {
        let token = self.next_token()?;
        match token.token_type {
            TokenType::Number => Ok(Expr::I32(
                token.lexeme.as_ref().unwrap().parse::<i32>().unwrap(),
            )),
            TokenType::Identifier => Ok(Expr::Identifier(token.lexeme.as_ref().unwrap().clone())),
            TokenType::LeftParen => {
                let expr = self.expr()?;
                self.consume(TokenType::RightParen, "Missing closing ')'")?;
                Ok(expr)
            }
            TokenType::EOF => Err(ParserError::EOF),
            _ => unreachable!("Got an unexpected token in `primary` {:?}", token),
        }
    }

    /// Iterates to the next token and returns an error if it is not of the given type
    fn consume(&mut self, token_type: TokenType, msg: &str) -> Result<(), ParserError> {
        let token = self.next_token()?;
        if token.token_type != token_type {
            return Err(ParserError::MissingToken(
                String::from(msg),
                token.file_coords.clone(),
            ));
        }
        Ok(())
    }

    fn match_token(
        &mut self,
        token_types: Vec<TokenType>,
    ) -> Result<Option<TokenType>, ParserError> {
        for token_type in token_types {
            if self.peek_token()?.token_type == token_type {
                let _ = self.next_token()?;
                return Ok(Some(token_type));
            }
        }
        Ok(None)
    }

    fn peek_token(&mut self) -> Result<&Token, ParserError> {
        if self.token_index >= self.tokens.len() {
            let mut chunk: Vec<u8> = vec![0; CHUNK_SIZE];
            return match self.reader.read(&mut chunk) {
                Ok(bytes_read) => {
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
                Err(_) => Err(ParserError::IOError(
                    String::from("Failed to read next chunk from line"),
                    self.tokens
                        .last()
                        .unwrap_or(&Token::eof())
                        .file_coords
                        .clone(),
                )),
            };
        }

        Ok(self.tokens.get(self.token_index).unwrap())
    }

    fn next_token(&mut self) -> Result<&Token, ParserError> {
        if self.token_index >= self.tokens.len() {
            let mut chunk: Vec<u8> = vec![0; CHUNK_SIZE];
            return match self.reader.read(&mut chunk) {
                Ok(bytes_read) => {
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
                Err(_) => Err(ParserError::IOError(
                    String::from("Failed to read next chunk from line"),
                    self.tokens
                        .last()
                        .unwrap_or(&Token::eof())
                        .file_coords
                        .clone(),
                )),
            };
        }
        let token = self.tokens.get(self.token_index).unwrap();
        self.token_index += 1;
        Ok(token)
    }
}
