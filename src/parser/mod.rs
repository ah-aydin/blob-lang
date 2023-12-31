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

const CHUNK_SIZE: usize = 4096;
const INTIAL_TOKEN_CAPACITY: usize = 1024;

type StmtResult = Result<Stmt, ParserError>;
type ExprResult = Result<Expr, ParserError>;
type TokenRefResult<'a> = Result<&'a Token, ParserError>;

#[derive(Debug)]
pub enum ParserError {
    IOError(String, FileCoords),
    WrongToken(String, Token),
    EOF,
}

/// Top down parser.
///
/// Parsing rules
/// ```
/// // Statement grammar
/// stmt -> func_decl_stmt / return_stmt / if_else_stmt / var_decl_stmt / while_stmt / block_stmt / assignment_stmt
/// func_decl_stmt -> "func" "(" ")" block_stmt
/// return_stmt -> "return" expr ";"
/// if_else_stmt -> "if" "(" expr ")" block_stmt
/// var_decl_stmt -> "var" IDENTIFIER  "=" expr ";"
/// while_stmt -> "while" "(" expr ")" block_stmt
/// block_stmt -> "{" stmt* "}"
/// assignment_stmt -> ((IDENTIFIER "=")? expr | IDENTIFIER) ";"
///
/// // Expression grammar
/// expr -> logic_or
/// logic_or -> logic_and ("||" logic_and)?
/// logic_and -> comparison("&&" comparison)?
/// comparison -> term (("<" | ">" | "<=" | ">=" | "==" | "!=") term)?
/// term -> factor (("+" | "-") factor)*
/// factor -> unary (("*" | "/") unary)*
/// unary -> ("!" | "-") unary | call
/// call -> primary ("(" arguments? ")")?
/// arguments -> expr ("," expr )*
/// primary -> NUMBER | IDENTIFIER | "(" expression ")"
/// ```
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
            tokens: Vec::with_capacity(INTIAL_TOKEN_CAPACITY),
            token_index: 0,
        })
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mut stmts: Vec<Stmt> = Vec::new();
        loop {
            if self.peek_token()?.token_type == TokenType::EOF {
                break;
            }
            stmts.push(self.stmt()?);
        }
        Ok(stmts)
    }

    fn stmt(&mut self) -> StmtResult {
        match self.match_token(vec![
            TokenType::Func,
            TokenType::Return,
            TokenType::If,
            TokenType::Var,
            TokenType::While,
            TokenType::LeftBrace,
        ])? {
            Some(TokenType::Func) => self.func_decl_stmt(),
            Some(TokenType::Return) => self.return_stmt(),
            Some(TokenType::If) => self.if_else_stmt(),
            Some(TokenType::Var) => self.var_decl_stmt(),
            Some(TokenType::While) => self.while_stmt(),
            Some(TokenType::LeftBrace) => self.block_stmt(),
            Some(_) | None => self.assignment_stmt(),
        }
    }

    fn func_decl_stmt(&mut self) -> StmtResult {
        let func_name = self.consume_identifier_and_get_lexeme("Expected function name")?;

        self.consume(
            TokenType::LeftParen,
            "Expected opening '(' for function argument declarations",
        )?;
        let mut args = Vec::new();
        let mut first_pass = true;
        while self.peek_token()?.token_type != TokenType::RightParen {
            if !first_pass {
                self.consume(
                    TokenType::Comma,
                    "Expecting ',' to seperate function arguments",
                )?;
            }
            // TODO change this logic to also accept variable types after introducing
            // types other than i32
            args.push(self.consume_identifier_and_get_lexeme("Expected argument name")?);
            first_pass = false;
        }
        self.consume(
            TokenType::RightParen,
            "Expected closing ')' for function argument declrations",
        )?;

        self.consume(
            TokenType::LeftBrace,
            "Expected opening '{' for function body",
        )?;
        let func_body = self.block_stmt()?;

        Ok(Stmt::FuncDecl(func_name, args, Box::new(func_body)))
    }

    fn return_stmt(&mut self) -> StmtResult {
        let expr = self.expr()?;
        self.consume(TokenType::Semicolon, "Expected ';' to end return statement")?;
        Ok(Stmt::Return(expr))
    }

    fn if_else_stmt(&mut self) -> StmtResult {
        self.consume(
            TokenType::LeftParen,
            "Expected opening '(' for if condition",
        )?;
        if self.peek_token()?.token_type == TokenType::RightParen {
            return Err(ParserError::WrongToken(
                String::from("Expected if condition"),
                self.next_token()?.clone(),
            ));
        }
        let condition = self.expr()?;
        self.consume(
            TokenType::RightParen,
            "Expected closing')' for while conditon",
        )?;

        self.consume(TokenType::LeftBrace, "Expected opening '{' for if body")?;
        let if_clause = self.block_stmt()?;

        // Check for `else` and `else if` chains
        if self.peek_token()?.token_type == TokenType::Else {
            self.next_token()?;
            let else_clause = match self.match_token(vec![TokenType::If, TokenType::LeftBrace])? {
                Some(TokenType::If) => self.if_else_stmt(),
                Some(TokenType::LeftBrace) => self.block_stmt(),
                Some(_) | None => Err(ParserError::WrongToken(
                    String::from("Expected another 'if' statement or opening '{' for else body"),
                    self.peek_token()?.clone(),
                )),
            }?;
            return Ok(Stmt::IfElse(
                condition,
                Box::new(if_clause),
                Box::new(else_clause),
            ));
        }

        Ok(Stmt::If(condition, Box::new(if_clause)))
    }

    fn var_decl_stmt(&mut self) -> StmtResult {
        // TODO add option to specify var type after introducing types other
        // than i32
        let var_name = self.consume_identifier_and_get_lexeme("Expected variable name")?;
        self.consume(
            TokenType::Equal,
            "Must give an intial value for var declaration",
        )?;
        let expr = self.expr()?;
        self.consume(TokenType::Semicolon, "Expected ';'")?;
        Ok(Stmt::VarDecl(var_name.clone(), expr))
    }

    fn while_stmt(&mut self) -> StmtResult {
        self.consume(
            TokenType::LeftParen,
            "Expected opening '(' for while condition",
        )?;
        if self.peek_token()?.token_type == TokenType::RightParen {
            return Err(ParserError::WrongToken(
                String::from("Expected while condition"),
                self.next_token()?.clone(),
            ));
        }
        let condition = self.expr()?;
        self.consume(
            TokenType::RightParen,
            "Expected closing')' for while conditon",
        )?;
        self.consume(TokenType::LeftBrace, "Expected opening '{' for while body")?;
        let block = self.block_stmt()?;
        Ok(Stmt::While(condition, Box::new(block)))
    }

    /// Consume a "{" befor calling this function
    fn block_stmt(&mut self) -> StmtResult {
        let mut block_stmts = Vec::new();
        while self.peek_token()?.token_type != TokenType::RightBrace {
            block_stmts.push(self.stmt()?);
        }
        self.consume(TokenType::RightBrace, "Expected closing'}' for block")?;
        Ok(Stmt::Block(block_stmts))
    }

    fn assignment_stmt(&mut self) -> StmtResult {
        match self.expr()? {
            Expr::Identifier(ident) => {
                if self.peek_token()?.token_type == TokenType::Equal {
                    self.consume(TokenType::Equal, "Expected '='")?;
                    let to_expr = self.expr()?;
                    self.consume(TokenType::Semicolon, "Expected ';' after expression")?;
                    return Ok(Stmt::Assign(ident, to_expr));
                }
                self.consume(TokenType::Semicolon, "Expected ';' after expression")?;
                Ok(Stmt::ExprStmt(Expr::Identifier(ident)))
            }
            other => {
                self.consume(TokenType::Semicolon, "Expected ';' after expression")?;
                Ok(Stmt::ExprStmt(other))
            }
        }
    }

    fn expr(&mut self) -> ExprResult {
        self.logic_or()
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
            if let Expr::Identifier(ident) = possible_callee {
                self.consume(TokenType::LeftParen, "Expected opening '('")?;
                let args = self.arguments()?;
                return Ok(Expr::Call(ident, args));
            }
            return Err(ParserError::WrongToken(
                String::from("Call expression requires an identifier"),
                self.peek_token()?.clone(),
            ));
        }
        Ok(possible_callee)
    }

    fn arguments(&mut self) -> Result<Vec<Expr>, ParserError> {
        let mut args = Vec::new();
        let mut first_pass = true;
        while self.peek_token()?.token_type != TokenType::RightParen {
            if !first_pass {
                self.consume(TokenType::Comma, "Expecting ',' to seperate call arguments")?;
            }
            args.push(self.expr()?);
            first_pass = false;
        }
        self.consume(TokenType::RightParen, "Expected closing ')'")?;
        Ok(args)
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
            _ => Err(ParserError::WrongToken(
                String::from("Unexpected token"),
                token.clone(),
            )),
        }
    }

    /// Iterates to the next token and returns an error if it is not of the given type
    fn consume(&mut self, token_type: TokenType, msg: &str) -> TokenRefResult {
        let token = self.next_token()?;
        if token.token_type != token_type {
            return Err(ParserError::WrongToken(String::from(msg), token.clone()));
        }
        Ok(token)
    }

    fn consume_identifier_and_get_lexeme(&mut self, msg: &str) -> Result<String, ParserError> {
        Ok(String::from(
            self.consume(TokenType::Identifier, msg)?
                .lexeme
                .as_ref()
                .unwrap(),
        ))
    }

    /// Iterates to the next token if token is in the given types.
    /// Returns an `Option<TokenType>` for the found token and iterates
    /// to the next one if a match is found.
    fn match_token(
        &mut self,
        token_types: Vec<TokenType>,
    ) -> Result<Option<TokenType>, ParserError> {
        let peek_token_type = self.peek_token()?.token_type.clone();
        for token_type in token_types {
            if peek_token_type == token_type {
                let _ = self.next_token()?;
                return Ok(Some(token_type));
            }
        }
        Ok(None)
    }

    /// Returns the next token in the iteration, does not iterate to the next one
    fn peek_token(&mut self) -> TokenRefResult {
        if self.token_index >= self.tokens.len() {
            return self.read_next_chunk();
        }

        Ok(self.tokens.get(self.token_index).unwrap())
    }

    /// Returns the next token in the iteration, iterate to the next one
    fn next_token(&mut self) -> TokenRefResult {
        if self.token_index >= self.tokens.len() {
            return self.read_next_chunk();
        }
        let token = self.tokens.get(self.token_index).unwrap();
        self.token_index += 1;
        Ok(token)
    }

    fn read_next_chunk(&mut self) -> TokenRefResult {
        let mut chunk: Vec<u8> = vec![0; CHUNK_SIZE];
        return match self.reader.read(&mut chunk) {
            Ok(bytes_read) => {
                if bytes_read == 0 {
                    self.tokens.push(Token::eof(self.scanner.get_coords()));
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
                    .unwrap_or(&Token::eof(self.scanner.get_coords()))
                    .file_coords
                    .clone(),
            )),
        };
    }
}
