use std::fmt::Debug;

use ast::{
    op_type::{BinaryOpType, BooleanOpType},
    FileCoords,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
    // Single character
    Plus,
    Minus,
    Slash,
    Star,
    Bang,
    Greater,
    Less,
    Semicolon,
    Colon,
    Equal,
    Pipe,
    Ampersand,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    Comma,

    // Double character
    EqualEqual,
    GreaterEqual,
    LessEqual,
    BangEqual,
    AmpersandAmpersand,
    PipePipe,

    // Literal
    True,
    False,
    Number,
    Identifier,

    // Keywords
    Else,
    Func,
    If,
    Return,
    Var,
    While,

    // Types
    Bool,
    I32,

    Blank,

    EOF,
}

impl TokenType {
    fn requires_lexeme(&self) -> bool {
        match self {
            TokenType::Number | TokenType::Identifier => true,
            _ => false,
        }
    }

    pub fn get_bin_op_type(&self) -> BinaryOpType {
        match self {
            TokenType::Plus => BinaryOpType::Add,
            TokenType::Minus => BinaryOpType::Subtract,
            TokenType::Slash => BinaryOpType::Divide,
            TokenType::Star => BinaryOpType::Multiply,
            _ => unreachable!("{:?} does not have a BinaryOpType", self),
        }
    }

    pub fn get_boolean_op_type(&self) -> BooleanOpType {
        match self {
            TokenType::AmpersandAmpersand => BooleanOpType::And,
            TokenType::PipePipe => BooleanOpType::Or,
            TokenType::Greater => BooleanOpType::Greater,
            TokenType::Less => BooleanOpType::Less,
            TokenType::EqualEqual => BooleanOpType::Equal,
            TokenType::GreaterEqual => BooleanOpType::GreaterOrEqual,
            TokenType::LessEqual => BooleanOpType::LessOrEqual,
            TokenType::BangEqual => BooleanOpType::NotEqual,
            _ => unreachable!("{:?} does not have a BooleanOpType", self),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Token {
    pub token_type: TokenType,
    pub file_coords: FileCoords,
    pub lexeme: Option<String>,
}

impl Token {
    pub fn new(token_type: TokenType, line: usize, col: usize) -> Token {
        assert!(!token_type.requires_lexeme());
        Token {
            token_type,
            file_coords: FileCoords { line, col },
            lexeme: None,
        }
    }

    pub fn new_with_lexeme(
        token_type: TokenType,
        line: usize,
        col: usize,
        lexeme: String,
    ) -> Token {
        assert!(token_type.requires_lexeme());
        Token {
            token_type,
            file_coords: FileCoords { line, col },
            lexeme: Some(lexeme),
        }
    }

    pub fn eof(file_coords: FileCoords) -> Token {
        Token {
            token_type: TokenType::EOF,
            file_coords,
            lexeme: None,
        }
    }

    pub fn blank(file_coords: FileCoords) -> Token {
        Token {
            token_type: TokenType::Blank,
            file_coords,
            lexeme: None,
        }
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?} ({}, {}) {}",
            self.token_type,
            self.file_coords.line,
            self.file_coords.col,
            self.lexeme.clone().unwrap_or("".to_string())
        )
    }
}
