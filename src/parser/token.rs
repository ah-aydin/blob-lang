use std::fmt::Debug;

use crate::ast::op_type::BinaryOpType;

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
    Number,
    Identifier,

    // Keywords
    Else,
    Func,
    If,
    Return,
    Var,
    While,

    EOF,
}

#[derive(Default, Debug, PartialEq, Eq, Clone)]
pub struct FileCoords {
    pub line: usize,
    pub col: usize,
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
            TokenType::Greater => BinaryOpType::Greater,
            TokenType::Less => BinaryOpType::Less,
            TokenType::EqualEqual => BinaryOpType::Equal,
            TokenType::GreaterEqual => BinaryOpType::GreaterOrEqual,
            TokenType::LessEqual => BinaryOpType::LessOrEqual,
            TokenType::BangEqual => BinaryOpType::NotEqual,
            TokenType::AmpersandAmpersand => BinaryOpType::And,
            TokenType::PipePipe => BinaryOpType::Or,
            _ => unreachable!("{:?} does not have a BinaryOpType", self),
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
