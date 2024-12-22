use std::fmt::Debug;

use crate::ast::{
    btype::BType,
    op::{BinaryOp, UnaryOp},
};

use crate::file_coords::FileCoords;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
    Dot,

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
    I64,
    String,
    Identifier,

    // Keywords
    Else,
    Func,
    If,
    Return,
    Struct,
    Var,
    While,

    // Types
    BTypeBool,
    BTypeI64,

    EOF,
}

impl TokenType {
    fn requires_lexeme(&self) -> bool {
        match self {
            TokenType::I64 | TokenType::String | TokenType::Identifier => true,
            _ => false,
        }
    }

    pub fn get_bin_op_type(&self) -> BinaryOp {
        match self {
            TokenType::Plus => BinaryOp::Add,
            TokenType::Minus => BinaryOp::Sub,
            TokenType::Slash => BinaryOp::Div,
            TokenType::Star => BinaryOp::Mul,
            TokenType::AmpersandAmpersand => BinaryOp::BooleanAnd,
            TokenType::PipePipe => BinaryOp::BooleanOr,
            TokenType::Ampersand => BinaryOp::BitwiseAnd,
            TokenType::Pipe => BinaryOp::BitwiseOr,
            TokenType::Greater => BinaryOp::Gt,
            TokenType::GreaterEqual => BinaryOp::Gte,
            TokenType::Less => BinaryOp::Lt,
            TokenType::LessEqual => BinaryOp::Lte,
            TokenType::EqualEqual => BinaryOp::Eq,
            TokenType::BangEqual => BinaryOp::Neq,
            _ => unreachable!("{:?} does not have a BinaryOp", self),
        }
    }

    pub fn get_unary_op_type(&self) -> UnaryOp {
        match self {
            TokenType::Bang => UnaryOp::Not,
            TokenType::Minus => UnaryOp::Neg,
            _ => unreachable!("{:?} does not have a UnaryOp", self),
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
    pub fn new(token_type: TokenType, file_coords: FileCoords) -> Token {
        assert!(
            !token_type.requires_lexeme(),
            "Did not expecdt a lexeme for {:?} {}",
            token_type,
            file_coords
        );
        Token {
            token_type,
            file_coords,
            lexeme: None,
        }
    }

    pub fn new_with_lexeme(
        token_type: TokenType,
        file_coords: FileCoords,
        lexeme: String,
    ) -> Token {
        assert!(
            token_type.requires_lexeme(),
            "Expected a lexeme for {:?} {}",
            token_type,
            file_coords
        );
        Token {
            token_type,
            file_coords,
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

    pub fn get_btype(&self) -> BType {
        match self.token_type {
            TokenType::BTypeBool => BType::Bool,
            TokenType::BTypeI64 => BType::I64,
            TokenType::Identifier => BType::Struct(self.lexeme.as_ref().unwrap().clone()),
            _ => unreachable!("{:?} does not have a BType", self.token_type),
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
            self.lexeme.clone().unwrap_or_default()
        )
    }
}
