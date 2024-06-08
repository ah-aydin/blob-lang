use std::fmt::Debug;

use crate::{
    ast::{
        btype::BType,
        op::{BinaryOp, BitwiseOp, BooleanOp, CmpOp, UnaryOp},
    },
    common::FileCoords,
};

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
    Var,
    While,

    // Types
    BTypeBool,
    BTypeI64,
    BTypeStr,

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
            _ => unreachable!("{:?} does not have a BinaryOp", self),
        }
    }

    pub fn get_bitwise_op_type(&self) -> BitwiseOp {
        match self {
            TokenType::Ampersand => BitwiseOp::And,
            TokenType::Pipe => BitwiseOp::Or,
            _ => unreachable!("{:?} does not have a BitwiseOp", self),
        }
    }

    pub fn get_boolean_op_type(&self) -> BooleanOp {
        match self {
            TokenType::AmpersandAmpersand => BooleanOp::And,
            TokenType::PipePipe => BooleanOp::Or,
            _ => unreachable!("{:?} does not have a BooleanOpType", self),
        }
    }

    pub fn get_cmp_op_type(&self) -> CmpOp {
        match self {
            TokenType::Greater => CmpOp::Gt,
            TokenType::GreaterEqual => CmpOp::Gte,
            TokenType::Less => CmpOp::Lt,
            TokenType::LessEqual => CmpOp::Lte,
            TokenType::EqualEqual => CmpOp::Eq,
            TokenType::BangEqual => CmpOp::Neq,
            _ => unreachable!("{:?} does not have a CmpOp", self),
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
            TokenType::BTypeStr => BType::Str,
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
