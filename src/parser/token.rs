use std::fmt::Debug;

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
    Equal,
    Pipe,
    Ampersand,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,

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

    EOF,
}

impl TokenType {
    fn requires_lexeme(&self) -> bool {
        match self {
            TokenType::Number | TokenType::Identifier => true,
            _ => false,
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Token {
    pub token_type: TokenType,
    pub line: usize,
    pub col: usize,
    pub lexeme: Option<String>,
}

impl Token {
    pub fn new(token_type: TokenType, line: usize, col: usize) -> Token {
        assert!(!token_type.requires_lexeme());
        Token {
            token_type,
            line,
            col,
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
            line,
            col,
            lexeme: Some(lexeme),
        }
    }

    pub fn eof() -> Token {
        Token {
            token_type: TokenType::EOF,
            line: 0,
            col: 0,
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
            self.line,
            self.col,
            self.lexeme.clone().unwrap_or("".to_string())
        )
    }
}
