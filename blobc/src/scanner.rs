use std::collections::HashMap;

use lazy_static::lazy_static;

use crate::{
    common::FileCoords,
    token::{Token, TokenType},
};

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut map = HashMap::new();

        map.insert("bool", TokenType::BTypeBool);
        map.insert("else", TokenType::Else);
        map.insert("false", TokenType::False);
        map.insert("func", TokenType::Func);
        map.insert("if", TokenType::If);
        map.insert("i64", TokenType::BTypeI64);
        map.insert("return", TokenType::Return);
        map.insert("true", TokenType::True);
        map.insert("var", TokenType::Var);
        map.insert("while", TokenType::While);

        map
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScannerError {
    IncompleteString,
}

struct Scanner {
    current_index: usize,
    file_coords: FileCoords,
}

impl Scanner {
    fn new() -> Scanner {
        Scanner {
            current_index: 0,
            file_coords: FileCoords { line: 1, col: 1 },
        }
    }

    fn scan(&mut self, src: &str) -> Result<Vec<Token>, ScannerError> {
        let mut src_iter = src.chars().peekable();
        let mut tokens = vec![];

        while let Some(_) = src_iter.peek() {
            // Skip whitespace
            loop {
                match src_iter.peek() {
                    // Whitespace and comments
                    Some(' ') | Some('\r') | Some('\t') => self.advance(),
                    Some('\n') => self.new_line(),
                    Some('#') => {
                        self.advance();
                        while let Some(c) = src_iter.peek() {
                            if *c == '\n' {
                                self.new_line();
                                src_iter.next();
                                break;
                            }
                            self.advance();
                            src_iter.next();
                        }
                    }
                    _ => break,
                };
                src_iter.next();
            }

            let token = match src_iter.next() {
                // Single character tokens
                Some('+') => Ok(self.build_single_char_token(TokenType::Plus)),
                Some('-') => Ok(self.build_single_char_token(TokenType::Minus)),
                Some('*') => Ok(self.build_single_char_token(TokenType::Star)),
                Some('/') => Ok(self.build_single_char_token(TokenType::Slash)),
                Some(';') => Ok(self.build_single_char_token(TokenType::Semicolon)),
                Some(':') => Ok(self.build_single_char_token(TokenType::Colon)),
                Some('(') => Ok(self.build_single_char_token(TokenType::LeftParen)),
                Some(')') => Ok(self.build_single_char_token(TokenType::RightParen)),
                Some('[') => Ok(self.build_single_char_token(TokenType::LeftBracket)),
                Some(']') => Ok(self.build_single_char_token(TokenType::RightBracket)),
                Some('{') => Ok(self.build_single_char_token(TokenType::LeftBrace)),
                Some('}') => Ok(self.build_single_char_token(TokenType::RightBrace)),
                Some(',') => Ok(self.build_single_char_token(TokenType::Comma)),
                Some('.') => Ok(self.build_single_char_token(TokenType::Dot)),

                // Single and double character tokens
                Some('=') => match src_iter.peek() {
                    Some('=') => {
                        src_iter.next();
                        Ok(self.build_double_char_token(TokenType::EqualEqual))
                    }
                    _ => Ok(self.build_single_char_token(TokenType::Equal)),
                },
                Some('>') => match src_iter.peek() {
                    Some('=') => {
                        src_iter.next();
                        Ok(self.build_double_char_token(TokenType::GreaterEqual))
                    }
                    _ => Ok(self.build_single_char_token(TokenType::Greater)),
                },
                Some('<') => match src_iter.peek() {
                    Some('=') => {
                        src_iter.next();
                        Ok(self.build_double_char_token(TokenType::LessEqual))
                    }
                    _ => Ok(self.build_single_char_token(TokenType::Less)),
                },
                Some('!') => match src_iter.peek() {
                    Some('=') => {
                        src_iter.next();
                        Ok(self.build_double_char_token(TokenType::BangEqual))
                    }
                    _ => Ok(self.build_single_char_token(TokenType::Bang)),
                },
                Some('|') => match src_iter.peek() {
                    Some('|') => {
                        src_iter.next();
                        Ok(self.build_double_char_token(TokenType::PipePipe))
                    }
                    _ => Ok(self.build_single_char_token(TokenType::Pipe)),
                },
                Some('&') => match src_iter.peek() {
                    Some('&') => {
                        src_iter.next();
                        Ok(self.build_double_char_token(TokenType::AmpersandAmpersand))
                    }
                    _ => Ok(self.build_single_char_token(TokenType::Ampersand)),
                },

                // Check for string literals
                Some('"') => {
                    let start_index = self.current_index;
                    let mut closed = false;
                    while let Some(c) = src_iter.peek() {
                        if *c == '"' {
                            closed = true;
                            self.advance();
                            src_iter.next();
                            break;
                        }
                        self.advance();
                        src_iter.next();
                    }
                    if !closed {
                        return Err(ScannerError::IncompleteString);
                    }
                    let lexeme = src[start_index + 1..self.current_index].to_string();
                    Ok(Token::new_with_lexeme(
                        TokenType::String,
                        self.file_coords.clone(),
                        lexeme,
                    ))
                }

                // Check for numbers, identifiers and keywords
                Some(_) => {
                    let start_index = self.current_index;
                    self.advance();

                    while let Some(c) = src_iter.peek() {
                        if !c.is_alphanumeric() && *c != '_' {
                            break;
                        }
                        self.advance();
                        src_iter.next();
                    }

                    let lexeme = src[start_index..self.current_index].to_string();
                    let file_coords = self.file_coords.new_offset(0, -(lexeme.len() as isize));
                    if lexeme.len() == 0 {
                        Ok(self.build_single_char_token(TokenType::EOF))
                    } else if lexeme.parse::<i64>().is_ok() {
                        Ok(Token::new_with_lexeme(TokenType::I64, file_coords, lexeme))
                    } else if let Some(token_type) = KEYWORDS.get(lexeme.as_str()) {
                        Ok(Token::new(*token_type, file_coords))
                    } else {
                        Ok(Token::new_with_lexeme(
                            TokenType::Identifier,
                            file_coords,
                            lexeme,
                        ))
                    }
                }

                None => Ok(Token::eof(self.file_coords.clone())),
            }?;
            tokens.push(token);
        }

        Ok(tokens)
    }

    fn advance(&mut self) {
        self.current_index += 1;
        self.file_coords.col += 1;
    }

    fn new_line(&mut self) {
        self.file_coords.line += 1;
        self.file_coords.col = 0;
        self.advance();
    }

    fn build_single_char_token(&mut self, token_type: TokenType) -> Token {
        self.advance();
        Token::new(token_type, self.file_coords.new_offset(0, -1))
    }

    fn build_double_char_token(&mut self, token_type: TokenType) -> Token {
        self.advance();
        self.advance();
        Token::new(token_type, self.file_coords.new_offset(0, -2))
    }
}

pub fn scan(src: &str) -> Result<Vec<Token>, ScannerError> {
    Scanner::new().scan(src)
}
