use super::token::{Token, TokenType};
use std::{iter::Peekable, str::Chars};

pub struct Scanner<'a> {
    source_chunk: Peekable<Chars<'a>>,
    current_index: usize,
    start_index: usize,
    line: usize,
    col: usize,
    /// ```
    /// (last_in_progress_lexeme: String, last_in_progress_token_type: TokenType)
    /// ```
    last_in_progress: Option<(String, TokenType)>,
}

impl<'a> Scanner<'a> {
    pub fn new() -> Scanner<'a> {
        Scanner {
            source_chunk: "".chars().peekable(),
            current_index: 0,
            start_index: 0,
            line: 1,
            col: 1,
            last_in_progress: None,
        }
    }

    pub fn scan(&mut self, source_chunk: &'a str) -> Vec<Token> {
        self.source_chunk = source_chunk.chars().peekable();
        self.current_index = 0;
        self.start_index = 0;
        let mut tokens = vec![];

        // Process the token that is in seperate chunks
        if let Some(token) = match self.last_in_progress {
            Some((_, TokenType::Number)) => self.number('1'),
            Some((_, TokenType::Identifier)) => self.identifier('a'),
            _ => None,
        } {
            tokens.push(token);
        }
        self.last_in_progress = None;

        while let Some(c) = self.source_chunk.next() {
            self.start_index = self.current_index;
            match c {
                // Whitespaces and comments
                ' ' | '\r' | '\t' => {
                    self.new_char();
                }
                '\n' => {
                    self.new_line();
                }
                '#' => self.comment(),

                // Single character tokens
                '+' => tokens.push(self.build_single_char_token(TokenType::Plus)),
                '-' => tokens.push(self.build_single_char_token(TokenType::Minus)),
                '*' => tokens.push(self.build_single_char_token(TokenType::Star)),
                '/' => tokens.push(self.build_single_char_token(TokenType::Slash)),
                ';' => tokens.push(self.build_single_char_token(TokenType::Semicolon)),
                '(' => tokens.push(self.build_single_char_token(TokenType::LeftParen)),
                ')' => tokens.push(self.build_single_char_token(TokenType::RightParen)),
                '[' => tokens.push(self.build_single_char_token(TokenType::LeftBrace)),
                ']' => tokens.push(self.build_single_char_token(TokenType::RightBrace)),
                '{' => tokens.push(self.build_single_char_token(TokenType::LeftBrace)),
                '}' => tokens.push(self.build_single_char_token(TokenType::RightBrace)),

                // Single and double character tokens
                '=' => match self.source_chunk.peek() {
                    Some('=') => {
                        self.source_chunk.next();
                        tokens.push(self.build_double_char_token(TokenType::EqualEqual));
                    }
                    _ => tokens.push(self.build_single_char_token(TokenType::Equal)),
                },
                '>' => match self.source_chunk.peek() {
                    Some('=') => {
                        self.source_chunk.next();
                        tokens.push(self.build_double_char_token(TokenType::GreaterEqual));
                    }
                    _ => tokens.push(self.build_single_char_token(TokenType::Greater)),
                },
                '<' => match self.source_chunk.peek() {
                    Some('=') => {
                        self.source_chunk.next();
                        tokens.push(self.build_double_char_token(TokenType::LessEqual));
                    }
                    _ => tokens.push(self.build_single_char_token(TokenType::Less)),
                },
                '!' => match self.source_chunk.peek() {
                    Some('=') => {
                        self.source_chunk.next();
                        tokens.push(self.build_double_char_token(TokenType::BangEqual));
                    }
                    _ => tokens.push(self.build_single_char_token(TokenType::Bang)),
                },
                '|' => match self.source_chunk.peek() {
                    Some('|') => {
                        self.source_chunk.next();
                        tokens.push(self.build_double_char_token(TokenType::PipePipe));
                    }
                    _ => tokens.push(self.build_single_char_token(TokenType::Pipe)),
                },
                '&' => match self.source_chunk.peek() {
                    Some('&') => {
                        self.source_chunk.next();
                        tokens.push(self.build_double_char_token(TokenType::AmpersandAmpersand));
                    }
                    _ => tokens.push(self.build_single_char_token(TokenType::Ampersand)),
                },

                _ => {
                    self.new_char();
                    if let Some(token) = self.number(c) {
                        tokens.push(token);
                    } else if let Some(token) = self.identifier(c) {
                        tokens.push(token);
                    }
                }
            }
        }

        tokens
    }

    fn comment(&mut self) {
        self.new_char();
        while let Some(c) = self.source_chunk.peek() {
            if *c == '\n' {
                self.new_line();
                self.source_chunk.next();
                break;
            }
            self.new_char();
            self.source_chunk.next();
        }
    }

    fn number(&mut self, c: char) -> Option<Token> {
        if !c.is_numeric() {
            return None;
        }
        let mut lexeme = String::with_capacity(32);
        let mut is_end_of_source = true;
        if self.last_in_progress.is_none() {
            lexeme.push(c);
        }

        while let Some(c) = self.source_chunk.peek() {
            let c = c.clone();
            if !c.is_numeric() {
                is_end_of_source = false;
                break;
            }
            self.source_chunk.next();
            self.new_char();
            lexeme.push(c);
        }

        if is_end_of_source {
            self.last_in_progress = Some((lexeme, TokenType::Number));
            println!("Setting in progress to {:?}", self.last_in_progress);
            return None;
        }
        if let Some((last_lexeme, _)) = &self.last_in_progress {
            lexeme = String::from(last_lexeme) + &lexeme;
        }

        Some(self.build_token_with_lexeme(TokenType::Number, lexeme))
    }

    fn identifier(&mut self, c: char) -> Option<Token> {
        if !c.is_ascii_alphabetic() && c != '_' {
            return None;
        }
        let mut lexeme = String::with_capacity(32);
        let mut is_end_of_source = true;
        if self.last_in_progress.is_none() {
            lexeme.push(c);
        }

        while let Some(c) = self.source_chunk.peek() {
            let c = c.clone();
            if !c.is_ascii_alphabetic() && c != '_' && !c.is_numeric() {
                is_end_of_source = false;
                break;
            }
            self.source_chunk.next();
            self.new_char();
            lexeme.push(c);
        }

        if is_end_of_source {
            self.last_in_progress = Some((lexeme, TokenType::Identifier));
            println!("Setting in progress to {:?}", self.last_in_progress);
            return None;
        }
        if let Some((last_lexeme, _)) = &self.last_in_progress {
            lexeme = String::from(last_lexeme) + &lexeme;
        }

        Some(self.build_token_with_lexeme(TokenType::Identifier, lexeme))
    }

    fn new_line(&mut self) {
        self.line += 1;
        self.col = 1;
        self.current_index += 1;
    }

    fn new_char(&mut self) {
        self.col += 1;
        self.current_index += 1;
    }

    fn build_token(&mut self, token_type: TokenType) -> Token {
        let offset;
        match &self.last_in_progress {
            Some((lexeme, _)) => offset = lexeme.len(),
            None => offset = 0,
        };
        Token::new(
            token_type,
            self.line,
            self.col - (self.current_index - self.start_index) - offset,
        )
    }

    fn build_token_with_lexeme(&mut self, token_type: TokenType, lexeme: String) -> Token {
        let offset;
        match &self.last_in_progress {
            Some((lexeme, _)) => offset = lexeme.len(),
            None => offset = 0,
        };
        Token::new_with_lexeme(
            token_type,
            self.line,
            self.col - (self.current_index - self.start_index) - offset,
            lexeme,
        )
    }

    fn build_single_char_token(&mut self, token_type: TokenType) -> Token {
        self.new_char();
        self.build_token(token_type)
    }

    fn build_double_char_token(&mut self, token_type: TokenType) -> Token {
        self.new_char();
        self.new_char();
        self.build_token(token_type)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn math_tokens() {
        let mut scanner = Scanner::new();
        let code = "45 + 3;";
        let tokens = scanner.scan(code);
        assert_eq!(
            tokens,
            vec![
                Token::new_with_lexeme(TokenType::Number, 1, 1, String::from("45")),
                Token::new(TokenType::Plus, 1, 4),
                Token::new_with_lexeme(TokenType::Number, 1, 6, String::from("3")),
                Token::new(TokenType::Semicolon, 1, 7),
            ]
        );
    }

    #[test]
    fn multi_line_math_tokens() {
        let mut scanner = Scanner::new();
        let code = "45 + 3;\n\
        55 + 44;";
        let tokens = scanner.scan(code);
        assert_eq!(
            tokens,
            vec![
                Token::new_with_lexeme(TokenType::Number, 1, 1, String::from("45")),
                Token::new(TokenType::Plus, 1, 4),
                Token::new_with_lexeme(TokenType::Number, 1, 6, String::from("3")),
                Token::new(TokenType::Semicolon, 1, 7),
                Token::new_with_lexeme(TokenType::Number, 2, 1, String::from("55")),
                Token::new(TokenType::Plus, 2, 4),
                Token::new_with_lexeme(TokenType::Number, 2, 6, String::from("44")),
                Token::new(TokenType::Semicolon, 2, 8),
            ]
        );
    }

    #[test]
    fn comments() {
        let mut scanner = Scanner::new();
        let code = "45 + 3;#Some comment\n\
        55 + 44; #Some other comment";
        let tokens = scanner.scan(code);
        assert_eq!(
            tokens,
            vec![
                Token::new_with_lexeme(TokenType::Number, 1, 1, String::from("45")),
                Token::new(TokenType::Plus, 1, 4),
                Token::new_with_lexeme(TokenType::Number, 1, 6, String::from("3")),
                Token::new(TokenType::Semicolon, 1, 7),
                Token::new_with_lexeme(TokenType::Number, 2, 1, String::from("55")),
                Token::new(TokenType::Plus, 2, 4),
                Token::new_with_lexeme(TokenType::Number, 2, 6, String::from("44")),
                Token::new(TokenType::Semicolon, 2, 8),
            ]
        );
    }

    #[test]
    fn comments_end_of_chunk() {
        let mut scanner = Scanner::new();
        let code = "45 + 3;# Some comment\n\
        55 + 44; # Some other comment";
        let tokens = scanner.scan(code);
        assert_eq!(
            tokens,
            vec![
                Token::new_with_lexeme(TokenType::Number, 1, 1, String::from("45")),
                Token::new(TokenType::Plus, 1, 4),
                Token::new_with_lexeme(TokenType::Number, 1, 6, String::from("3")),
                Token::new(TokenType::Semicolon, 1, 7),
                Token::new_with_lexeme(TokenType::Number, 2, 1, String::from("55")),
                Token::new(TokenType::Plus, 2, 4),
                Token::new_with_lexeme(TokenType::Number, 2, 6, String::from("44")),
                Token::new(TokenType::Semicolon, 2, 8),
            ]
        );
    }

    #[test]
    fn scan_2_chunks() {
        let mut scanner = Scanner::new();
        let code_chunk_1 = "45 + 3;\n\
        55 + 44;\n";
        let tokens = scanner.scan(code_chunk_1);
        assert_eq!(
            tokens,
            vec![
                Token::new_with_lexeme(TokenType::Number, 1, 1, String::from("45")),
                Token::new(TokenType::Plus, 1, 4),
                Token::new_with_lexeme(TokenType::Number, 1, 6, String::from("3")),
                Token::new(TokenType::Semicolon, 1, 7),
                Token::new_with_lexeme(TokenType::Number, 2, 1, String::from("55")),
                Token::new(TokenType::Plus, 2, 4),
                Token::new_with_lexeme(TokenType::Number, 2, 6, String::from("44")),
                Token::new(TokenType::Semicolon, 2, 8),
            ]
        );

        let code_chunk_2 = "22 - 4;\n\
        77 *66;";
        let tokens = scanner.scan(code_chunk_2);
        assert_eq!(
            tokens,
            vec![
                Token::new_with_lexeme(TokenType::Number, 3, 1, String::from("22")),
                Token::new(TokenType::Minus, 3, 4),
                Token::new_with_lexeme(TokenType::Number, 3, 6, String::from("4")),
                Token::new(TokenType::Semicolon, 3, 7),
                Token::new_with_lexeme(TokenType::Number, 4, 1, String::from("77")),
                Token::new(TokenType::Star, 4, 4),
                Token::new_with_lexeme(TokenType::Number, 4, 5, String::from("66")),
                Token::new(TokenType::Semicolon, 4, 7),
            ]
        );
    }

    #[test]
    fn double_character_tokens() {
        let mut scanner = Scanner::new();
        let code = "(var1 <= var2 && var2 >= theotherone || var5 < 5)";
        let tokens = scanner.scan(code);
        assert_eq!(
            tokens,
            vec![
                Token::new(TokenType::LeftParen, 1, 1),
                Token::new_with_lexeme(TokenType::Identifier, 1, 2, String::from("var1")),
                Token::new(TokenType::LessEqual, 1, 7),
                Token::new_with_lexeme(TokenType::Identifier, 1, 10, String::from("var2")),
                Token::new(TokenType::AmpersandAmpersand, 1, 15),
                Token::new_with_lexeme(TokenType::Identifier, 1, 18, String::from("var2")),
                Token::new(TokenType::GreaterEqual, 1, 23),
                Token::new_with_lexeme(TokenType::Identifier, 1, 26, String::from("theotherone")),
                Token::new(TokenType::PipePipe, 1, 38),
                Token::new_with_lexeme(TokenType::Identifier, 1, 41, String::from("var5")),
                Token::new(TokenType::Less, 1, 46),
                Token::new_with_lexeme(TokenType::Number, 1, 48, String::from("5")),
                Token::new(TokenType::RightParen, 1, 49),
            ]
        );
    }

    #[test]
    fn token_at_end_of_chunk() {
        let mut scanner = Scanner::new();
        let code_chunk1 = "var1 + var2"; // var2 should be present in the next batch of tokens
        let tokens = scanner.scan(code_chunk1);
        assert_eq!(
            tokens,
            vec![
                Token::new_with_lexeme(TokenType::Identifier, 1, 1, String::from("var1")),
                Token::new(TokenType::Plus, 1, 6),
            ]
        );

        let code_chunk2 = "; var4 + var3;";
        let tokens = scanner.scan(code_chunk2);
        assert_eq!(
            tokens,
            vec![
                Token::new_with_lexeme(TokenType::Identifier, 1, 8, String::from("var2")),
                Token::new(TokenType::Semicolon, 1, 12),
                Token::new_with_lexeme(TokenType::Identifier, 1, 14, String::from("var4")),
                Token::new(TokenType::Plus, 1, 19),
                Token::new_with_lexeme(TokenType::Identifier, 1, 21, String::from("var3")),
                Token::new(TokenType::Semicolon, 1, 25),
            ]
        );
    }

    #[test]
    fn token_split_in_2_chunks() {
        let mut scanner = Scanner::new();
        let code_chunk1 = "var1 + va"; // var2 should be present in the next batch of tokens
        let tokens = scanner.scan(code_chunk1);
        assert_eq!(
            tokens,
            vec![
                Token::new_with_lexeme(TokenType::Identifier, 1, 1, String::from("var1")),
                Token::new(TokenType::Plus, 1, 6),
            ]
        );

        let code_chunk2 = "; var4 + var3;";
        let tokens = scanner.scan(code_chunk2);
        assert_eq!(
            tokens,
            vec![
                Token::new_with_lexeme(TokenType::Identifier, 1, 8, String::from("var2")),
                Token::new(TokenType::Semicolon, 1, 12),
                Token::new_with_lexeme(TokenType::Identifier, 1, 14, String::from("var4")),
                Token::new(TokenType::Plus, 1, 19),
                Token::new_with_lexeme(TokenType::Identifier, 1, 21, String::from("var3")),
                Token::new(TokenType::Semicolon, 1, 25),
            ]
        );
    }
}
