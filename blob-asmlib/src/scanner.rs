use std::collections::HashMap;

use crate::{
    token::{DirectiveType, SectionType, Token, TokenType},
    LR_REG, SP_REG,
};
use blob_bc::OpCode;
use blob_common::{error, file_coords::FileCoords, info};
use lazy_static::lazy_static;

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut map = HashMap::new();

        map.insert("sp", TokenType::Reg(SP_REG as u8));
        map.insert("lr", TokenType::Reg(LR_REG as u8));

        map.insert("hlt", TokenType::Op(OpCode::Hlt));

        map.insert("load", TokenType::Op(OpCode::Load));

        map.insert("add", TokenType::Op(OpCode::Add));
        map.insert("sub", TokenType::Op(OpCode::Sub));
        map.insert("mul", TokenType::Op(OpCode::Mul));
        map.insert("div", TokenType::Op(OpCode::Div));

        map.insert("jmp", TokenType::Op(OpCode::Jmp));
        map.insert("jmpf", TokenType::Op(OpCode::JmpF));
        map.insert("jmpb", TokenType::Op(OpCode::JmpB));
        map.insert("jcmp", TokenType::Op(OpCode::JCmp));
        map.insert("jcmpf", TokenType::Op(OpCode::JCmpF));
        map.insert("jcmpb", TokenType::Op(OpCode::JCmpB));

        map.insert("eq", TokenType::Op(OpCode::Eq));
        map.insert("neq", TokenType::Op(OpCode::NEq));
        map.insert("gt", TokenType::Op(OpCode::Gt));
        map.insert("lt", TokenType::Op(OpCode::Lt));
        map.insert("ge", TokenType::Op(OpCode::Ge));
        map.insert("le", TokenType::Op(OpCode::Le));

        map.insert("push", TokenType::Op(OpCode::Push));
        map.insert("pop", TokenType::Op(OpCode::Pop));

        map.insert("aloc", TokenType::Op(OpCode::Aloc));
        map.insert("igl", TokenType::Op(OpCode::IGL));

        map.insert(".asciz", TokenType::Directive(DirectiveType::ASCIZ));
        map.insert(".asci", TokenType::Directive(DirectiveType::ASCI));
        map.insert(".word", TokenType::Directive(DirectiveType::WORD));
        map.insert(".byte", TokenType::Directive(DirectiveType::BYTE));

        map.insert(".data", TokenType::Section(SectionType::DATA));
        map.insert(".text", TokenType::Section(SectionType::TEXT));

        map
    };
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

    fn scan(&mut self, src: &str) -> Result<Vec<Token>, ()> {
        let mut src_iter = src.chars().peekable();
        let mut tokens = vec![];

        while let Some(_) = src_iter.peek() {
            // Skip whitespace
            loop {
                match src_iter.peek() {
                    Some(' ') | Some('\r') | Some('\t') => {
                        self.advance();
                        src_iter.next();
                    }
                    _ => break,
                };
            }

            let token: Token = match src_iter.next() {
                Some('\n') => {
                    self.new_line();
                    Ok(Token {
                        token_type: TokenType::NL,
                        file_coords: self.file_coords.clone(),
                    })
                }
                Some('[') => {
                    self.advance();
                    Ok(Token {
                        token_type: TokenType::LeftBracket,
                        file_coords: self.file_coords.clone(),
                    })
                }
                Some(']') => {
                    self.advance();
                    Ok(Token {
                        token_type: TokenType::RightBracket,
                        file_coords: self.file_coords.clone(),
                    })
                }
                Some('=') => {
                    self.advance();
                    Ok(Token {
                        token_type: TokenType::Equal,
                        file_coords: self.file_coords.clone(),
                    })
                }

                // Registers
                Some('r') | Some('R') => {
                    self.advance();

                    let start_index = self.current_index;
                    while let Some(c) = src_iter.peek() {
                        if !c.is_numeric() {
                            break;
                        }
                        self.advance();
                        src_iter.next();
                    }

                    let lexeme = src[start_index..self.current_index].to_string();
                    let file_coords = self.file_coords.new_offset(0, -(lexeme.len() as isize));
                    if let Ok(reg_num) = lexeme.parse::<u8>() {
                        Ok(Token {
                            token_type: TokenType::Reg(reg_num),
                            file_coords,
                        })
                    } else {
                        Err(())
                    }
                }

                // Immediate values
                Some('#') => {
                    self.advance();

                    let start_index = self.current_index;
                    while let Some(c) = src_iter.peek() {
                        if !c.is_numeric() {
                            break;
                        }
                        self.advance();
                        src_iter.next();
                    }

                    let lexeme = src[start_index..self.current_index].to_string();
                    let file_coords = self.file_coords.new_offset(0, -(lexeme.len() as isize));
                    if let Ok(imd_val) = lexeme.parse::<i32>() {
                        Ok(Token {
                            token_type: TokenType::ImdVal(imd_val),
                            file_coords,
                        })
                    } else {
                        Err(())
                    }
                }

                Some('"') => {
                    self.advance();

                    let start_index = self.current_index;
                    while let Some(c) = src_iter.peek() {
                        if *c == '\"' {
                            break;
                        }
                        self.advance();
                        src_iter.next();
                    }

                    let lexeme = src[start_index..self.current_index].to_string();
                    let file_coords = self.file_coords.new_offset(0, -(lexeme.len() as isize));
                    self.advance();
                    src_iter.next();
                    if lexeme.is_ascii() {
                        Ok(Token {
                            token_type: TokenType::String(lexeme.to_string()),
                            file_coords,
                        })
                    } else {
                        Err(())
                    }
                }

                // Keywords and labels
                Some(_) => {
                    let start_index = self.current_index;
                    self.advance();

                    while let Some(c) = src_iter.peek() {
                        if !c.is_alphanumeric() && *c != '_' && *c != ':' {
                            break;
                        }
                        self.advance();
                        src_iter.next();
                    }

                    let lexeme = src[start_index..self.current_index].to_string();
                    let file_coords = self.file_coords.new_offset(0, -(lexeme.len() as isize));
                    if lexeme.len() == 0 {
                        Ok(Token {
                            token_type: TokenType::EOF,
                            file_coords: self.file_coords.clone(),
                        })
                    } else if let Some(token_type) = KEYWORDS.get(lexeme.to_lowercase().as_str()) {
                        Ok(Token {
                            token_type: token_type.clone(),
                            file_coords,
                        })
                    } else if lexeme.ends_with(":") {
                        Ok(Token {
                            token_type: TokenType::LabelDecl(lexeme[..lexeme.len() - 1].to_owned()),
                            file_coords,
                        })
                    } else {
                        Ok(Token {
                            token_type: TokenType::LabelUsg(lexeme.clone()),
                            file_coords,
                        })
                    }
                }

                None => Ok(Token {
                    token_type: TokenType::EOF,
                    file_coords: self.file_coords.clone(),
                }),
            }?;
            if token.token_type == TokenType::NL {
                continue;
            }
            tokens.push(token);
        }
        tokens.push(Token {
            token_type: TokenType::EOF,
            file_coords: self.file_coords.clone(),
        });

        Ok(tokens)
    }

    fn advance(&mut self) {
        self.current_index += 1;
        self.file_coords.col += 1;
    }

    fn new_line(&mut self) {
        self.current_index += 1;
        self.file_coords.line += 1;
        self.file_coords.col = 1;
    }
}

pub fn scan(src: &str) -> Result<Vec<Token>, ()> {
    info!("Scanning...");
    let result = Scanner::new().scan(src);
    match result {
        Ok(tokens) => {
            info!("Scanning comlpete!");
            Ok(tokens)
        }
        Err(scanner_error) => {
            error!("Scanning failed: {:?}!", scanner_error);
            Err(())
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn scan(src: &str) -> Vec<Token> {
        super::scan(src).unwrap()
    }

    #[test]
    fn immediate_value() {
        let result = scan("#123");
        assert_eq!(
            *result.get(0).unwrap(),
            Token {
                token_type: TokenType::ImdVal(123),
                file_coords: FileCoords { line: 1, col: 2 }
            }
        );
        assert_eq!(result.last().unwrap().token_type, TokenType::EOF);
    }

    #[test]
    fn braces() {
        let result = scan("[ \t ]");
        assert_eq!(result.get(0).unwrap().token_type, TokenType::LeftBracket);
        assert_eq!(result.get(1).unwrap().token_type, TokenType::RightBracket);
        assert_eq!(result.last().unwrap().token_type, TokenType::EOF);
    }

    #[test]
    fn label_declaration() {
        let result = scan("my_label:");
        assert_eq!(
            result.get(0).unwrap().token_type,
            TokenType::LabelDecl("my_label".to_owned())
        );
    }

    #[test]
    fn label_usage() {
        let result = scan("my_label");
        assert_eq!(
            result.get(0).unwrap().token_type,
            TokenType::LabelUsg("my_label".to_owned())
        );
    }

    #[test]
    fn op_code_keywords_1() {
        let result = scan("add");
        assert_eq!(
            *result.get(0).unwrap(),
            Token {
                token_type: TokenType::Op(OpCode::Add),
                file_coords: FileCoords { line: 1, col: 1 }
            }
        );
        assert_eq!(result.last().unwrap().token_type, TokenType::EOF);
    }

    #[test]
    fn op_code_keywords_2() {
        let result = scan("ADD");
        assert_eq!(
            *result.get(0).unwrap(),
            Token {
                token_type: TokenType::Op(OpCode::Add),
                file_coords: FileCoords { line: 1, col: 1 }
            }
        );
        assert_eq!(result.last().unwrap().token_type, TokenType::EOF);
    }

    #[test]
    fn op_code_keywords_3() {
        let result = scan("load");
        assert_eq!(
            *result.get(0).unwrap(),
            Token {
                token_type: TokenType::Op(OpCode::Load),
                file_coords: FileCoords { line: 1, col: 1 }
            }
        );
        assert_eq!(result.last().unwrap().token_type, TokenType::EOF);
    }

    #[test]
    fn op_code_keywords_4() {
        let result = scan("loAd");
        assert_eq!(
            *result.get(0).unwrap(),
            Token {
                token_type: TokenType::Op(OpCode::Load),
                file_coords: FileCoords { line: 1, col: 1 }
            }
        );
        assert_eq!(result.last().unwrap().token_type, TokenType::EOF);
    }

    #[test]
    fn reg_1() {
        let result = scan("R01");
        assert_eq!(
            *result.get(0).unwrap(),
            Token {
                token_type: TokenType::Reg(1),
                file_coords: FileCoords { line: 1, col: 2 }
            }
        );
        assert_eq!(result.last().unwrap().token_type, TokenType::EOF);
    }

    #[test]
    fn reg_2() {
        let result = scan("R1");
        assert_eq!(
            *result.get(0).unwrap(),
            Token {
                token_type: TokenType::Reg(1),
                file_coords: FileCoords { line: 1, col: 2 }
            }
        );
        assert_eq!(result.last().unwrap().token_type, TokenType::EOF);
    }

    #[test]
    fn reg_3() {
        let result = scan("R14");
        assert_eq!(
            *result.get(0).unwrap(),
            Token {
                token_type: TokenType::Reg(14),
                file_coords: FileCoords { line: 1, col: 2 }
            }
        );
        assert_eq!(result.last().unwrap().token_type, TokenType::EOF);
    }

    #[test]
    fn reg_4() {
        let result = scan("R0");
        assert_eq!(
            *result.get(0).unwrap(),
            Token {
                token_type: TokenType::Reg(0),
                file_coords: FileCoords { line: 1, col: 2 }
            }
        );
        assert_eq!(result.last().unwrap().token_type, TokenType::EOF);
    }

    #[test]
    fn reg_5() {
        let result = scan("r0");
        assert_eq!(
            *result.get(0).unwrap(),
            Token {
                token_type: TokenType::Reg(0),
                file_coords: FileCoords { line: 1, col: 2 }
            }
        );
        assert_eq!(result.last().unwrap().token_type, TokenType::EOF);
    }

    #[test]
    fn instruction_1() {
        let result = scan("load r0 #10");
        assert_eq!(
            result.get(0).unwrap().token_type,
            TokenType::Op(OpCode::Load)
        );
        assert_eq!(result.get(1).unwrap().token_type, TokenType::Reg(0));
        assert_eq!(result.get(2).unwrap().token_type, TokenType::ImdVal(10));
        assert_eq!(result.last().unwrap().token_type, TokenType::EOF);
    }

    #[test]
    fn instruction_2() {
        let result = scan("load r0 r31");
        assert_eq!(
            result.get(0).unwrap().token_type,
            TokenType::Op(OpCode::Load)
        );
        assert_eq!(result.get(1).unwrap().token_type, TokenType::Reg(0));
        assert_eq!(result.get(2).unwrap().token_type, TokenType::Reg(31));
        assert_eq!(result.last().unwrap().token_type, TokenType::EOF);
    }

    #[test]
    fn program_1() {
        let result_token_types: Vec<TokenType> = scan(".data .text load r0 r31\n\t add r1 #54")
            .iter()
            .map(|token| token.token_type.clone())
            .collect();

        let expected_token_types = vec![
            TokenType::Section(SectionType::DATA),
            TokenType::Section(SectionType::TEXT),
            TokenType::Op(OpCode::Load),
            TokenType::Reg(0),
            TokenType::Reg(31),
            TokenType::Op(OpCode::Add),
            TokenType::Reg(1),
            TokenType::ImdVal(54),
            TokenType::EOF,
        ];

        assert_eq!(result_token_types, expected_token_types);
    }

    #[test]
    fn program_2() {
        let result_token_types: Vec<TokenType> =
            scan(".data my_string: .asciz \"hello\" .text my_label:\n\t load r0 r31\n\t add r1 #54\n\tjmp my_label \n add r1 [r2]")
                .iter()
                .map(|token| token.token_type.clone())
                .collect();

        let expected_token_types = vec![
            TokenType::Section(SectionType::DATA),
            TokenType::LabelDecl("my_string".to_owned()),
            TokenType::Directive(DirectiveType::ASCIZ),
            TokenType::String("hello".to_string()),
            TokenType::Section(SectionType::TEXT),
            TokenType::LabelDecl("my_label".to_owned()),
            TokenType::Op(OpCode::Load),
            TokenType::Reg(0),
            TokenType::Reg(31),
            TokenType::Op(OpCode::Add),
            TokenType::Reg(1),
            TokenType::ImdVal(54),
            TokenType::Op(OpCode::Jmp),
            TokenType::LabelUsg("my_label".to_owned()),
            TokenType::Op(OpCode::Add),
            TokenType::Reg(1),
            TokenType::LeftBracket,
            TokenType::Reg(2),
            TokenType::RightBracket,
            TokenType::EOF,
        ];

        assert_eq!(result_token_types, expected_token_types);
    }
}
