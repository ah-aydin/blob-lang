use std::collections::HashSet;

use blob_bc::{DirectiveType, InsArg, InsData, InsText, OpCode, OpCodeType, SectionType};
use blob_common::{error, info};

use crate::token::{Token, TokenType};

pub struct ParseData {
    pub ins_data: Vec<InsData>,
    pub ins_text: Vec<InsText>,
}

struct Parser {
    tokens: Vec<Token>,
    labels: HashSet<String>,
    current: usize,
    section_type: SectionType,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens,
            labels: HashSet::new(),
            current: 0,
            section_type: SectionType::Text,
        }
    }

    fn parse(&mut self) -> Result<ParseData, ()> {
        let mut ins_text: Vec<InsText> = Vec::new();
        let mut ins_data: Vec<InsData> = Vec::new();

        loop {
            let peek_token_result = self.peek_token();
            if peek_token_result.is_err() {
                break;
            }
            let peek_token = peek_token_result.unwrap();

            if peek_token.token_type == TokenType::EOF {
                break;
            }
            if peek_token.token_type == TokenType::NL {
                self.advance();
                continue;
            }

            // I gave up on fighing the borrow checker, so here is a pointless clone
            match self.get_and_advance()?.token_type.clone() {
                TokenType::Op(op_code) => {
                    ins_text.push(self.parse_op(op_code)?);
                    Ok(())
                }
                TokenType::LabelDecl(label_name) => {
                    if self.labels.contains(&label_name) {
                        error!(
                            "{} Label '{label_name}' already exists",
                            self.peek_token()?.file_coords
                        );
                        return Err(());
                    }
                    match self.section_type {
                        SectionType::Data => {
                            ins_data.push(InsData::Label(label_name.clone()));
                            self.labels.insert(label_name.clone());
                        }
                        SectionType::Text => {
                            ins_text.push(InsText::LabelDecl(label_name.clone()));
                            self.labels.insert(label_name.clone());
                        }
                    };
                    Ok(())
                }

                TokenType::Section(st) => {
                    self.section_type = st;
                    Ok(())
                }

                TokenType::NL => Ok(()),
                TokenType::EOF => break,

                _ => {
                    error!(
                        "{} Got unexpected token to start exprssion {:?}",
                        self.peek_token()?.file_coords,
                        self.peek_token()?.token_type
                    );
                    Err(())
                }
            }?;
        }

        Ok(ParseData { ins_data, ins_text })
    }

    fn parse_op(&mut self, op_code: OpCode) -> Result<InsText, ()> {
        if self.section_type != SectionType::Text {
            error!(
                "{} Can only have an operation inside the '.text' section",
                self.peek_token()?.file_coords
            );
            return Err(());
        }
        match op_code.get_type() {
            OpCodeType::Misc => Ok(InsText::Arg0(op_code)),
            OpCodeType::Load => {
                let oc;
                let dest_arg = self.parse_reg_arg()?;
                let src_token = self.get_and_advance()?;
                let src_arg;

                match src_token.token_type {
                    TokenType::Reg(src_reg) => {
                        oc = OpCode::Load;
                        src_arg = InsArg::Reg(src_reg);
                    }
                    TokenType::ImdVal(imd_val) => {
                        oc = OpCode::LoadImd;
                        src_arg = InsArg::Imd(imd_val as u16);
                    }

                    TokenType::Directive(DirectiveType::Word) => {
                        oc = OpCode::LoadMemRegWord;
                        src_arg = self.parse_mem_reg()?;
                    }
                    TokenType::Directive(DirectiveType::HalfWord) => {
                        oc = OpCode::LoadMemRegHalfWord;
                        src_arg = self.parse_mem_reg()?;
                    }
                    TokenType::Directive(DirectiveType::QuaterWord) => {
                        oc = OpCode::LoadMemRegQuaterWord;
                        src_arg = self.parse_mem_reg()?;
                    }
                    TokenType::Directive(DirectiveType::Byte) => {
                        oc = OpCode::LoadMemRegByte;
                        src_arg = self.parse_mem_reg()?;
                    }
                    _ => {
                        error!(
                            "{} Expected a register, immediate value, or a memory reg as source.",
                            src_token.file_coords
                        );
                        return Err(());
                    }
                };

                Ok(InsText::Arg2(oc, dest_arg, src_arg))
            }
            OpCodeType::Str => {
                let mut oc;
                let directive_token = self.get_and_advance()?;
                let file_coords = directive_token.file_coords.clone();
                match directive_token.token_type {
                    TokenType::Directive(DirectiveType::Word) => {
                        oc = OpCode::StrWord;
                    }
                    TokenType::Directive(DirectiveType::HalfWord) => {
                        oc = OpCode::StrHalfWord;
                    }
                    TokenType::Directive(DirectiveType::QuaterWord) => {
                        oc = OpCode::StrQuaterWord;
                    }
                    TokenType::Directive(DirectiveType::Byte) => {
                        oc = OpCode::StrByte;
                    }
                    _ => {
                        error!(
                            "{} Expected a register, immediate value, or a memory reg as source.",
                            directive_token.file_coords
                        );
                        return Err(());
                    }
                };

                let dest_arg = self.parse_mem_reg()?;
                let src_arg = self.parse_reg_or_imd_arg()?;
                if src_arg.is_imd() {
                    if oc == OpCode::StrWord || oc == OpCode::StrHalfWord {
                        error!(
                            "{} '{:?}' cannot haave an immedate value as source.",
                            file_coords, oc
                        );
                        return Err(());
                    }
                    oc = oc.get_imd_version();
                }

                Ok(InsText::Arg2(oc, dest_arg, src_arg))
            }
            OpCodeType::Math => {
                let dest_arg = self.parse_reg_arg()?;
                let operand_1 = self.parse_reg_arg()?;
                let operand_2 = self.parse_reg_or_imd_arg()?;

                let oc = match operand_2.is_imd() {
                    true => op_code.get_imd_version(),
                    false => op_code,
                };

                Ok(InsText::Arg3(oc, dest_arg, operand_1, operand_2))
            }
            OpCodeType::Jmp => {
                let oc;
                let arg = self.parse_reg_or_imd_or_label_arg()?;
                if arg.is_label() && op_code != OpCode::Jmp && op_code != OpCode::JCmp {
                    error!(
                        "{} Only `JMP` and `JCMP` can use label as arguments",
                        self.peek_prev_token()?.file_coords
                    );
                    return Err(());
                } else if arg.is_imd() {
                    oc = op_code.get_imd_version();
                } else {
                    oc = op_code;
                }
                Ok(InsText::Arg1(oc, arg))
            }
            OpCodeType::Cmp => todo!(),
            OpCodeType::Stack => todo!(),
            OpCodeType::Heap => todo!(),
        }
    }

    fn parse_reg_arg(&mut self) -> Result<InsArg, ()> {
        let token = self.get_and_advance()?;
        match token.token_type.get_reg() {
            Ok(reg) => Ok(InsArg::Reg(reg)),
            Err(_) => {
                error!(
                    "{} Expected a register but got '{:?}'",
                    token.file_coords, token.token_type
                );
                Err(())
            }
        }
    }

    fn parse_reg_or_imd_arg(&mut self) -> Result<InsArg, ()> {
        let token = self.get_and_advance()?;
        match token.token_type {
            TokenType::Reg(reg) => Ok(InsArg::Reg(reg)),
            TokenType::ImdVal(imd_val) => Ok(InsArg::Imd(imd_val as u16)),
            _ => {
                error!(
                    "{} Expected a registor or immediate value, but got '{:?}'",
                    token.file_coords, token.token_type
                );
                Err(())
            }
        }
    }

    fn parse_reg_or_imd_or_label_arg(&mut self) -> Result<InsArg, ()> {
        let token = self.get_and_advance()?;
        match &token.token_type {
            TokenType::Reg(reg) => Ok(InsArg::Reg(*reg)),
            TokenType::LabelUsg(label) => Ok(InsArg::Label(label.clone())),
            TokenType::ImdVal(imd_val) => Ok(InsArg::Imd(*imd_val as u16)),
            _ => {
                error!(
                    "{} Expected an immediate value or label, but got '{:?}'",
                    token.file_coords, token.token_type
                );
                Err(())
            }
        }
    }

    fn parse_mem_reg(&mut self) -> Result<InsArg, ()> {
        let token = self.get_and_advance()?;
        if token.token_type != TokenType::LeftBracket {
            error!("{} Expected '[' to specify memory", token.file_coords);
            return Err(());
        }
        let reg_arg = self.parse_reg_arg()?;
        let token = self.get_and_advance()?;
        if token.token_type != TokenType::RightBracket {
            error!("{} Expected ']' missing closing", token.file_coords);
            return Err(());
        }
        Ok(reg_arg)
    }

    fn get_and_advance(&mut self) -> Result<&Token, ()> {
        self.current += 1;
        self.tokens.get(self.current - 1).ok_or(())
    }

    fn advance(&mut self) {
        self.current += 1;
    }

    fn peek_token(&self) -> Result<&Token, ()> {
        self.tokens.get(self.current).ok_or(())
    }

    fn peek_prev_token(&self) -> Result<&Token, ()> {
        self.tokens.get(self.current - 1).ok_or(())
    }
}

pub fn parse(tokens: Vec<Token>) -> ParseData {
    match Parser::new(tokens).parse() {
        Ok(parse_data) => {
            info!("Parsing complete!");
            parse_data
        }
        Err(()) => {
            error!("Parsing failed!");
            std::process::exit(1);
        }
    }
}
