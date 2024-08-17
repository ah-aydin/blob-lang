use std::{collections::HashMap, slice::Iter, usize};

use blob_bc::{Ins, InsArg, OpCode};
use blob_common::error;

use crate::token::{DirectiveType, SectionType, Token, TokenType};

/// Parsing rules
/// ```ignore
/// load -> "LOAD" reg (reg | label | imd)
///
/// add -> "ADD" reg reg (reg | imd)
/// sub -> "SUB" reg reg (reg | imd)
/// mul -> "MUL" reg reg (reg | imd)
/// div -> "DIV" reg reg (reg | imd)
///
/// jmp -> "JMP" reg
/// jmpf -> "JMPF" (reg | imd)
/// jmpb -> "JMPB" (reg | imd)
/// jcmp -> "JCMP" reg
/// jcmpf -> "JCMPF" (reg | imd)
/// jcmpb -> "JCMPB" (reg | imd)
///
/// eq -> "EQ" reg (reg | imd)
/// neq -> "NEQ" reg (reg | imd)
/// gt -> "GT" reg (reg | imd)
/// lt -> "LT" reg (reg | imd)
/// ge -> "GE" reg (reg | imd)
/// le -> "LE" reg (reg | imd)
///
/// aloc -> "ALOC" reg
///
/// hlt -> "HTL"
///
/// memory -> "[" (reg | label) "]"
/// imd -> "#" NUMBER
/// reg -> "R"[0-31]
/// label -> IDENTIFIER
/// ```
struct Linker {
    tokens: Vec<Token>,
    label_to_instruction: HashMap<String, usize>,
    label_to_memory: HashMap<String, usize>,
    section: SectionType,
}

impl Linker {
    fn new(tokens: Vec<Token>) -> Linker {
        Linker {
            tokens,
            label_to_instruction: HashMap::new(),
            label_to_memory: HashMap::new(),
            section: SectionType::TEXT,
        }
    }

    fn link(&mut self) -> Result<Vec<u8>, ()> {
        self.second_pass()
    }

    fn second_pass(&mut self) -> Result<Vec<u8>, ()> {
        let mut t_iter = self.tokens.iter();
        let mut instructions: Vec<Ins> = Vec::new();
        let mut instruction_bytes: usize = 0;

        let mut data_section: Vec<u8> = Vec::new();
        while let Some(token) = t_iter.next() {
            match &token.token_type {
                TokenType::Section(section_type) => {
                    self.section = *section_type;
                    Ok(())
                }
                TokenType::Op(op_code) => {
                    if self.section != SectionType::TEXT {
                        error!("Cannot have instruction outside the '.text' section");
                        return Err(());
                    }
                    let ins = self.link_op(&token, *op_code, &mut t_iter)?;
                    instruction_bytes = ins.to_bytes().len();
                    instructions.push(ins);
                    Ok(())
                }
                TokenType::LabelDecl(lbl) => {
                    match self.section {
                        SectionType::DATA => {
                            self.label_to_memory.insert(lbl.clone(), data_section.len())
                        }
                        SectionType::TEXT => self
                            .label_to_instruction
                            .insert(lbl.clone(), instruction_bytes),
                    };
                    Ok(())
                }
                TokenType::Directive(directive) => match directive {
                    DirectiveType::ASCIZ => {
                        let s = t_iter.next();
                        if s.is_none() {
                            error!("Expected a string after '.asciz' directive");
                            return Err(());
                        }

                        let s = s.unwrap();
                        if let TokenType::String(s) = &s.token_type {
                            if !s.is_ascii() {
                                error!("Given string does not consist of ascii characters");
                            }
                            data_section.append(&mut s.clone().into_bytes());
                            data_section.push(b'\0');
                        } else {
                            error!("Expected a string after '.asciz' directive");
                            return Err(());
                        }

                        Ok(())
                    }
                    DirectiveType::ASCI => {
                        let s = t_iter.next();
                        if s.is_none() {
                            error!("Expected a string after '.asciz' directive");
                            return Err(());
                        }

                        let s = s.unwrap();
                        if let TokenType::String(s) = &s.token_type {
                            if !s.is_ascii() {
                                error!("Given string does not consist of ascii characters");
                            }
                            data_section.append(&mut s.clone().into_bytes());
                        } else {
                            error!("Expected a string after '.asciz' directive");
                            return Err(());
                        }

                        Ok(())
                    }
                    DirectiveType::WORD => todo!(),
                    DirectiveType::BYTE => todo!(),
                },
                TokenType::EOF => break,
                _ => todo!("{:?}", token),
            }?;
        }

        let text_section: Vec<u8> = instructions
            .iter()
            .map(|ins| ins.to_bytes())
            .flat_map(|v| v)
            .collect();
        Ok(text_section)
    }

    fn link_op(&self, token: &Token, op_code: OpCode, t_iter: &mut Iter<Token>) -> Result<Ins, ()> {
        let args_types = op_code.get_args_types();
        let args_count = args_types.len();
        let mut args: Vec<InsArg> = Vec::with_capacity(3);

        for (i, arg_types) in args_types.iter().enumerate() {
            let arg = t_iter.next();
            if arg.is_none() {
                error!(
                    "{} Expected {args_count} argument for instruction {:?} but {i} where given",
                    token.file_coords, op_code
                );
                return Err(());
            }

            let token = arg.unwrap();
            let token_arg_type = token.token_type.get_ins_arg_type();
            if token_arg_type.is_none() {
                error!(
                    "{} Got unexpected token '{:?}' as argument for OpCode '{:?}'",
                    token.file_coords, token.token_type, op_code
                );
                return Err(());
            }
            let token_arg_type = token_arg_type.unwrap();

            if !arg_types.contains(&token_arg_type) {
                error!(
                    "{} Argument {} for OpCode '{:?}' cannot be '{:?}'",
                    token.file_coords,
                    i + 1,
                    op_code,
                    token_arg_type
                );
                return Err(());
            }
            let arg = match &token.token_type {
                TokenType::Reg(reg) => InsArg::Reg(*reg),
                TokenType::ImdVal(imd) => InsArg::Imd(*imd as u16),
                TokenType::LabelUsg(_) => todo!(),
                _ => unreachable!("How did we get here?"),
            };
            args.push(arg);
        }

        Ok(Ins::from_ins_args(op_code, args))
    }
}

pub fn link(tokens: Vec<Token>) -> Result<Vec<u8>, ()> {
    Linker::new(tokens).link()
}
