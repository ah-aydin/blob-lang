use std::{collections::HashMap, slice::Iter};

use blob_bc::{Ins, InsArg, OpCode};
use blob_common::error;

use crate::token::{Token, TokenType};

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
}

impl Linker {
    fn new(tokens: Vec<Token>) -> Linker {
        Linker {
            tokens,
            label_to_instruction: HashMap::new(),
        }
    }

    fn link(&mut self) -> Result<Vec<u8>, ()> {
        let mut t_iter = self.tokens.iter();
        let mut instructions: Vec<u8> = Vec::new();

        while let Some(token) = t_iter.next() {
            match &token.token_type {
                TokenType::Op(op_code) => instructions
                    .append(&mut self.link_op(&token, *op_code, &mut t_iter)?.to_bytes()),
                TokenType::LabelDecl(lbl) => {
                    self.label_to_instruction
                        .insert(lbl.clone(), instructions.len());
                }
                TokenType::EOF => break,
                _ => todo!("{:?}", token),
            };
        }

        Ok(instructions)
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
