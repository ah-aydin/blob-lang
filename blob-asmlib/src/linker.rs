use std::collections::HashMap;

use blob_bc::{InsData, InsText, SectionType};
use blob_common::{error, info};
use blob_executable::BlobExecutable;

use crate::token::{Token, TokenType};

struct Linker {
    tokens: Vec<Token>,
    text_label_to_index: HashMap<String, usize>,
    data_label_to_index: HashMap<String, usize>,
}

impl Linker {
    fn new(tokens: Vec<Token>) -> Linker {
        Linker {
            tokens,
            text_label_to_index: HashMap::new(),
            data_label_to_index: HashMap::new(),
        }
    }

    fn link(&mut self) -> Result<BlobExecutable, ()> {
        let (ins_text, ins_data) = self.first_pass()?;
        Err(())
    }

    fn first_pass(&mut self) -> Result<(Vec<InsText>, Vec<InsData>), ()> {
        let mut ins_text: Vec<InsText> = Vec::new();
        let mut ins_data: Vec<InsData> = Vec::new();
        let mut section_type = SectionType::Text;

        let mut t_iter = self.tokens.iter();

        while let Some(token) = t_iter.next() {
            match &token.token_type {
                TokenType::Op(op_code) => todo!(),
                TokenType::LabelDecl(label_name) => {
                    if self.text_label_to_index.contains_key(label_name)
                        || self.data_label_to_index.contains_key(label_name)
                    {
                        error!("Label '{label_name}' already exists");
                        return Err(());
                    }
                    match section_type {
                        SectionType::Data => {
                            ins_data.push(InsData::Label(label_name.clone()));
                            self.data_label_to_index
                                .insert(label_name.clone(), ins_data.len() - 1);
                            Ok(())
                        }
                        SectionType::Text => {
                            ins_text.push(InsText::Label(label_name.clone()));
                            self.text_label_to_index
                                .insert(label_name.clone(), ins_data.len() - 1);
                            Ok(())
                        }
                    }
                }

                TokenType::Section(st) => {
                    section_type = *st;
                    Ok(())
                }
                TokenType::EOF => break,
                _ => {
                    error!(
                        "{} Got unexpected token to start exprssion {:?}",
                        token.file_coords, token.token_type
                    );
                    Err(())
                }
            }?;
        }

        Ok((ins_text, ins_data))
    }
}

pub fn link(tokens: Vec<Token>) -> Result<BlobExecutable, ()> {
    Linker::new(tokens).link()
}
