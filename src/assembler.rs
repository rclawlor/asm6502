use std::collections::HashMap;

use crate::{
    error::AssemblerError,
    instruction::{
        AddressingMode, Preprocessor, Register::{A, X, Y}
    },
    lexer::Token::{
        self, Address, CloseGroup, Constant, Instruction, OpenGroup, Register
    }
};

pub struct Assembler {
    symbol_table: HashMap<String, Token>
}

impl Assembler {
    /// Create new `Assembler` instance
    pub fn new() -> Self {
        Self { symbol_table: HashMap::new() }
    }

    /// Assemble tokens into binary file
    pub fn assemble(&mut self, tokens: Vec<Vec<Token>>) -> Result<(), AssemblerError> {
        for token in tokens {
            if token.len() == 0 {
                continue;
            } 

            match &token[0] {
                Token::Preprocessor(Preprocessor::DEFINE) => {
                    let n_tokens = token.len();
                    if n_tokens < 2 {
                        return Err(
                            AssemblerError::InvalidPreprocessorArguments(
                                String::from(
                                    "The .DEFINE preprocessor expects a variable name and optional value"
                                )
                            )
                        )
                    }

                    match &token[1] {
                        Token::Label(label) => {
                            if n_tokens == 2 {
                                self.symbol_table.insert(label.clone(), Token::Constant(1));
                            }
                            else if n_tokens == 3 {
                                self.symbol_table.insert(label.clone(), token[2].clone());
                            }
                        }
                        _ => return Err(
                            AssemblerError::InvalidPreprocessorArguments(
                                String::from(
                                    "The .DEFINE preprocessor needs a variable name as it's first parameter"
                                )
                            )
                        )
                    }
                }
                _ => ()
            }

            match &token[0] {
                Token::Instruction(opcode) => {
                    let opcode = self.get_opcode(&token);
                    println!("{:?}", opcode);
                },
                _ => ()
            };
        }

        Ok(())
    }

    fn get_opcode(&self, tokens: &Vec<Token>) -> Result<AddressingMode, AssemblerError> {
        let mut token_match = Vec::new();
        for token in tokens {
            match token {
                Token::Label(label) => token_match.push(
                    match self.symbol_table.get(label) {
                        Some(value) => value,
                        None => return Err(AssemblerError::InvalidAddressingMode("".to_string()))
                    }
                ),
                other => token_match.push(other)
            }
        }
        let addressing_mode = match &token_match[..] {
            [Instruction(_), Constant(_)] => AddressingMode::Immediate,
            [Instruction(_), Address(a)] if *a <= 0xFF => AddressingMode::ZeroPage,
            [Instruction(_), Address(a), Register(X)] if *a <= 0xFF => AddressingMode::ZeroPageX,
            [Instruction(_), Address(_)] => AddressingMode::Absolute,
            [Instruction(_), Address(_), Register(X)] => AddressingMode::AbsoluteX,
            [Instruction(_), Address(_), Register(Y)] => AddressingMode::AbsoluteY,
            [Instruction(_), OpenGroup, Address(a), Register(X), CloseGroup] if *a <= 0xFF => AddressingMode::IndirectX,
            [Instruction(_), OpenGroup, Address(a), CloseGroup, Register(Y)] if *a <= 0xFF => AddressingMode::IndirectX,
            _ => return Err(AssemblerError::InvalidAddressingMode(format!("{:?}", tokens)))
        };

        Ok(addressing_mode)
    }
}
