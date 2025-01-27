use std::collections::HashMap;

use crate::{
    error::AssemblerError,
    instruction::{
        AddressingMode, Preprocessor,
        Register::{A, X, Y},
    },
    lexer::{
        LineTokens,
        Token::{self, Address, CloseGroup, Constant, Instruction, OpenGroup, Register},
    },
};

pub struct Assembler {
    symbol_table: HashMap<String, Token>,
}

impl Assembler {
    /// Create new `Assembler` instance
    pub fn new() -> Self {
        Self {
            symbol_table: HashMap::new(),
        }
    }

    /// Assemble tokens into binary file
    pub fn assemble(&mut self, tokens: Vec<LineTokens>) -> Result<(), AssemblerError> {
        for line in tokens {
            if line.tokens().len() == 0 {
                continue;
            }

            match &line.tokens()[0] {
                Token::Preprocessor(Preprocessor::DEFINE) => {
                    let n_tokens = line.tokens().len();
                    if n_tokens < 2 {
                        return Err(
                            AssemblerError::InvalidPreprocessorArguments(
                                String::from(
                                    "The .DEFINE preprocessor expects a variable name and optional value"
                                ),
                                line.line_idx(),
                                line.path()
                            )
                        );
                    }

                    match &line.tokens()[1] {
                        Token::Label(label) => {
                            if n_tokens == 2 {
                                self.symbol_table.insert(label.clone(), Token::Constant(1));
                            }
                            else if n_tokens == 3 {
                                self.symbol_table.insert(label.clone(), line.tokens()[2].clone());
                            }
                        }
                        _ => return Err(
                            AssemblerError::InvalidPreprocessorArguments(
                                String::from(
                                    "The .DEFINE preprocessor needs a variable name as it's first parameter"
                                ),
                                line.line_idx(),
                                line.path()
                            )
                        )
                    }
                }
                _ => (),
            }

            match &line.tokens()[0] {
                Token::Instruction(opcode) => {
                    let opcode = self.get_opcode(&line)?;
                }
                _ => (),
            };
        }

        Ok(())
    }

    fn get_opcode(&self, line: &LineTokens) -> Result<AddressingMode, AssemblerError> {
        let mut token_match = Vec::new();
        for token in line.tokens() {
            match token {
                Token::Label(label) => token_match.push(match self.symbol_table.get(label) {
                    Some(value) => value,
                    None => {
                        return Err(AssemblerError::InvalidAddressingMode(
                            format!("Could not find definition for symbol '{}'", label),
                            line.line_idx(),
                            line.path(),
                        ))
                    }
                }),
                other => token_match.push(&other),
            }
        }
        let addressing_mode = match &token_match[..] {
            [Instruction(_), Constant(_)] => AddressingMode::Immediate,
            [Instruction(_), Address(a)] if *a <= 0xFF => AddressingMode::ZeroPage,
            [Instruction(_), Address(a), Register(X)] if *a <= 0xFF => AddressingMode::ZeroPageX,
            [Instruction(_), Address(_)] => AddressingMode::Absolute,
            [Instruction(_), Address(_), Register(X)] => AddressingMode::AbsoluteX,
            [Instruction(_), Address(_), Register(Y)] => AddressingMode::AbsoluteY,
            [Instruction(_), OpenGroup, Address(a), Register(X), CloseGroup] if *a <= 0xFF => {
                AddressingMode::IndirectX
            }
            [Instruction(_), OpenGroup, Address(a), CloseGroup, Register(Y)] if *a <= 0xFF => {
                AddressingMode::IndirectX
            }
            _ => {
                return Err(AssemblerError::InvalidAddressingMode(
                    format!("Invalid addressing mode for '{:?}'", &token_match[0]),
                    line.line_idx(),
                    line.path(),
                ))
            }
        };

        Ok(addressing_mode)
    }
}
