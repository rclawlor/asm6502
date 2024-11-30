// Standard library
use std::{
    fs::File,
    io::{BufReader, Lines},
    iter::{Enumerate, Peekable},
    str::{Chars, FromStr}
};

// Local
use crate::{error::LexerError, instruction::OpCode};


/// The 6502 language tokens
#[allow(dead_code)]
#[derive(Clone, Debug)]
pub enum Token {
    Instruction(OpCode),
    Address(u16),
    Constant(u16),
    Label(String),
    LocalLabel(String),
}


pub struct Lexer {
    tokens: Vec<Vec<Token>>,
    line_idx: usize
}

impl Lexer {
    /// Create a `Lexer` instance
    pub fn new() -> Self {
        Self { tokens: Vec::new(), line_idx: 1 }
    }

    /// Lex file and output tokens
    pub fn lex(&mut self, lines: Lines<BufReader<File>>) -> Result<Vec<Vec<Token>>, LexerError> {
        for (line_idx, line) in lines.into_iter().enumerate() {
            let line = match line {
                Ok(line) => line,
                Err(e) => return Err(LexerError::FileRead(format!("Unable to read file: {}", e)))
            };
            self.line_idx = line_idx;

            let mut line_tokens = Vec::new();
            let mut it = line.chars().enumerate().peekable();
            while let Some((char_idx, c)) = it.peek() {
                match c {
                    // String
                    'a'..='z' | 'A'..='Z' | '@' | '_' => {
                        let start = char_idx.clone();
                        let text = self.get_string(&mut it)?;
                        let length = text.len();
                        if text.ends_with(':') {
                            if text.starts_with('@') {
                                line_tokens.push(Token::LocalLabel(text.strip_suffix(':').unwrap().to_string()))
                            } else {
                                line_tokens.push(Token::Label(text.strip_suffix(':').unwrap().to_string()))
                            }
                        }
                        else if text.len() == 3 {
                            let opcode = match OpCode::from_str(text.as_str()) {
                                Ok(opcode) => opcode,
                                Err(e) => return Err(
                                    LexerError::InvalidInstruction(e.get_msg(), self.line_idx, start, length, line)
                                )
                            };
                            line_tokens.push(Token::Instruction(opcode))
                        }
                        else {
                            return Err(
                                LexerError::InvalidInstruction(
                                    format!("Invalid instruction '{}'", text), self.line_idx, start, length, line
                                )
                            )
                        }
                    }
                    // Constant
                    '#' => {
                        it.next();
                        if let Some((char_idx, d)) = it.next() {
                            let token = match d {
                                    '$' => Token::Constant(self.get_hex_number(&mut it)?),
                                    'd' => Token::Constant(self.get_decimal_number(&mut it)?),
                                    '%' => Token::Constant(self.get_binary_number(&mut it)?),
                                    _ => return Err(
                                        LexerError::InvalidNumber(
                                            format!("invalid number constant '{}'", d), self.line_idx, char_idx, 1, line
                                        )
                                    )
                            };
                            line_tokens.push(token);
                        };
                    },
                    // Address
                    '$' => {
                        it.next();
                        if let Some((char_idx, _d)) = it.next() {
                            let address = match self.get_hex_number(&mut it) {
                                Ok(address) => address,
                                Err(e) => return Err(LexerError::InvalidAddress(format!("{}", e), self.line_idx, char_idx, 0, line))
                            };
                            line_tokens.push(Token::Address(address));
                        };
                    },
                    // New line
                    '\n' => break,
                    // Whitespace
                    ' ' | '\t' => {it.next();},
                    // Separator
                    ',' => {it.next();},
                    _ => {}
                }
            }

            self.tokens.push(line_tokens);
        }

        Ok(self.tokens.clone())
    }

    /// Parse a string from the source file
    fn get_string(&mut self, it: &mut Peekable<Enumerate<Chars<'_>>>) -> Result<String, LexerError> {
        let mut text = String::new();
        while let Some((_, c)) = it.peek() {
            match c {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {
                    let (_, c) = it.next().unwrap();
                    text.push(c.clone());
                },
                ':' => {
                    let (_, c) = it.next().unwrap();
                    text.push(c.clone());
                    break
                }
                _ => break
            }
        }

        Ok(text)
    }

    /// Parse a hex number from the source file
    fn get_hex_number(&mut self, it: &mut Peekable<Enumerate<Chars<'_>>>) -> Result<u16, LexerError> {
        let mut number: u16 = 0;
        while let Some((char_idx, c)) = it.peek() {
            match c {
                '0'..='9' | 'a'..='f' | 'A'..='F' => {
                    let (_, c) = it.next().unwrap();
                    number = (number * 16) + u16::from_str_radix(&c.to_string(), 16)
                        .expect("Number already checked to be within [0-9a-fA-F] range");
                },
                ' ' | '\n' | ',' => break,
                _ => return Err(LexerError::InvalidNumber(format!("Invalid hex number"), self.line_idx, *char_idx, 1, "".to_string()))
            }
        }

        Ok(number)
    }

    /// Parse a decimal number from the source file
    fn get_decimal_number(&mut self, it: &mut Peekable<Enumerate<Chars<'_>>>) -> Result<u16, LexerError> {
        let mut number: u16 = 0;
        while let Some((char_idx, c)) = it.peek() {
            match c {
                '0'..='9' => {
                    let (_, c) = it.next().unwrap();
                    number = (number * 10) + u16::from_str_radix(&c.to_string(), 10)
                        .expect("Number already checked to be within [0-9] range");
                }
                ' ' | '\n' | ',' => break,
                _ => return Err(LexerError::InvalidNumber(format!("Invalid decimal number"), self.line_idx, *char_idx, 1, "".to_string()))

            }
        }

        Ok(number)
    }

    /// Parse a binary number from the source file
    fn get_binary_number(&mut self, it: &mut Peekable<Enumerate<Chars<'_>>>) -> Result<u16, LexerError> {
        let mut number: u16 = 0;
        while let Some((char_idx, c)) = it.peek() {
            match c {
                '0'..='1' => {
                    let (_, c) = it.next().unwrap();
                    number = (number * 2) + c.to_string().parse::<u16>().expect("Number already checked to be within 0-9 range");
                },
                '2'..='9' => {
                    return Err(
                        LexerError::InvalidNumber(
                            format!("Binary number can only contain [0-1]"), self.line_idx, *char_idx, 1, "".to_string()
                        )
                    )
                }
                ' ' | '\n' | ',' => break,
                _ => return Err(LexerError::InvalidNumber(format!("Invalid binary number"), self.line_idx, *char_idx, 1, "".to_string()))

            }
        }

        Ok(number)
    }
}
