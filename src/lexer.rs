use std::{iter::Peekable, str::Chars};


use crate::error::LexerError;


// The 6502 language tokens
#[derive(Clone, Copy, Debug)]
pub enum Token {
    Instruction,
    Address,
    Constant(u8),
}


pub struct Lexer<'a> {
    it: Peekable<Chars<'a>>,
    tokens: Vec<Token>,
    line: usize
}

impl<'a> Lexer<'a> {
    pub fn new(text: &'a String) -> Self {
        Self { it: text.chars().peekable(), tokens: Vec::new(), line: 0 }
    }

    pub fn lex(&mut self) -> Result<Vec<Token>, LexerError> {
        while let Some(c) = self.it.next() {
            match c {
                // Constant
                '#' => {
                    if let Some(d) = self.it.next() {
                        let token = match d {
                                '$' => Token::Constant(self.get_hex_number()?),
                                'd' => Token::Constant(self.get_decimal_number()?),
                                '%' => Token::Constant(self.get_binary_number()?),
                                _ => return Err(
                                    LexerError::InvalidNumber(format!("Invalid number constant '{}' on line {}", d, self.line))
                                )
                        };
                        self.tokens.push(token);
                    };
                },
                '\n' => {
                    self.line += 1
                }
                _ => {}
            }
        }

        Ok(self.tokens.clone())
    }

    /// Parse a hex number from the source file
    fn get_hex_number(&mut self) -> Result<u8, LexerError> {
        let mut number: u8 = 0;
        while let Some(&c) = self.it.peek() {
            match c {
                '0'..='9' | 'a'..='f' | 'A'..='F' => {
                    self.it.next();
                    number = (number * 16) + u8::from_str_radix(&c.to_string(), 16)
                        .expect("Number already checked to be within [0-9a-fA-F] range");
                }
                _ => break
            }
        }

        Ok(number)
    }

    /// Parse a decimal number from the source file
    fn get_decimal_number(&mut self) -> Result<u8, LexerError> {
        let mut number: u8 = 0;
        while let Some(&c) = self.it.peek() {
            match c {
                '0'..='9' => {
                    self.it.next();
                    number = (number * 10) + u8::from_str_radix(&c.to_string(), 10)
                        .expect("Number already checked to be within [0-9] range");
                }
                _ => break
            }
        }

        Ok(number)
    }

    /// Parse a binary number from the source file
    fn get_binary_number(&mut self) -> Result<u8, LexerError> {
        let mut number: u8 = 0;
        while let Some(&c) = self.it.peek() {
            match c {
                '0'..='1' => {
                    self.it.next();
                    number = (number * 2) + c.to_string().parse::<u8>().expect("Number already checked to be within 0-9 range");
                },
                '2'..='9' => {
                    return Err(
                        LexerError::InvalidNumber(
                            format!("Binary number can only contain [0-1]: found {} in line {}", c, self.line)
                        )
                    )
                }
                _ => break
            }
        }

        Ok(number)
    }
}
