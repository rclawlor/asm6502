// Standard library
use std::{iter::Peekable, str::Chars};


// Local
use crate::error::LexerError;


// The 6502 language tokens
#[derive(Clone, Debug)]
pub enum Token {
    Instruction(String),
    Address,
    Constant(u8),
    Label(String)
}


pub struct Lexer<'a> {
    it: Peekable<Chars<'a>>,
    tokens: Vec<Token>,
    line: usize
}

impl<'a> Lexer<'a> {
    pub fn new(text: &'a String) -> Self {
        Self { it: text.chars().peekable(), tokens: Vec::new(), line: 1 }
    }

    pub fn lex(&mut self) -> Result<Vec<Token>, LexerError> {
        while let Some(c) = self.it.peek() {
            match c {
                // String
                'a'..='z' | 'A'..='Z' | '_' => {
                    let text = self.get_string()?;
                    if text.ends_with(':') {
                        self.tokens.push(Token::Label(text.strip_suffix(':').unwrap().to_string()))
                    }
                    else if text.len() == 3 {
                        self.tokens.push(Token::Instruction(text))
                    }
                    else {
                        return Err(
                            LexerError::InvalidInstruction(
                                format!("Invalid instruction '{}' on line {}", text, self.line)
                            )
                        )
                    }
                }
                // Constant
                '#' => {
                    self.it.next();
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
                // New line
                '\n' => {
                    self.it.next();
                    self.line += 1
                },
                // Whitespace
                ' ' | '\t' => {self.it.next();},
                // Separator
                ',' => {self.it.next();},
                _ => {}
            }
        }

        Ok(self.tokens.clone())
    }

    /// Parse a string from the source file
    fn get_string(&mut self) -> Result<String, LexerError> {
        let mut text = String::new();
        while let Some(&c) = self.it.peek() {
            match c {
                'a'..='z' | 'A'..='Z' | '_' => {
                    self.it.next();
                    text.push(c);
                },
                ':' => {
                    self.it.next();
                    text.push(c);
                    break
                }
                _ => break
            }
        }

        return Ok(text)
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
                },
                ' ' | '\n' | ',' => break,
                _ => return Err(LexerError::InvalidNumber(format!("Invalid hex number on line {}", self.line)))
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
                ' ' | '\n' | ',' => break,
                _ => return Err(LexerError::InvalidNumber(format!("Invalid decimal number on line {}", self.line)))

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
                ' ' | '\n' | ',' => break,
                _ => return Err(LexerError::InvalidNumber(format!("Invalid binary number on line {}", self.line)))

            }
        }

        Ok(number)
    }
}
