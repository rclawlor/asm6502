// Standard library
use std::{
    fs::File,
    io::{BufReader, BufRead},
    iter::{Enumerate, Peekable},
    path::PathBuf,
    str::{Chars, FromStr}
};

// Local
use crate::{
    error::LexerError,
    instruction::{OpCode, Preprocessor, Register}
};


/// The 6502 language tokens
#[allow(dead_code)]
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Register(Register),
    Instruction(OpCode),
    Address(u16),
    Constant(u16),
    Label(String),
    StringLiteral(String),
    LocalLabel(String),
    Preprocessor(Preprocessor),
    OpenGroup,
    CloseGroup
}


pub struct Lexer {
    path: PathBuf,
    tokens: Vec<Vec<Token>>,
    line_idx: usize,
    line: String
}

impl Lexer {
    /// Create a `Lexer` instance
    pub fn new(path: PathBuf) -> Self {
        Self {
            path,
            tokens: Vec::new(),
            line_idx: 1,
            line: String::new()
        }
    }

    /// Lex file and output tokens
    pub fn lex(&mut self) -> Result<Vec<Vec<Token>>, LexerError> {
        let file = match File::open(&self.path) {
            Ok(file) => file,
            Err(_) => return Err(
                LexerError::FileRead(format!("Unable to read file {:#?}", self.path), 0, 0, 0, self.line.clone())
            )
        };
        let lines = BufReader::new(file).lines();

        for (line_idx, line) in lines.into_iter().enumerate() {
            let line = match line {
                Ok(line) => line,
                Err(e) => return Err(
                    LexerError::FileRead(format!("Unable to read file: {:#?}", e), line_idx, 0, 0, self.line.clone())
                )
            };
            self.line_idx = line_idx;
            self.line = line.clone();

            let mut line_tokens = Vec::new();
            let mut it = line.chars().enumerate().peekable();
            while let Some((char_idx, c)) = it.peek() {
                match c {
                    // String
                    'a'..='z' | 'A'..='Z' | '@' | '_' | '.' | '"' => {
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
                        else if text.starts_with('.') {
                            let preprocessor = match Preprocessor::from_str(text.strip_prefix('.').unwrap()) {
                                Ok(preprocessor) => preprocessor,
                                Err(e) => return Err(
                                    LexerError::InvalidPreprocessor(e.get_msg(), self.line_idx, start, length, line)
                                )
                            };
                            line_tokens.push(Token::Preprocessor(preprocessor));
                        }
                        else if text.starts_with('"') && text.ends_with('"') {
                            let literal = text.strip_prefix('"').unwrap().strip_suffix('"').unwrap();
                            line_tokens.push(Token::StringLiteral(literal.to_string()));
                        }
                        else if text.len() == 3 {
                            let opcode = match OpCode::from_str(text.as_str()) {
                                Ok(opcode) => opcode,
                                Err(e) => return Err(
                                    LexerError::InvalidInstruction(e.get_msg(), self.line_idx, start, length, line)
                                )
                            };
                            line_tokens.push(Token::Instruction(opcode));
                        }
                        else if text == "A" || text == "X" || text == "Y" {
                            let register = match Register::from_str(text.as_str()) {
                                Ok(register) => register,
                                Err(e) => return Err(
                                    LexerError::InvalidRegister(e.get_msg(), self.line_idx, start, 1, line)
                                )
                            };
                            line_tokens.push(Token::Register(register));
                        }
                        else {
                            line_tokens.push(Token::Label(text.to_string()))
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
                                Err(e) => return Err(LexerError::InvalidAddress(format!("{}", e), self.line_idx, char_idx, 1, line))
                            };
                            line_tokens.push(Token::Address(address));
                        };
                    },
                    // Open group
                    '(' => {
                        it.next();
                        line_tokens.push(Token::OpenGroup);
                    },
                    ')' => {
                        it.next();
                        line_tokens.push(Token::CloseGroup);
                    },
                    // New line
                    '\n' => break,
                    // Whitespace
                    ' ' | '\t' => {it.next();},
                    // Separator
                    ',' => {it.next();},
                    // Comment
                    ';' => break,
                    // Raise error on unrecognised character
                    other => return Err(
                        LexerError::InvalidCharacter(
                            format!("Invalid character '{}'", other), self.line_idx, *char_idx, 1, line
                        )
                    )
                }
            }

            // Lex imported files and add to token list
            if line_tokens.starts_with(&[Token::Preprocessor(Preprocessor::INCSRC)]) {
                match &line_tokens[line_tokens.len() - 1] {
                    Token::StringLiteral(s) => {
                        let path = match PathBuf::from_str(s) {
                            Ok(path) => path,
                            Err(_) => return Err(
                                LexerError::FileRead(format!("Unable to read import '{}'", s), self.line_idx, 0, self.line.len(), line)
                            )
                        };
                        let mut lexer = Self::new(path);
                        let subtokens = lexer.lex()?;
                        for token in subtokens.into_iter() {
                            self.tokens.push(token);
                        }
                    },
                    _ => return Err(LexerError::InvalidPreprocessor("Expected source file".to_string(), self.line_idx, 0, 1, line))
                };
            }
            self.tokens.push(line_tokens);
        }

        Ok(self.tokens.clone())
    }

    /// Parse a string from the source file
    fn get_string(&mut self, it: &mut Peekable<Enumerate<Chars<'_>>>) -> Result<String, LexerError> {
        let mut text = String::new();
        let mut string_idx = 0;
        while let Some((_, c)) = it.peek() {
            match (string_idx, c) {
                (_, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '/' | '.') => {
                    let (_, c) = it.next().unwrap();
                    text.push(c.clone());
                },
                (_, ':') => {
                    let (_, c) = it.next().unwrap();
                    text.push(c.clone());
                    break
                },
                (0, '"') => {
                    let (_, c) = it.next().unwrap();
                    text.push(c.clone());
                },
                (_, '"') => {
                    let (_, c) = it.next().unwrap();
                    text.push(c.clone());
                    break;
                },
                _ => break,
            }
            string_idx += 1;
        }
        Ok(text)
    }

    /// Parse a hex number from the source file
    fn get_hex_number(&mut self, it: &mut Peekable<Enumerate<Chars<'_>>>) -> Result<u16, LexerError> {
        let mut number_str = Vec::new();
        let (start_idx, _) = it.peek().expect("Already checked to have number").clone();
        while let Some((_, c)) = it.peek() {
            match c {
                ' ' | '\n' | ',' => break,
                _ => {
                    let (_, c) = it.next().unwrap();
                    number_str.push(c);
                }
            }
        }

        let mut number: u16 = 0;
        let number_len = number_str.len();
        for c in number_str {
            match c {
                '0'..='9' | 'a'..='f' | 'A'..='F' => {
                    number = (number * 16) + u16::from_str_radix(&c.to_string(), 16)
                        .expect("Number already checked to be within [0-9a-fA-F] range");

                },
                _ => return Err(
                    LexerError::InvalidNumber(
                        format!("Invalid hex number"), self.line_idx, start_idx, number_len, self.line.clone()
                    )
                )
            }
        }

        Ok(number)
    }

    /// Parse a decimal number from the source file
    fn get_decimal_number(&mut self, it: &mut Peekable<Enumerate<Chars<'_>>>) -> Result<u16, LexerError> {
        let mut number_str = Vec::new();
        let (start_idx, _) = it.peek().expect("Already checked to have number").clone();
        while let Some((_, c)) = it.peek() {
            match c {
                ' ' | '\n' | ',' => break,
                _ => {
                    let (_, c) = it.next().unwrap();
                    number_str.push(c);
                }
            }
        }

        let mut number: u16 = 0;
        let number_len = number_str.len();
        for c in number_str {
            match c {
                '0'..='9' => {
                    number = (number * 10) + u16::from_str_radix(&c.to_string(), 10)
                        .expect("Number already checked to be within [0-9] range");

                },
                _ => return Err(
                    LexerError::InvalidNumber(
                        format!("Invalid decimal number"), self.line_idx, start_idx, number_len, self.line.clone()
                    )
                )
            }
        }

        Ok(number)
    }

    /// Parse a binary number from the source file
    fn get_binary_number(&mut self, it: &mut Peekable<Enumerate<Chars<'_>>>) -> Result<u16, LexerError> {
        let mut number_str = Vec::new();
        let (start_idx, _) = it.peek().expect("Already checked to have number").clone();
        while let Some((_, c)) = it.peek() {
            match c {
                ' ' | '\n' | ',' => break,
                _ => {
                    let (_, c) = it.next().unwrap();
                    number_str.push(c);
                }
            }
        }

        let mut number: u16 = 0;
        let number_len = number_str.len();
        for c in number_str {
            match c {
                '0'..='1' => {
                    number = (number * 2) + u16::from_str_radix(&c.to_string(), 2)
                        .expect("Number already checked to be within [0-1] range");

                },
                _ => return Err(
                    LexerError::InvalidNumber(
                        format!("Invalid binary number"), self.line_idx, start_idx, number_len, self.line.clone()
                    )
                )
            }
        }

        Ok(number)
    }
}
