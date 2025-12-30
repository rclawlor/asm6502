use std::str::FromStr;

use crate::{
    ast::*,
    error::CompileError,
    lex::{Lexer, Token, TokenKind},
};

/// Parse source file
pub fn parse(source: &str) -> Result<Program, Vec<CompileError>> {
    let mut parser = Parser::new(source);
    let program = parser.parse_program();

    if parser.errors.is_empty() {
        Ok(program)
    } else {
        Err(parser.errors)
    }
}

/// Capitalise an ASCII string
pub fn capitalise(s: &str) -> String {
    s.chars()
        .enumerate()
        .map(|(idx, c)| {
            if idx == 0 {
                c.to_ascii_uppercase()
            } else {
                c.to_ascii_lowercase()
            }
        })
        .collect()
}

struct Parser<'source> {
    lexer: Lexer<'source>,
    current: Token<'source>,
    previous: Token<'source>,
    errors: Vec<CompileError>,
}

impl<'source> Parser<'source> {
    fn new(source: &'source str) -> Self {
        let mut lexer = Lexer::new(source);
        let current = lexer.next_token();
        let previous = current.clone();

        Parser {
            lexer,
            current,
            previous,
            errors: Vec::new(),
        }
    }

    fn parse_program(&mut self) -> Program {
        let start_loc = self.current.span;
        let mut items = Vec::new();
        while !self.at_end() {
            if self.is_opcode() {
                items.push(ProgramItem::Instruction(self.parse_instruction()));
            }
            else if self.is_preprocessor() {
                items.push(ProgramItem::Preprocessor(self.parse_preprocessor()));
            }
            else {
                self.advance();
            }
        }

        Program {
            id: next_node_id(),
            span: self.span_from(start_loc),
            items,
        }
    }

    fn expect_token(&mut self, kind: TokenKind) {
        if self.check(kind) {
            self.advance();
        } else {
            let message = format!("Expected {:?} but found {:?}", kind, self.current.kind);
            self.error(message, self.current.span);
            if !self.at_end() {
                self.advance();
            }
        }
    }

    /// Check if current token matches
    fn check(&self, kind: TokenKind) -> bool {
        self.current.kind == kind
    }

    /// Advance to next token
    fn advance(&mut self) {
        self.previous = self.current.clone();
        self.current = self.lexer.next_token();
    }

    /// Check if at end of file
    fn at_end(&self) -> bool {
        self.current.kind == TokenKind::Eof
    }

    /// Get span relative to starting span
    fn span_from(&self, start: Span) -> Span {
        Span {
            start: start.start,
            end: self.previous.span.end,
        }
    }

    /// Append new error message
    fn error(&mut self, message: String, span: Span) {
        self.errors.push(CompileError { message, span });
    }

    /// Parse opcode and operands
    fn parse_instruction(&mut self) -> Instruction {
        let loc = self.current.span;
        let key = capitalise(self.current.text);
        let opcode = match Opcode::from_str(key.as_str()) {
            Ok(opcode) => opcode,
            Err(_) => {
                self.error(format!("Invalid opcode: {}", self.current.text), loc);
                Opcode::Adc
            }
        };
        self.advance();
        let mut operands = Vec::new();
        loop {
            match self.current.kind {
                TokenKind::RegisterA | TokenKind::RegisterX | TokenKind::RegisterY => {
                    let register = match Register::from_token(self.current.kind) {
                        Some(register) => register,
                        None => {
                            self.error(
                                format!("Invalid register: {:#?}", self.current.kind),
                                self.current.span,
                            );
                            Register::A
                        }
                    };
                    operands.push(Operand::Register(register));
                }
                TokenKind::Hash => operands.push(Operand::Immediate),
                TokenKind::Comma => operands.push(Operand::Index),
                TokenKind::LeftBracket => operands.push(Operand::LeftBracket),
                TokenKind::RightBracket => operands.push(Operand::RightBracket),
                TokenKind::Ident => operands.push(Operand::Ident(self.parse_ident())),
                TokenKind::Number => operands.push(Operand::Number(self.parse_number())),
                _ => {
                    break;
                }
            }
            self.advance();
        }

        Instruction {
            id: next_node_id(),
            span: self.span_from(loc),
            opcode,
            operands,
        }
    }

    fn parse_number(&mut self) -> Number {
        let loc = self.current.span;
        let base = match &self.current.text.chars().next() {
            Some('$') => 16,
            Some('%') => 2,
            Some(_) => 10,
            None => {
                self.error(format!("Expected number, got '{}'", self.current.text), loc);
                10
            }
        };
        let s = if base != 10 {
            &self.current.text[1..]
        } else {
            self.current.text
        };
        let value = match i16::from_str_radix(s, base) {
            Ok(value) => value,
            Err(_) => {
                self.error(
                    format!("Unable to parse number '{}'", self.current.text),
                    loc,
                );
                0
            }
        };

        Number {
            id: next_node_id(),
            span: self.span_from(loc),
            value,
        }
    }

    /// Parse identifier definition
    fn parse_ident(&mut self) -> Ident {
        let loc = self.current.span;
        let name = self.current.text.to_string();
        self.expect_token(TokenKind::Ident);

        Ident {
            id: next_node_id(),
            span: loc,
            value: name,
        }
    }

    /// Parse a string literal
    fn parse_string(&mut self) -> StringLiteral {
        let loc = self.current.span;
        let value = self.current.text.to_string();
        self.expect_token(TokenKind::String);

        StringLiteral {
            id: next_node_id(),
            span: loc,
            value
        }
    }

    /// Parse preprocessor and operands
    fn parse_preprocessor(&mut self) -> Preprocessor {
        let loc = self.current.span;
        let key = capitalise(self.current.text.strip_prefix('.').unwrap());
        let directive = match Directive::from_str(key.as_str()) {
            Ok(directive) => directive,
            Err(_) => {
                self.error(format!("Invalid directive: {}", self.current.text), loc);
                Directive::Set
            }
        };
        self.advance();
        let mut args = Vec::new();
        loop {
            match self.current.kind {
                TokenKind::Ident => args.push(DirectiveItem::Ident(self.parse_ident())),
                TokenKind::Number => args.push(DirectiveItem::Number(self.parse_number())),
                TokenKind::String => args.push(DirectiveItem::String(self.parse_string())),
                _ => break,
            }
            self.advance();
        }

        Preprocessor {
            id: next_node_id(),
            span: self.span_from(loc),
            directive,
            args,
        }
    }

    fn is_opcode(&self) -> bool {
        self.current.kind == TokenKind::Opcode
    }

    fn is_preprocessor(&self) -> bool {
        self.current.kind == TokenKind::Preprocessor
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty() {
        let program = parse("").unwrap();
        assert_eq!(program.items.len(), 0);
    }

    #[test]
    fn test_whitespace_only() {
        let program = parse("  \t\n\r ").unwrap();
        assert_eq!(program.items.len(), 0);
    }

    #[test]
    fn test_instruction() {
        let program = parse("LDA #$10").unwrap();
        assert_eq!(program.items.len(), 1);
        match &program.items[0] {
            ProgramItem::Instruction(instr) => {
                println!("{:#?}", instr);
                assert_eq!(instr.opcode, Opcode::Lda);
                assert_eq!(instr.operands.len(), 2);
                match &instr.operands[1] {
                    Operand::Number(x) => assert_eq!(x.value, 0x10),
                    other => assert!(false, "Expected number, got {:#?}", other),
                }
            }
            other => assert!(false, "Expected instruction, got {:#?}", other),
        }
    }
}
