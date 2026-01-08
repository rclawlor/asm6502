use std::{collections::HashSet, str::FromStr};

use crate::{
    ast::{
        next_node_id, Directive, DirectiveItem, Ident, Instruction, Label, Number, Opcode, Operand,
        Preprocessor, Program, ProgramItem, Register, Span, StringLiteral,
    },
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
    labels: HashSet<String>,
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
            labels: HashSet::new(),
            errors: Vec::new(),
        }
    }

    fn parse_program(&mut self) -> Program {
        let start_loc = self.current.span;
        let mut items = Vec::new();
        while !self.at_end() {
            if self.is_opcode() {
                items.push(ProgramItem::Instruction(self.parse_instruction()));
            } else if self.is_preprocessor() {
                items.push(ProgramItem::Preprocessor(self.parse_preprocessor()));
            } else if self.is_label() {
                items.push(ProgramItem::Label(self.parse_label()));
            } else {
                self.error(String::from("Unexpected token"), self.current.span, None);
                self.advance();
            }
        }

        Program {
            id: next_node_id(),
            span: self.span_from(start_loc),
            items,
            labels: self.labels.clone(),
        }
    }

    fn expect_token(&mut self, kind: TokenKind) {
        if self.check(kind) {
            self.advance();
        } else {
            let message = format!("Expected {:?} but found {:?}", kind, self.current.kind);
            self.error(message, self.current.span, None);
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
    fn error(&mut self, message: String, span: Span, help: Option<String>) {
        self.errors.push(CompileError {
            message,
            span,
            help,
        });
    }

    /// Parse opcode and operands
    fn parse_instruction(&mut self) -> Instruction {
        let loc = self.current.span;
        let key = capitalise(self.current.text);
        let opcode = if let Ok(opcode) = Opcode::from_str(key.as_str()) {
            opcode
        } else {
            self.error(format!("Invalid opcode: {}", self.current.text), loc, None);
            Opcode::Adc
        };
        self.expect_token(TokenKind::Opcode);
        let mut operands = Vec::new();
        loop {
            match self.current.kind {
                TokenKind::RegisterA | TokenKind::RegisterX | TokenKind::RegisterY => {
                    let register = if let Some(register) = Register::from_token(self.current.kind) {
                        register
                    } else {
                        self.error(
                            format!("Invalid register: {:#?}", self.current.kind),
                            self.current.span,
                            None,
                        );
                        Register::A
                    };
                    operands.push(Operand::Register(register));
                    self.advance();
                }
                TokenKind::Hash => {
                    operands.push(Operand::Immediate);
                    self.advance();
                }
                TokenKind::Comma => {
                    operands.push(Operand::Idx);
                    self.advance();
                }
                TokenKind::LeftBracket => {
                    operands.push(Operand::LBracket);
                    self.advance();
                }
                TokenKind::RightBracket => {
                    operands.push(Operand::RBracket);
                    self.advance();
                }
                TokenKind::Ident => operands.push(Operand::Ident(self.parse_ident())),
                TokenKind::Number => operands.push(Operand::Number(self.parse_number())),
                _ => {
                    break;
                }
            }
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
                self.error(
                    format!("Expected number, got '{}'", self.current.text),
                    loc,
                    None,
                );
                10
            }
        };
        let s = if base == 10 {
            self.current.text
        } else {
            &self.current.text[1..]
        };
        let value = if let Ok(value) = i32::from_str_radix(s, base) {
            value
        } else {
            self.error(
                format!("Unable to parse number '{}'", self.current.text),
                loc,
                None,
            );
            0
        };
        self.expect_token(TokenKind::Number);

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
        let mut value = self.current.text.to_string();
        self.expect_token(TokenKind::String);
        // Remove string identifiers
        value = value.trim_matches(|c| c == '"' || c == '\'').to_string();

        StringLiteral {
            id: next_node_id(),
            span: loc,
            value,
        }
    }

    /// Parse preprocessor and operands
    fn parse_preprocessor(&mut self) -> Preprocessor {
        let loc = self.current.span;
        let key = capitalise(self.current.text.strip_prefix('.').unwrap());
        let directive = if let Ok(directive) = Directive::from_str(key.as_str()) {
            directive
        } else {
            self.error(
                format!("Invalid directive: {}", self.current.text),
                loc,
                None,
            );
            Directive::Set
        };
        self.expect_token(TokenKind::Preprocessor);
        let mut args = Vec::new();
        while !self.lexer.at_end() {
            match self.current.kind {
                TokenKind::Ident => args.push(DirectiveItem::Ident(self.parse_ident())),
                TokenKind::Number => args.push(DirectiveItem::Number(self.parse_number())),
                TokenKind::String => args.push(DirectiveItem::String(self.parse_string())),
                TokenKind::Comma => self.advance(),
                _ => break,
            }
        }

        Preprocessor {
            id: next_node_id(),
            span: self.span_from(loc),
            directive,
            args,
        }
    }

    fn parse_label(&mut self) -> Label {
        let loc = self.current.span;
        let label = self.current.text.strip_suffix(':').unwrap();
        self.labels.insert(label.to_string());

        self.expect_token(TokenKind::Label);

        Label {
            id: next_node_id(),
            span: loc,
            label: label.to_string(),
        }
    }

    fn is_opcode(&self) -> bool {
        self.current.kind == TokenKind::Opcode
    }

    fn is_preprocessor(&self) -> bool {
        self.current.kind == TokenKind::Preprocessor
    }

    fn is_label(&self) -> bool {
        self.current.kind == TokenKind::Label
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
                assert_eq!(instr.opcode, Opcode::Lda);
                assert_eq!(instr.operands.len(), 2);
                match &instr.operands[1] {
                    Operand::Number(x) => assert_eq!(x.value, 0x10),
                    other => panic!("Expected number, got {:#?}", other),
                }
            }
            other => panic!("Expected instruction, got {:#?}", other),
        }
    }
}
