use std::{collections::HashSet, str::FromStr};

use crate::{
    ast::*,
    error::CompileError,
    lex::{Lexer, Token, TokenKind},
    T,
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
        self.current.kind == T![eof]
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
        self.expect_token(T![opcode]);
        let mut operands = Vec::new();
        let mut byte_select = None;
        loop {
            match self.current.kind {
                T![A] | T![X] | T![Y] => {
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
                T![#] => {
                    operands.push(Operand::Immediate);
                    self.advance();
                }
                T![,] => {
                    operands.push(Operand::Idx);
                    self.advance();
                }
                T!['('] => {
                    operands.push(Operand::LBracket);
                    self.advance();
                }
                T![')'] => {
                    operands.push(Operand::RBracket);
                    self.advance();
                }
                T![<] => {
                    byte_select = Some(ByteSelect::Low);
                    self.advance();
                }
                T![>] => {
                    byte_select = Some(ByteSelect::High);
                    self.advance();
                }
                T![ident] => {
                    operands.push(Operand::Ident(self.parse_ident(), byte_select));
                    byte_select = None;
                }
                T![number] => {
                    operands.push(Operand::Number(self.parse_number(), byte_select));
                    byte_select = None;
                }
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
        self.expect_token(T![number]);

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
        self.expect_token(T![ident]);

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
        self.expect_token(T![string]);
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
                T![ident] => args.push(DirectiveItem::Ident(self.parse_ident())),
                T![number] => args.push(DirectiveItem::Number(self.parse_number())),
                T![string] => args.push(DirectiveItem::String(self.parse_string())),
                T![,] => self.advance(),
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

        self.expect_token(T![label]);

        Label {
            id: next_node_id(),
            span: loc,
            label: label.to_string(),
        }
    }

    fn is_opcode(&self) -> bool {
        self.current.kind == T![opcode]
    }

    fn is_preprocessor(&self) -> bool {
        self.current.kind == TokenKind::Preprocessor
    }

    fn is_label(&self) -> bool {
        self.current.kind == T![label]
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
    fn test_number() {
        let program = parse(
            "
            LDX $10
            LDY %10
            LDA 10
            LDX #$10
            LDY #%10
            LDA #10
        ",
        )
        .unwrap();
        assert_eq!(program.items.len(), 6);
        for (idx, item) in program.items.iter().enumerate() {
            if let ProgramItem::Instruction(instr) = item {
                assert_eq!(instr.operands.len(), 1 + usize::from(idx > 2));
            } else {
                panic!("Expected instruction");
            }
        }
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
                    Operand::Number(x, b) => {
                        assert_eq!(x.value, 0x10);
                        assert!(b.is_none());
                    }
                    other => panic!("Expected number, got {:#?}", other),
                }
            }
            other => panic!("Expected instruction, got {:#?}", other),
        }
    }

    #[test]
    fn test_byte_indexing() {
        let program = parse(
            "
            .set var $0102

            Example:
                LDA <Example
                LDX >Example
                LDY <var
        ",
        )
        .unwrap();
        assert_eq!(program.items.len(), 5);
        match &program.items[2] {
            ProgramItem::Instruction(instr) => {
                assert_eq!(instr.opcode, Opcode::Lda);
                assert_eq!(instr.operands.len(), 1);
                match &instr.operands[0] {
                    Operand::Ident(_, b) => {
                        assert_eq!(*b, Some(ByteSelect::Low));
                    }
                    other => panic!("Expected lower byte operator, got {:#?}", other),
                }
            }
            other => panic!("Expected instruction, got {:#?}", other),
        }
        match &program.items[3] {
            ProgramItem::Instruction(instr) => {
                assert_eq!(instr.opcode, Opcode::Ldx);
                assert_eq!(instr.operands.len(), 1);
                match &instr.operands[0] {
                    Operand::Ident(_, b) => assert_eq!(*b, Some(ByteSelect::High)),
                    other => panic!("Expected upper byte operator, got {:#?}", other),
                }
            }
            other => panic!("Expected instruction, got {:#?}", other),
        }
        match &program.items[4] {
            ProgramItem::Instruction(instr) => {
                assert_eq!(instr.opcode, Opcode::Ldy);
                assert_eq!(instr.operands.len(), 1);
                match &instr.operands[0] {
                    Operand::Ident(_, b) => assert_eq!(*b, Some(ByteSelect::Low)),
                    other => panic!("Expected lower byte operator, got {:#?}", other),
                }
            }
            other => panic!("Expected instruction, got {:#?}", other),
        }
    }

    #[test]
    fn test_db_dw_preprocessor() {
        let program = parse(
            "
            .db $01, $02
            .dw $01, $0200
        ",
        )
        .unwrap();
        assert_eq!(program.items.len(), 2);
        match program.items.first() {
            Some(ProgramItem::Preprocessor(pp)) => {
                assert_eq!(pp.directive, Directive::Db);
                assert_eq!(pp.args.len(), 2);
            }
            _ => panic!("Expected .db preprocessor"),
        }
        match program.items.get(1) {
            Some(ProgramItem::Preprocessor(pp)) => {
                assert_eq!(pp.directive, Directive::Dw);
                assert_eq!(pp.args.len(), 2);
            }
            _ => panic!("Expected .dw preprocessor"),
        }
    }
}
