//! A 6502 assembly parser
//!
//! See [`Parser`] for details

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

/// 6502 assembly parser
///
/// Parses tokens generated from the [`Lexer`] and generates AST
/// nodes found in [`crate::ast`].
///
/// If the parser encounters any invalid syntax it will append a
/// [`CompileError`] but continue parsing until the file is finished.
struct Parser<'source> {
    lexer: Lexer<'source>,
    current: Token<'source>,
    previous: Token<'source>,
    labels: HashSet<String>,
    errors: Vec<CompileError>,
}

impl<'source> Parser<'source> {
    /// Create a new `Parser` instance
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

    /// Parse the source file and generate a [`Program`], containing
    /// a vec of AST nodes and unresolved labels
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

    /// Consume `TokenKind` if present, otherwise append a new `CompileError`
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

    /// Get span from starting span to current
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
                }
                T![#] => operands.push(Operand::Immediate),
                T![,] => operands.push(Operand::Idx),
                T!['('] => operands.push(Operand::LBracket),
                T![')'] => operands.push(Operand::RBracket),
                T![<] => byte_select = Some(ByteSelect::Low),
                T![>] => byte_select = Some(ByteSelect::High),
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
            self.advance();
        }

        Instruction {
            id: next_node_id(),
            span: self.span_from(loc),
            opcode,
            operands,
        }
    }

    /// Parse a number
    ///
    /// Supports hex, binary and decimal number representations
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
        let key = self
            .current
            .text
            .strip_prefix('.')
            .unwrap()
            .to_ascii_lowercase();

        self.advance();
        let directive = match key.as_str() {
            "inesprg" => {
                let size = self.parse_number();
                self.expect_token(T![number]);
                Directive::Inesprg {
                    id: next_node_id(),
                    span: self.span_from(loc),
                    size,
                }
            }
            "ineschr" => {
                let size = self.parse_number();
                self.expect_token(T![number]);
                Directive::Ineschr {
                    id: next_node_id(),
                    span: self.span_from(loc),
                    size,
                }
            }
            "inesmap" => {
                let map = self.parse_number();
                self.expect_token(T![number]);
                Directive::Inesmap {
                    id: next_node_id(),
                    span: self.span_from(loc),
                    map,
                }
            }
            "inesmir" => {
                let mirror = self.parse_number();
                self.expect_token(T![number]);
                Directive::Inesmir {
                    id: next_node_id(),
                    span: self.span_from(loc),
                    mirror,
                }
            }
            "db" => {
                let mut bytes = Vec::new();
                while !self.lexer.at_end() {
                    match self.current.kind {
                        T![ident] => bytes.push(ValueExpr::Ident(self.parse_ident())),
                        T![number] => bytes.push(ValueExpr::Number(self.parse_number())),
                        T![,] => (),
                        _ => break,
                    }
                    self.advance();
                }
                Directive::Db {
                    id: next_node_id(),
                    span: self.span_from(loc),
                    bytes,
                }
            }
            "dw" => {
                let mut words = Vec::new();
                while !self.lexer.at_end() {
                    match self.current.kind {
                        T![ident] => words.push(ValueExpr::Ident(self.parse_ident())),
                        T![number] => words.push(ValueExpr::Number(self.parse_number())),
                        T![,] => (),
                        _ => break,
                    }
                    self.advance();
                }
                Directive::Dw {
                    id: next_node_id(),
                    span: self.span_from(loc),
                    words,
                }
            }
            "incbin" => {
                let filename = self.parse_string();
                self.expect_token(T![string]);
                Directive::Incbin {
                    id: next_node_id(),
                    span: self.span_from(loc),
                    filename,
                }
            }
            "pad" => {
                let target_addr = self.parse_number();
                self.expect_token(T![number]);
                Directive::Pad {
                    id: next_node_id(),
                    span: self.span_from(loc),
                    target_addr,
                }
            }
            "org" => {
                let address = self.parse_number();
                self.expect_token(T![number]);
                Directive::Org {
                    id: next_node_id(),
                    span: self.span_from(loc),
                    address,
                }
            }
            "set" => {
                let ident = self.parse_ident();
                self.expect_token(T![ident]);
                let value = self.parse_number();
                self.expect_token(T![number]);
                Directive::Set {
                    id: next_node_id(),
                    span: self.span_from(loc),
                    ident,
                    value,
                }
            }
            other => {
                self.error(
                    format!("Unrecognised preprocessor: {other}"),
                    self.span_from(loc),
                    None,
                );
                self.advance();
                Directive::Db {
                    id: next_node_id(),
                    span: self.span_from(loc),
                    bytes: Vec::new(),
                }
            }
        };

        Preprocessor {
            id: next_node_id(),
            span: self.span_from(loc),
            directive,
        }
    }

    /// Parse address label
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
        matches!(
            self.current.kind,
            T![inesprg]
                | T![ineschr]
                | T![inesmap]
                | T![db]
                | T![dw]
                | T![incbin]
                | T![pad]
                | T![org]
                | T![set]
        )
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
                if let Directive::Db { bytes, .. } = &pp.directive {
                    assert_eq!(bytes.len(), 2);
                } else {
                    panic!("Expected .db directive");
                }
            }
            _ => panic!("Expected .db preprocessor"),
        }
        match program.items.get(1) {
            Some(ProgramItem::Preprocessor(pp)) => {
                if let Directive::Dw { words, .. } = &pp.directive {
                    assert_eq!(words.len(), 2);
                } else {
                    panic!("Expected .db directive");
                }
            }
            _ => panic!("Expected .dw preprocessor"),
        }
    }
}
