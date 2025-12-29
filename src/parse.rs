use crate::{
    ast::*,
    error::CompileError,
    lex::{Lexer, Token, TokenKind},
};

/// Parse source file
pub fn parse(source: &str) -> Result<(), Vec<CompileError>> {
    Ok(())
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

    fn parse_assembly(&mut self) -> Program {
        let start_loc = self.current.span;
        let mut items = Vec::new();
        while !self.at_end() {

        }

        Program {
            id: next_node_id(),
            span: self.span_from(start_loc),
            items
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
}
