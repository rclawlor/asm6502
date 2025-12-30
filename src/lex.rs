use std::str::CharIndices;

use phf::phf_map;
use unicode_ident::{is_xid_continue, is_xid_start};

use crate::{ast::{Directive, Span}, parse::capitalise};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    // Keywords
    Opcode,       // e.g. LDA
    RegisterA,    // "A"
    RegisterX,    // "X"
    RegisterY,    // "Y"
    Preprocessor, // e.g. .include

    // Delimiters
    Comma,        // ","
    LeftBracket,  // "("
    RightBracket, // ")"

    // Identifiers
    Number, // [0-9]+
    Ident,  // XID_Start XID_Continue*
    String, // "..." or '...'

    // Special tokens
    Hash,      // "#"
    Colon,     // ":"
    SemiColon, // ";"

    // Other
    InvalidToken,
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token<'source> {
    pub kind: TokenKind,
    pub text: &'source str,
    pub span: Span,
}

pub struct Lexer<'source> {
    source: &'source str,
    iter: CharIndices<'source>,
    c: char,
    pos: usize,
    line: usize,
}

impl<'source> Lexer<'source> {
    /// Create a new Lexer instance
    pub fn new(source: &'source str) -> Self {
        let mut lexer = Self {
            source,
            iter: source.char_indices(),
            c: '\0',
            pos: 0,
            line: 0,
        };

        lexer.advance();
        lexer
    }

    /// Find next token
    pub fn next_token(&mut self) -> Token<'source> {
        loop {
            self.skip_whitespace();

            if self.at_end() {
                return Token {
                    kind: TokenKind::Eof,
                    text: "",
                    span: Span {
                        start: self.source.len(),
                        end: self.source.len(),
                    },
                };
            }

            let start_pos = self.pos;

            let token_kind = match self.next_char() {
                // Single-character tokens
                ',' => TokenKind::Comma,
                '(' => TokenKind::LeftBracket,
                ')' => TokenKind::RightBracket,
                ':' => TokenKind::Colon,
                '#' => TokenKind::Hash,

                // Comment
                ';' => {
                    if self.accept(';') {
                        while !self.at_end() && self.peek_char() != '\n' {
                            self.advance();
                        }
                        continue;
                    }
                    TokenKind::SemiColon
                }

                // Preprocessor
                '.' => {
                    self.advance();
                    while is_xid_continue(self.peek_char()) {
                        self.advance();
                    }

                    let text = &self.source[start_pos..self.pos]
                        .strip_prefix('.')
                        .expect("Already checked to start with '.' above");
                    if Directive::is_directive(&capitalise(text)) {
                        TokenKind::Preprocessor
                    } else {
                        TokenKind::InvalidToken
                    }
                }

                c if (c == '"') | (c == '\'') => {
                    let quote = c;
                    let mut escaped = false;
                    while (self.peek_char() != quote || escaped)
                        && !self.at_end()
                        && self.peek_char() != '\n'
                    {
                        escaped = self.peek_char() == '\\';
                        self.advance();
                    }
                    self.advance();

                    TokenKind::String
                }

                // Numbers
                c if (c.is_ascii_digit() || c == '$' || c == '%') => {
                    if !c.is_ascii_digit() {
                        self.advance();
                    }
                    while self.peek_char().is_ascii_hexdigit() || self.peek_char() == 'x' {
                        self.advance();
                    }
                    TokenKind::Number
                }

                // Keywords
                c if is_xid_start(c) => {
                    while is_xid_continue(self.peek_char()) {
                        self.advance();
                    }

                    let text = &self.source[start_pos..self.pos];
                    KEYWORDS.get(text).copied().unwrap_or(TokenKind::Ident)
                }

                _ => TokenKind::InvalidToken,
            };

            let text = &self.source[start_pos..self.pos];

            return Token {
                kind: token_kind,
                text,
                span: Span {
                    start: start_pos,
                    end: self.pos,
                },
            };
        }
    }

    /// Skip all whitespace characters
    fn skip_whitespace(&mut self) {
        if self.peek_char() == '\n' {
            self.line += 1;
        }
        while self.peek_char().is_whitespace() {
            self.advance();
        }
    }

    /// Advance to next char
    fn advance(&mut self) {
        if let Some((pos, c)) = self.iter.next() {
            self.pos = pos;
            self.c = c;
        } else {
            self.pos = self.source.len();
            self.c = '\0';
        }
    }

    /// Consume and move to next char
    fn next_char(&mut self) -> char {
        let c = self.c;
        self.advance();
        c
    }

    /// Check next char
    fn peek_char(&self) -> char {
        self.c
    }

    /// Consume next char on match
    fn accept(&mut self, c: char) -> bool {
        if self.peek_char() == c {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Check if source file finished
    fn at_end(&self) -> bool {
        self.pos >= self.source.len()
    }
}

static KEYWORDS: phf::Map<&'static str, TokenKind> = phf_map! {
    "A" => TokenKind::RegisterA,
    "X" => TokenKind::RegisterX,
    "Y" => TokenKind::RegisterY,
    "ADC" | "AND" | "ASL" |
    "BCC" | "BCS" | "BEQ" |
    "BIT" | "BMI" | "BNE" |
    "BPL" | "BRK" | "BVC" |
    "BVS" | "CLC" | "CLD" |
    "CLI" | "CLV" | "CMP" |
    "CPX" | "CPY" | "DEC" |
    "DEX" | "DEY" | "EOR" |
    "INC" | "INX" | "INY" |
    "JMP" | "JSR" | "LDA" |
    "LDX" | "LDY" | "LSR" |
    "NOP" | "ORA" | "PHA" |
    "PHP" | "PLA" | "PLP" |
    "ROL" | "ROR" | "RTI" |
    "RTS" | "SBC" | "SEC" |
    "SED" | "SEI" | "STA" |
    "STX" | "STY" | "TAX" |
    "TAY" | "TSX" | "TXA" |
    "TXS" | "TYA" => TokenKind::Opcode,
};

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(source: &str) -> Vec<Token<'_>> {
        let mut lexer = Lexer::new(source);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token();
            let is_eof = token.kind == TokenKind::Eof;
            tokens.push(token);
            if is_eof {
                break;
            }
        }
        tokens
    }

    #[test]
    fn test_empty() {
        let tokens = lex("");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::Eof);
    }

    #[test]
    fn test_whitespace_only() {
        let tokens = lex("  \t\n\r ");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, TokenKind::Eof);
    }

    #[test]
    fn test_single_char_tokens() {
        let tokens = lex(":;(),#");
        assert_eq!(tokens.len(), 6 + 1);
        assert_eq!(tokens[0].kind, TokenKind::Colon);
        assert_eq!(tokens[1].kind, TokenKind::SemiColon);
        assert_eq!(tokens[2].kind, TokenKind::LeftBracket);
        assert_eq!(tokens[3].kind, TokenKind::RightBracket);
        assert_eq!(tokens[4].kind, TokenKind::Comma);
        assert_eq!(tokens[5].kind, TokenKind::Hash);
        assert_eq!(tokens[6].kind, TokenKind::Eof);
    }

    #[test]
    fn test_keywords() {
        let tokens = lex("A X Y ADC LDA TXA");
        assert_eq!(tokens.len(), 6 + 1);
        assert_eq!(tokens[0].kind, TokenKind::RegisterA);
        assert_eq!(tokens[1].kind, TokenKind::RegisterX);
        assert_eq!(tokens[2].kind, TokenKind::RegisterY);
        assert_eq!(tokens[3].kind, TokenKind::Opcode);
        assert_eq!(tokens[4].kind, TokenKind::Opcode);
        assert_eq!(tokens[5].kind, TokenKind::Opcode);
        assert_eq!(tokens[6].kind, TokenKind::Eof);
    }

    #[test]
    fn test_numbers() {
        let tokens = lex("0 $F0 %101 234 $10 $FF10");
        println!("{:#?}", tokens);
        assert_eq!(tokens.len(), 6 + 1);
        assert_eq!(tokens[0].kind, TokenKind::Number);
        assert_eq!(tokens[0].text, "0");
        assert_eq!(tokens[1].kind, TokenKind::Number);
        assert_eq!(tokens[1].text, "$F0");
        assert_eq!(tokens[2].kind, TokenKind::Number);
        assert_eq!(tokens[2].text, "%101");
        assert_eq!(tokens[3].kind, TokenKind::Number);
        assert_eq!(tokens[3].text, "234");
        assert_eq!(tokens[4].kind, TokenKind::Number);
        assert_eq!(tokens[4].text, "$10");
        assert_eq!(tokens[5].kind, TokenKind::Number);
        assert_eq!(tokens[5].text, "$FF10");
        assert_eq!(tokens[6].kind, TokenKind::Eof);
    }

    #[test]
    fn test_preprocessor() {
        let tokens = lex(".include .INCLUDE .ifdef .endif");
        assert_eq!(tokens.len(), 4 + 1);
        assert_eq!(tokens[0].kind, TokenKind::Preprocessor);
        assert_eq!(tokens[1].kind, TokenKind::Preprocessor);
        assert_eq!(tokens[2].kind, TokenKind::Preprocessor);
        assert_eq!(tokens[3].kind, TokenKind::Preprocessor);
        assert_eq!(tokens[4].kind, TokenKind::Eof);
    }

    #[test]
    fn test_string() {
        let tokens = lex("\"Hello\" 'filename/test' \"Test \\\'test\\\'\" ");
        assert_eq!(tokens.len(), 3 + 1);
        assert_eq!(tokens[0].kind, TokenKind::String);
        assert_eq!(tokens[1].kind, TokenKind::String);
        assert_eq!(tokens[2].kind, TokenKind::String);
        assert_eq!(tokens[3].kind, TokenKind::Eof);
    }
}
