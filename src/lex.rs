//! A 6502 assembly lexer
//!
//! The [`Lexer`] struct converts text to a series of [`Token`]s

use std::str::CharIndices;

use phf::phf_map;
use unicode_ident::{is_xid_continue, is_xid_start};

use crate::{ast::Span, T};

/// `TokenKind` enum lists each token expected in a 6502 assembly file
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TokenKind {
    // Keywords
    Opcode,    // e.g. LDA
    RegisterA, // "A"
    RegisterX, // "X"
    RegisterY, // "Y"

    // Preprocessors
    InesprgPP, // ".inesprg"
    IneschrPP, // ".ineschr"
    InesmapPP, // ".inesmap"
    InesmirPP, // ".inesmir"
    DbPP,      // ".db"
    DwPP,      // ".dw"
    IncbinPP,  // ".incbin"
    PadPP,     // ".pad"
    OrgPP,     // ".org"
    SetPP,     // ".set"

    // Delimiters
    Comma,        // ","
    LeftBracket,  // "("
    RightBracket, // ")"

    // Identifiers
    Number, // [0-9]+
    Ident,  // XID_Start XID_Continue*
    String, // "..." or '...'
    Label,  // XID_Start XID_Continue*:

    // Special tokens
    Hash,        // "#"
    Colon,       // ":"
    LessThan,    // "<"
    GreaterThan, // ">"

    // Other
    InvalidToken,
    Eof,
}

/// Wrapper for [`TokenKind`] to give source text and [`Span`] information
#[derive(Debug, Clone)]
pub struct Token<'source> {
    pub kind: TokenKind,
    pub text: &'source str,
    pub span: Span,
}

/// Hand-coded 6502 assembly lexer
pub struct Lexer<'source> {
    source: &'source str,
    iter: CharIndices<'source>,
    c: char,
    pos: usize,
    line: usize,
}

impl<'source> Lexer<'source> {
    /// Create a new `Lexer` instance
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
                    kind: T![eof],
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
                ',' => T![,],
                '(' => T!['('],
                ')' => T![')'],
                ':' => T![:],
                '#' => T![#],
                '<' => T![<],
                '>' => T![>],

                // Comment
                ';' => {
                    while !self.at_end() && self.peek_char() != '\n' {
                        self.advance();
                    }
                    continue;
                }

                // Preprocessor
                '.' => {
                    self.advance();
                    while is_xid_continue(self.peek_char()) {
                        self.advance();
                    }

                    let text = self.source[start_pos..self.pos]
                        .strip_prefix('.')
                        .expect("Already checked to start with '.' above");
                    match text.to_ascii_lowercase().as_str() {
                        "inesprg" => T![inesprg],
                        "ineschr" => T![ineschr],
                        "inesmap" => T![inesmap],
                        "inesmir" => T![inesmir],
                        "db" => T![db],
                        "dw" => T![dw],
                        "incbin" => T![incbin],
                        "pad" => T![pad],
                        "org" => T![org],
                        "set" => T![set],
                        _ => TokenKind::InvalidToken,
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

                    T![string]
                }

                // Numbers
                c if (c.is_ascii_digit() || c == '$' || c == '%') => {
                    if !c.is_ascii_digit() {
                        self.advance();
                    }
                    while self.peek_char().is_ascii_hexdigit() || self.peek_char() == 'x' {
                        self.advance();
                    }
                    T![number]
                }

                // Keywords
                c if is_xid_start(c) => {
                    while is_xid_continue(self.peek_char()) {
                        self.advance();
                    }

                    if self.peek_char() == ':' {
                        self.advance();
                        T![label]
                    } else {
                        let text = &self.source[start_pos..self.pos].to_ascii_uppercase();
                        KEYWORDS.get(text).copied().unwrap_or(T![ident])
                    }
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

    /// Skip whitespace characters until next token
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

    /// Check if source file finished
    pub fn at_end(&self) -> bool {
        self.pos >= self.source.len()
    }
}

/// More readable syntax to describe [`TokenKind`]
#[macro_export]
macro_rules ! T {
    [A] => { $ crate::lex::TokenKind::RegisterA };
    [X] => { $ crate::lex::TokenKind::RegisterX };
    [Y] => { $ crate::lex::TokenKind::RegisterY };
    [,] => { $ crate::lex::TokenKind::Comma };
    ['('] => { $ crate::lex::TokenKind::LeftBracket };
    [')'] => { $ crate::lex::TokenKind::RightBracket };
    [number] => { $ crate::lex::TokenKind::Number };
    [ident] => { $ crate::lex::TokenKind::Ident };
    [string] => { $ crate::lex::TokenKind::String };
    [label] => { $ crate::lex::TokenKind::Label };
    [opcode] => { $ crate::lex::TokenKind::Opcode };
    [#] => { $ crate::lex::TokenKind::Hash };
    [:] => { $ crate::lex::TokenKind::Colon };
    [<] => { $ crate::lex::TokenKind::LessThan };
    [>] => { $ crate::lex::TokenKind::GreaterThan };
    [invalid] => { $ crate::lex::TokenKind::InvalidToken };
    [eof] => { $ crate::lex::TokenKind::Eof };
    [inesprg] => { $ crate::lex::TokenKind::InesprgPP };
    [ineschr] => { $ crate::lex::TokenKind::IneschrPP };
    [inesmap] => { $ crate::lex::TokenKind::InesmapPP };
    [inesmir] => { $ crate::lex::TokenKind::InesmirPP };
    [db] => { $ crate::lex::TokenKind::DbPP };
    [dw] => { $ crate::lex::TokenKind::DwPP };
    [incbin] => { $ crate::lex::TokenKind::IncbinPP };
    [pad] => { $ crate::lex::TokenKind::PadPP };
    [org] => { $ crate::lex::TokenKind::OrgPP };
    [set] => { $ crate::lex::TokenKind::SetPP };
}

/// Map keyword text to corresponding [`TokenKind`]
static KEYWORDS: phf::Map<&'static str, TokenKind> = phf_map! {
    "A" => T![A],
    "X" => T![X],
    "Y" => T![Y],
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
    "TXS" | "TYA" => T![opcode],
};

#[cfg(test)]
mod tests {
    use super::*;

    fn lex(source: &str) -> Vec<Token<'_>> {
        let mut lexer = Lexer::new(source);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token();
            let is_eof = token.kind == T![eof];
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
        assert_eq!(tokens[0].kind, T![eof]);
    }

    #[test]
    fn test_whitespace_only() {
        let tokens = lex("  \t\n\r ");
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0].kind, T![eof]);
    }

    #[test]
    fn test_single_char_tokens() {
        let tokens = lex(":(),#<>");
        assert_eq!(tokens.len(), 7 + 1);
        assert_eq!(tokens[0].kind, T![:]);
        assert_eq!(tokens[1].kind, T!['(']);
        assert_eq!(tokens[2].kind, T![')']);
        assert_eq!(tokens[3].kind, T![,]);
        assert_eq!(tokens[4].kind, T![#]);
        assert_eq!(tokens[5].kind, T![<]);
        assert_eq!(tokens[6].kind, T![>]);
        assert_eq!(tokens[7].kind, T![eof]);
    }

    #[test]
    fn test_keywords() {
        let tokens = lex("A X Y ADC LDA TXA");
        assert_eq!(tokens.len(), 6 + 1);
        assert_eq!(tokens[0].kind, T![A]);
        assert_eq!(tokens[1].kind, T![X]);
        assert_eq!(tokens[2].kind, T![Y]);
        assert_eq!(tokens[3].kind, T![opcode]);
        assert_eq!(tokens[4].kind, T![opcode]);
        assert_eq!(tokens[5].kind, T![opcode]);
        assert_eq!(tokens[6].kind, T![eof]);
    }

    #[test]
    fn test_numbers() {
        let tokens = lex("0 $F0 %101 234 $10 $FF10");
        assert_eq!(tokens.len(), 6 + 1);
        assert_eq!(tokens[0].kind, T![number]);
        assert_eq!(tokens[0].text, "0");
        assert_eq!(tokens[1].kind, T![number]);
        assert_eq!(tokens[1].text, "$F0");
        assert_eq!(tokens[2].kind, T![number]);
        assert_eq!(tokens[2].text, "%101");
        assert_eq!(tokens[3].kind, T![number]);
        assert_eq!(tokens[3].text, "234");
        assert_eq!(tokens[4].kind, T![number]);
        assert_eq!(tokens[4].text, "$10");
        assert_eq!(tokens[5].kind, T![number]);
        assert_eq!(tokens[5].text, "$FF10");
        assert_eq!(tokens[6].kind, T![eof]);
    }

    #[test]
    fn test_preprocessor() {
        let tokens = lex("
            .inesprg
            .ineschr
            .inesmap
            .db
            .dw
            .incbin
            .pad
            .org
            .set
        ");
        assert_eq!(tokens.len(), 9 + 1);
        assert_eq!(tokens[0].kind, T![inesprg]);
        assert_eq!(tokens[1].kind, T![ineschr]);
        assert_eq!(tokens[2].kind, T![inesmap]);
        assert_eq!(tokens[3].kind, T![db]);
        assert_eq!(tokens[4].kind, T![dw]);
        assert_eq!(tokens[5].kind, T![incbin]);
        assert_eq!(tokens[6].kind, T![pad]);
        assert_eq!(tokens[7].kind, T![org]);
        assert_eq!(tokens[8].kind, T![set]);
        assert_eq!(tokens[9].kind, T![eof]);
    }

    #[test]
    fn test_string() {
        let tokens = lex("\"Hello\" 'filename/test' \"Test \\\'test\\\'\" ");
        assert_eq!(tokens.len(), 3 + 1);
        assert_eq!(tokens[0].kind, T![string]);
        assert_eq!(tokens[1].kind, T![string]);
        assert_eq!(tokens[2].kind, T![string]);
        assert_eq!(tokens[3].kind, T![eof]);
    }
}
