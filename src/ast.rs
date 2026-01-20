//! Contains the AST node types generated during parsing

use std::{collections::HashMap, str::FromStr, sync::atomic::AtomicUsize};

use strum::{AsRefStr, EnumString, IntoStaticStr};

use crate::{
    lex::TokenKind,
    semantic::{AddressMode, INSTRUCTION_SET},
    T,
};

/// ID of AST node
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(usize);

/// Used to track what AST node IDs have been used
static NEXT_NODE_ID: AtomicUsize = AtomicUsize::new(0);

/// Get next available AST node ID
pub fn next_node_id() -> NodeId {
    NodeId(NEXT_NODE_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
}

/// Span of AST node in source
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

/// Data required to generate an iNES header for emulators
#[derive(Debug, Clone)]
pub struct INesHeader {
    pub prg_size_16kb: u8,
    pub chr_size_16kb: u8,
    pub mapper: u8,
    pub mirror: u8,
}

impl INesHeader {
    pub fn new() -> INesHeader {
        INesHeader {
            prg_size_16kb: 1,
            chr_size_16kb: 1,
            mapper: 1,
            mirror: 0,
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Program {
    pub id: NodeId,
    pub span: Span,
    pub items: Vec<ProgramItem>,
    pub label_definitions: HashMap<String, NodeId>,
    pub constant_definitions: HashMap<String, NodeId>,
}

#[derive(Debug, Clone)]
pub enum ProgramItem {
    Preprocessor(Preprocessor),
    Instruction(Instruction),
    Label(Label),
    Binary(Binary),
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Preprocessor {
    pub id: NodeId,
    pub span: Span,
    pub directive: Directive,
}

#[derive(Debug, Clone)]
pub enum DirectiveItem {
    Number(Number),
    Ident(Ident),
    String(StringLiteral),
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Label {
    pub id: NodeId,
    pub span: Span,
    pub label: String,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Binary {
    pub id: NodeId,
    pub span: Span,
    pub filename: String,
    pub bytes: Vec<u8>,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Instruction {
    pub id: NodeId,
    pub span: Span,
    pub opcode: Opcode,
    pub operands: Vec<Operand>,
}

#[derive(Debug, Clone)]
pub enum Operand {
    Number(Number, Option<ByteSelect>),
    Ident(Ident, Option<ByteSelect>),
    Register(Register),
    Immediate,
    Idx,
    LBracket,
    RBracket,
    AddrLabel(String, Option<ByteSelect>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ByteSelect {
    Low,
    High,
}

#[derive(Debug, Clone)]
pub enum Register {
    A,
    X,
    Y,
}

impl Register {
    pub fn from_token(token: TokenKind) -> Option<Register> {
        match token {
            T![A] => Some(Register::A),
            T![X] => Some(Register::X),
            T![Y] => Some(Register::Y),
            _ => None,
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
pub struct Number {
    pub id: NodeId,
    pub span: Span,
    pub value: i32,
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub id: NodeId,
    pub span: Span,
    pub value: String,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub id: NodeId,
    pub span: Span,
    pub value: String,
}

#[derive(Debug, Clone)]
pub enum ValueExpr {
    Number(Number),
    Ident(Ident),
}

#[derive(Clone, Debug)]
pub enum Directive {
    Inesprg {
        id: NodeId,
        span: Span,
        size: Number,
    },
    Ineschr {
        id: NodeId,
        span: Span,
        size: Number,
    },
    Inesmap {
        id: NodeId,
        span: Span,
        map: Number,
    },
    Inesmir {
        id: NodeId,
        span: Span,
        mirror: Number,
    },
    Db {
        id: NodeId,
        span: Span,
        bytes: Vec<ValueExpr>,
    },
    Dw {
        id: NodeId,
        span: Span,
        words: Vec<ValueExpr>,
    },
    Incbin {
        id: NodeId,
        span: Span,
        filename: StringLiteral,
    },
    Pad {
        id: NodeId,
        span: Span,
        target_addr: Number,
    },
    Org {
        id: NodeId,
        span: Span,
        address: Number,
    },
    Set {
        id: NodeId,
        span: Span,
        ident: Ident,
        value: Number,
    },
}

/// All opcodes in the 6502 (excludes illegal opcodes)
#[derive(AsRefStr, Clone, Copy, Debug, Hash, PartialEq, Eq, EnumString, IntoStaticStr)]
pub enum Opcode {
    /// Add with Carry
    Adc,
    /// And with accumulator
    And,
    /// Arithmetic shift left
    Asl,
    /// Branch on carry clear
    Bcc,
    /// Branch on carry set
    Bcs,
    /// Branch on equal
    Beq,
    /// Test bits
    Bit,
    /// Branch on result minus
    Bmi,
    /// Branch on not equal
    Bne,
    /// Branch on result plus
    Bpl,
    /// Force break
    Brk,
    /// Branch on overflow clear
    Bvc,
    /// Branch on overflow set
    Bvs,
    /// Clear carry flag
    Clc,
    /// Clear decimal flag
    Cld,
    /// Clear interrupt disable bit
    Cli,
    /// Clear overflow flag
    Clv,
    /// Compare memory with accumulator
    Cmp,
    /// Compare memory with X
    Cpx,
    /// Compare memory with Y
    Cpy,
    /// Decrement accumulator
    Dec,
    /// Decrement X
    Dex,
    /// Decrement Y
    Dey,
    /// XOR with accumulator
    Eor,
    /// Increment accumulator
    Inc,
    /// Increment X
    Inx,
    /// Increment Y
    Iny,
    /// Jump to address
    Jmp,
    /// Jump to subroutine
    Jsr,
    /// Load value to accumulator
    Lda,
    /// Load value to X
    Ldx,
    /// Load value to Y
    Ldy,
    /// Logical shift right
    Lsr,
    /// No operation
    Nop,
    /// Or with accumulator
    Ora,
    /// Push accumulator on stack
    Pha,
    /// Push processor status on stack
    Php,
    /// Pull accumulator from stack
    Pla,
    /// Pull processor status from stack
    Plp,
    /// Rotate left
    Rol,
    /// Rotate right
    Ror,
    /// Return from interrupt
    Rti,
    /// Return from subroutine
    Rts,
    /// Subtract with carry
    Sbc,
    /// Set carry flag
    Sec,
    /// Set decimal flag
    Sed,
    /// Set interrupt disable status
    Sei,
    /// Store accumulator
    Sta,
    /// Store X
    Stx,
    /// Store Y
    Sty,
    /// Transfer A to X
    Tax,
    /// Transfer A to Y
    Tay,
    /// Transfer stack to X
    Tsx,
    /// Transfer X to A
    Txa,
    /// Transfer X to stack
    Txs,
    /// Transfer Y to A
    Tya,
}

impl Opcode {
    pub fn is_opcode(s: &str) -> bool {
        Opcode::from_str(s).is_ok()
    }

    pub fn is_implied_accumulator(&self) -> bool {
        matches!(self, Self::Asl | Self::Rol | Self::Lsr | Self::Ror)
    }

    pub fn is_relative(&self) -> bool {
        matches!(
            self,
            Self::Bcc
                | Self::Bcs
                | Self::Beq
                | Self::Bmi
                | Self::Bne
                | Self::Bpl
                | Self::Bvc
                | Self::Bvs
        )
    }

    pub fn address_modes(&self) -> Vec<AddressMode> {
        INSTRUCTION_SET
            .get(self.as_ref())
            .unwrap()
            .iter()
            .map(|mode| mode.mode)
            .collect()
    }

    pub fn value(&self, mode: AddressMode) -> Option<u8> {
        let values: Vec<_> = INSTRUCTION_SET.get(self.as_ref()).unwrap().iter().collect();
        for v in values {
            if v.mode == mode {
                return Some(v.opcode);
            }
        }
        None
    }

    pub fn as_help_str(&self) -> String {
        let idn = self.as_ref().to_ascii_uppercase();
        let modes: Vec<_> = self
            .address_modes()
            .iter()
            .map(|x| format!(" - {}", x.as_help_str(&idn)))
            .collect();
        format!("Available addressing modes:\n{}", modes.join("\n"))
    }
}
