use std::{collections::HashSet, str::FromStr, sync::atomic::AtomicUsize};

use strum::{AsRefStr, EnumString, IntoStaticStr};

use crate::{
    lex::TokenKind,
    semantic::{AddressMode, INSTRUCTION_SET},
};

/// ID of AST node
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NodeId(usize);

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
    pub labels: HashSet<String>,
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
    pub args: Vec<DirectiveItem>,
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
    Number(Number),
    Ident(Ident),
    Register(Register),
    Immediate,
    Idx,
    LBracket,
    RBracket,
    AddrLabel(String),
    LowerByte,
    UpperByte,
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
            TokenKind::RegisterA => Some(Register::A),
            TokenKind::RegisterX => Some(Register::X),
            TokenKind::RegisterY => Some(Register::Y),
            _ => None,
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Number {
    pub id: NodeId,
    pub span: Span,
    pub value: i32,
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
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

#[derive(Clone, Copy, Debug, Eq, PartialEq, EnumString, IntoStaticStr)]
pub enum Directive {
    Inesprg,
    Ineschr,
    Inesmap,
    Inesmir,
    Db,
    Dw,
    Incbin,
    Pad,
    Org,
    Set,
}

impl Directive {
    pub fn is_directive(s: &str) -> bool {
        Directive::from_str(s).is_ok()
    }
}

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
