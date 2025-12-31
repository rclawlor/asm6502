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

impl Span {
    /// Get span up to end
    pub fn up_to(&self, end: Span) -> Span {
        Span {
            start: self.start,
            end: end.end,
        }
    }
}

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
}

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

#[derive(Debug, Clone)]
pub struct Label {
    pub id: NodeId,
    pub span: Span,
    pub label: String,
}

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

#[derive(Debug, Clone)]
pub struct Number {
    pub id: NodeId,
    pub span: Span,
    pub value: i32,
}

#[derive(Debug, Clone)]
pub struct Ident {
    pub id: NodeId,
    pub span: Span,
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub id: NodeId,
    pub span: Span,
    pub value: String,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, EnumString, IntoStaticStr)]
pub enum Directive {
    Set,
    Org,
}

impl Directive {
    pub fn is_directive(s: &str) -> bool {
        Directive::from_str(s).is_ok()
    }
}

#[derive(AsRefStr, Clone, Copy, Debug, Hash, PartialEq, Eq, EnumString, IntoStaticStr)]
pub enum Opcode {
    Adc,
    And,
    Asl,
    Bcc,
    Bcs,
    Beq,
    Bit,
    Bmi,
    Bne,
    Bpl,
    Brk,
    Bvc,
    Bvs,
    Clc,
    Cld,
    Cli,
    Clv,
    Cmp,
    Cpx,
    Cpy,
    Dec,
    Dex,
    Dey,
    Eor,
    Inc,
    Inx,
    Iny,
    Jmp,
    Jsr,
    Lda,
    Ldx,
    Ldy,
    Lsr,
    Nop,
    Ora,
    Pha,
    Php,
    Pla,
    Plp,
    Rol,
    Ror,
    Rti,
    Rts,
    Sbc,
    Sec,
    Sed,
    Sei,
    Sta,
    Stx,
    Sty,
    Tax,
    Tay,
    Tsx,
    Txa,
    Txs,
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
