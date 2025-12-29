use std::{str::FromStr, sync::atomic::AtomicUsize};

use strum::{EnumString, IntoStaticStr};

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
}

#[derive(Debug, Clone)]
pub enum ProgramItem {
    Preprocessor(Preprocessor),
    Instruction(Instruction),
    Number(Number),
    Ident(Ident),
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
    String(String),
}

#[derive(Debug, Clone)]
pub struct Instruction {
    pub id: NodeId,
    pub span: Span,
    pub opcode: Opcode,
    pub operands: Vec<Operands>,
}

#[derive(Debug, Clone)]
pub enum Operands {
    Number(Number),
    Ident(Ident),
    String(String),
    Register(Register),
}

#[derive(Debug, Clone)]
pub enum Register {
    RegisterA,
    RegisterX,
    RegisterY,
}

#[derive(Debug, Clone)]
pub struct Number {
    pub id: NodeId,
    pub span: Span,
    pub value: i16,
}

#[derive(Debug, Clone)]
pub struct Ident {
    pub id: NodeId,
    pub span: Span,
    pub value: String,
}

#[derive(Clone, Copy, Debug, EnumString, IntoStaticStr)]
pub enum Directive {
    INCLUDE,
    DEFINE,
    IFDEF,
    IFNDEF,
    ENDIF,
}

impl Directive {
    pub fn is_directive(s: &str) -> bool {
        let key = s.strip_prefix('.').unwrap_or(" ");
        Directive::from_str(key).is_ok()
    }
}

#[derive(Clone, Copy, Debug, EnumString, IntoStaticStr)]
pub enum Opcode {
    ADC,
    AND,
    ASL,
    BCC,
    BCS,
    BEQ,
    BIT,
    BMI,
    BNE,
    BPL,
    BRK,
    BVC,
    BVS,
    CLC,
    CLD,
    CLI,
    CLV,
    CMP,
    CPX,
    CPY,
    DEC,
    DEX,
    DEY,
    EOR,
    INC,
    INX,
    INY,
    JMP,
    JSR,
    LDA,
    LDX,
    LDY,
    LSR,
    NOP,
    ORA,
    PHA,
    PHP,
    PLA,
    PLP,
    ROL,
    ROR,
    RTI,
    RTS,
    SBC,
    SEC,
    SED,
    SEI,
    STA,
    STX,
    STY,
    TAX,
    TAY,
    TSX,
    TXA,
    TXS,
    TYA,
    INVALID_OPCODE,
}

impl Opcode {
    pub fn is_opcode(s: &str) -> bool {
        Opcode::from_str(s).is_ok()
    }
}
