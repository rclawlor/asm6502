//! Semantic analysis of the parsed 6502 assembly
//!
//! This involves:
//! - Resolving labels
//! - Expanding preprocessors
//! - Resolving opcode addressing modes

use std::collections::HashMap;

use phf::phf_map;
use strum::AsRefStr;

use crate::{ast::*, error::CompileError};

const UNKNOWN_ADDR: i32 = -0x0001;

pub fn semantic_analysis(ast: &Program) -> Result<AnalysedProgram, Vec<CompileError>> {
    let mut analyser = SemanticAnalyser::new(ast.clone());
    let program = analyser.analyse();
    if analyser.errors.is_empty() {
        Ok(program)
    } else {
        Err(analyser.errors)
    }
}

/// Post-semantic analysis program prepared for easy codegen
#[derive(Clone, Debug)]
pub struct AnalysedProgram {
    pub items: Vec<AnalysedItem>,
    pub header: INesHeader,
}

#[derive(Clone, Debug)]
pub enum AnalysedItem {
    Word(AnalysedWord),
    Byte(AnalysedByte),
    Instruction(AnalysedInstruction),
}

impl AnalysedItem {
    pub fn address(&self) -> u16 {
        match self {
            Self::Word(word) => word.address,
            Self::Byte(byte) => byte.address,
            Self::Instruction(instr) => instr.address,
        }
    }
}

impl std::fmt::Display for AnalysedItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Word(word) => write!(f, "{}", word),
            Self::Byte(byte) => write!(f, "{}", byte),
            Self::Instruction(instr) => write!(f, "{}", instr),
        }
    }
}

/// A raw byte from a `.db` or `.pad` preprocessor
#[derive(Clone, Debug)]
pub struct AnalysedByte {
    address: u16,
    pub value: u8,
}

impl std::fmt::Display for AnalysedByte {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#06x}: {:#04x}", self.address, self.value)
    }
}

/// A raw word from a `.dw` preprocessor
#[derive(Clone, Debug)]
pub struct AnalysedWord {
    address: u16,
    pub value: u16,
}

impl AnalysedWord {
    pub fn upper_byte(&self) -> u8 {
        ((self.value & 0xff00) >> 8).try_into().unwrap()
    }

    pub fn lower_byte(&self) -> u8 {
        (self.value & 0x00ff).try_into().unwrap()
    }
}

impl std::fmt::Display for AnalysedWord {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#06x}: {:#06x}", self.address, self.value)
    }
}

/// [`Opcode`] with its [`AddressMode`] determined from operands
#[derive(Clone, Debug)]
pub struct AnalysedInstruction {
    address: u16,
    pub opcode: Opcode,
    pub mode: AddressMode,
    pub operand: Option<i32>,
    pub span: Span,
}

impl std::fmt::Display for AnalysedInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:#06x}: {} {}",
            self.address,
            self.opcode.as_ref().to_ascii_uppercase(),
            match self.operand {
                Some(n) => {
                    if self.opcode.is_relative() {
                        format!("{n}   ({:#06x})", i32::from(self.address) + n)
                    } else {
                        format!("{n:#02x}")
                    }
                }
                None => String::new(),
            }
        )
    }
}

/// An address label that has not yet been defined
#[derive(Debug, Clone)]
struct UnknownLabel {
    idx: usize,
    byte_select: Option<ByteSelect>,
}

impl UnknownLabel {
    /// Create a new `UnknownLabel` instance
    fn new(idx: usize, byte_select: Option<ByteSelect>) -> UnknownLabel {
        UnknownLabel { idx, byte_select }
    }
}

fn value_from_byte_select(byte_select: Option<ByteSelect>, value: i32) -> i32 {
    match byte_select {
        Some(ByteSelect::Low) => value & 0x00FF,
        Some(ByteSelect::High) => (value & 0xFF00) >> 8,
        None => value,
    }
}

/// Extract single byte from value
fn byte_from_byte_select(byte_select: Option<ByteSelect>, value: i32) -> Option<i32> {
    match byte_select {
        Some(ByteSelect::Low) => Some(value & 0x00FF),
        Some(ByteSelect::High) => Some((value & 0xFF00) >> 8),
        None => {
            if value <= 0xFF {
                Some(value)
            } else {
                None
            }
        }
    }
}

/// Performs semantic analysis on parsed 6502 [`Program`]
///
/// It determines the opcode address modes, resolves outstanding preprocessors
/// and resolves address labels. If targetting the NES console, this also
/// creates the [`INesHeader`].
struct SemanticAnalyser {
    ast: Program,
    /// Current address
    address: u16,
    /// Post-analysis items
    items: Vec<AnalysedItem>,
    /// Node to corresponding address
    node_addresses: HashMap<NodeId, u16>,
    /// Node to corresponding value
    constant_values: HashMap<NodeId, i32>,
    /// Labels not yet found
    unknown_labels: HashMap<String, Vec<UnknownLabel>>,
    errors: Vec<CompileError>,
}

impl SemanticAnalyser {
    fn new(ast: Program) -> Self {
        SemanticAnalyser {
            ast,
            address: 0x0000,
            items: Vec::new(),
            node_addresses: HashMap::new(),
            constant_values: HashMap::new(),
            unknown_labels: HashMap::new(),
            errors: Vec::new(),
        }
    }

    /// Try convert i32 into u8, adding error if it exceeds range
    fn try_as_byte(&mut self, n: &Number) -> u8 {
        if n.value > 0xFF {
            self.error(String::from("Number exceeds 1 byte"), n.span, None);
            0x00
        } else {
            n.value as u8
        }
    }

    /// Analyse the resolved [`Program`] to prepare for codegen
    fn analyse(&mut self) -> AnalysedProgram {
        let mut header = INesHeader::new();
        for item in self.ast.items.clone() {
            match item {
                ProgramItem::Instruction(instr) => {
                    let i = self.analyse_instruction(&instr);
                    self.items.push(AnalysedItem::Instruction(i));
                }
                ProgramItem::Preprocessor(pp) => match pp.directive {
                    Directive::Inesprg { size, .. } => {
                        header.prg_size_16kb = self.try_as_byte(&size)
                    }
                    Directive::Ineschr { size, .. } => {
                        header.chr_size_16kb = self.try_as_byte(&size)
                    }
                    Directive::Inesmap { map, .. } => header.mapper = self.try_as_byte(&map),
                    Directive::Inesmir { mirror, .. } => header.mirror = self.try_as_byte(&mirror),
                    Directive::Org { address, .. } => {
                        self.address = u16::try_from(address.value).unwrap_or_else(|_| {
                            self.error(
                                format!("Address {:#10x} out of u16 range", address.value),
                                pp.span,
                                None,
                            );
                            0x0000
                        });
                    }
                    Directive::Db { bytes, .. } => {
                        for byte in self.analyse_byte(&bytes) {
                            self.items.push(AnalysedItem::Byte(byte));
                        }
                    }
                    Directive::Dw { words, .. } => {
                        for word in self.analyse_word(&words) {
                            self.items.push(AnalysedItem::Word(word));
                        }
                    }
                    Directive::Pad { target_addr, .. } => self.analyse_pad(&target_addr),
                    Directive::Set { ident, value, .. } => {
                        self.resolve_constant(&ident, &value);
                    }
                    Directive::Incbin { filename, .. } => {
                        let binary = match std::fs::read(&filename.value) {
                            Ok(b) => b,
                            Err(_) => {
                                self.error(
                                    format!("Unable to read file '{}'", filename.value),
                                    filename.span,
                                    None,
                                );
                                Vec::new()
                            }
                        };
                        for b in binary {
                            self.items.push(AnalysedItem::Byte(AnalysedByte {
                                address: self.address,
                                value: b,
                            }));
                            self.address = self.address.saturating_add(1);
                        }
                    }
                },
                ProgramItem::Label(label) => self.analyse_label(&label),
            }
        }
        AnalysedProgram {
            items: self.items.clone(),
            header,
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

    fn analyse_word(&mut self, words: &[ValueExpr]) -> Vec<AnalysedWord> {
        let values = words
            .iter()
            .map(|x| match x {
                ValueExpr::Number(n) => n.value,
                ValueExpr::Ident(i) => {
                    if let Some(id) = self.ast.label_definitions.get(&i.value) {
                        if let Some(addr) = self.node_addresses.get(id) {
                            *addr as i32
                        } else {
                            self.add_unknown_label(&i.value, None);
                            UNKNOWN_ADDR
                        }
                    } else {
                        self.error(format!("Label '{}' not found", i.value), i.span, None);
                        UNKNOWN_ADDR
                    }
                }
            })
            .collect::<Vec<i32>>();
        let mut words = Vec::new();
        for value in values {
            words.push(AnalysedWord {
                address: self.address,
                value: value as u16,
            });
            self.address = self.address.wrapping_add(2);
        }
        words
    }

    fn analyse_byte(&mut self, bytes: &[ValueExpr]) -> Vec<AnalysedByte> {
        let values = bytes
            .iter()
            .map(|x| match x {
                ValueExpr::Number(n) => n.value,
                ValueExpr::Ident(i) => {
                    self.error(
                        format!(
                            "2 byte address for label '{}' exceeds .db 1 byte range",
                            i.value
                        ),
                        i.span,
                        None,
                    );
                    0x00
                }
            })
            .collect::<Vec<i32>>();
        let mut bytes = Vec::new();
        for value in values {
            bytes.push(AnalysedByte {
                address: self.address,
                value: value as u8,
            });
            self.address = self.address.wrapping_add(1);
        }
        bytes
    }

    fn analyse_pad(&mut self, target_addr: &Number) {
        while self.address < (target_addr.value as u16) {
            self.items.push(AnalysedItem::Byte(AnalysedByte {
                address: self.address,
                value: 0x00,
            }));
            self.address = self.address.wrapping_add(1);
        }
    }

    fn get_definition_id(&self, s: &str) -> Option<&NodeId> {
        if let Some(id) = self.ast.label_definitions.get(s) {
            Some(id)
        } else if let Some(id) = self.ast.constant_definitions.get(s) {
            Some(id)
        } else {
            None
        }
    }

    fn get_definition_value(&self, id: &NodeId) -> Option<i32> {
        if let Some(value) = self.node_addresses.get(id) {
            Some(i32::from(*value))
        } else {
            self.constant_values.get(id).copied()
        }
    }

    fn is_addr_label(&self, ident: &Ident) -> bool {
        self.ast.label_definitions.contains_key(&ident.value)
    }

    fn add_unknown_label(&mut self, label: &str, byte_select: Option<ByteSelect>) {
        self.unknown_labels
            .entry(label.to_string())
            .or_default()
            .push(UnknownLabel::new(self.items.len(), byte_select));
    }

    fn analyse_instruction(&mut self, instr: &Instruction) -> AnalysedInstruction {
        let (mode, operand) = match &instr.operands[..] {
            // Accumulator or implied
            [] => {
                if instr.opcode.is_implied_accumulator() {
                    (AddressMode::ImpliedAccumulator, None)
                } else {
                    (AddressMode::Implied, None)
                }
            }
            // Absolute or zero-page
            [Operand::Number(n, b)] => match byte_from_byte_select(*b, n.value) {
                Some(value) => (AddressMode::ZeroPage, Some(value)),
                None => (AddressMode::Absolute, Some(n.value)),
            },
            // Absolute or relative
            [Operand::Ident(s, b)] if self.is_addr_label(s) && b.is_none() => {
                let m = if instr.opcode.is_relative() {
                    AddressMode::Relative
                } else {
                    AddressMode::Absolute
                };
                if let Some(id) = self.get_definition_id(&s.value) {
                    if let Some(addr) = self.get_definition_value(id) {
                        if m == AddressMode::Relative {
                            // Difference is w.r.t. PC after instruction is executed, hence +2
                            let mut diff = addr - i32::from(self.address + 2);
                            if !(-128..=127).contains(&diff) {
                                self.error(
                                    format!("Jump out of range (-128, +127): {diff:+}"),
                                    instr.span,
                                    None,
                                );
                                diff = 0;
                            }
                            (m, Some(diff))
                        } else {
                            (m, Some(addr))
                        }
                    } else {
                        self.add_unknown_label(&s.value, *b);
                        (m, Some(UNKNOWN_ADDR))
                    }
                } else {
                    self.error(format!("Label '{}' not found", s.value), instr.span, None);
                    (m, Some(0x0000))
                }
            }
            // Absolute or zero-page
            [Operand::Ident(s, b)] => {
                if let Some(id) = self.get_definition_id(&s.value) {
                    if let Some(value) = self.get_definition_value(id) {
                        match byte_from_byte_select(*b, value) {
                            Some(byte) => (AddressMode::ZeroPage, Some(byte)),
                            None => (AddressMode::Absolute, Some(value)),
                        }
                    } else {
                        self.add_unknown_label(&s.value, *b);
                        // TODO: this could be a zero-page address after label resolution
                        (AddressMode::Absolute, Some(UNKNOWN_ADDR))
                    }
                } else {
                    self.error(
                        format!("Variable '{}' not found", s.value),
                        instr.span,
                        None,
                    );
                    (AddressMode::Absolute, Some(0x0000))
                }
            }

            // Absolute, X/Y-indexed with label
            [Operand::Ident(s, b), Operand::Idx, Operand::Register(reg)] => {
                let mode = match reg {
                    Register::X => AddressMode::AbsoluteXIdx,
                    Register::Y => AddressMode::AbsoluteYIdx,
                    Register::A => {
                        self.error(
                            String::from("Value cannot be indexed by accumulator"),
                            instr.span,
                            None,
                        );
                        AddressMode::AbsoluteXIdx
                    }
                };
                if let Some(id) = self.get_definition_id(&s.value) {
                    if let Some(addr) = self.get_definition_value(id) {
                        (mode, Some(addr))
                    } else {
                        self.add_unknown_label(&s.value, *b);
                        (mode, Some(UNKNOWN_ADDR))
                    }
                } else {
                    self.error(format!("Label '{}' not found", s.value), instr.span, None);
                    (mode, Some(0x0000))
                }
            }
            // Immediate
            [Operand::Immediate, Operand::Number(n, b)] => {
                let value = byte_from_byte_select(*b, n.value).unwrap_or_else(|| {
                    self.error(
                        String::from("Immediate mode argument cannot exceed 1 byte"),
                        n.span,
                        None,
                    );
                    0x0000
                });
                (AddressMode::Immediate, Some(value))
            }
            // Immediate
            [Operand::Immediate, Operand::Ident(s, b)] => {
                if let Some(id) = self.get_definition_id(&s.value) {
                    if let Some(addr) = self.get_definition_value(id) {
                        let value = byte_from_byte_select(*b, addr).unwrap_or_else(|| {
                            self.error(
                                format!(
                                    "Immediate mode argument cannot exceed 1 byte, got '{}'",
                                    s.value
                                ),
                                s.span,
                                None,
                            );
                            0x0000
                        });
                        (AddressMode::Immediate, Some(value))
                    } else {
                        self.add_unknown_label(&s.value, *b);
                        (AddressMode::Immediate, Some(UNKNOWN_ADDR))
                    }
                } else {
                    self.error(format!("Label '{}' not found", s.value), instr.span, None);
                    (AddressMode::Immediate, Some(0x0000))
                }
            }
            // Indirect
            [Operand::LBracket, Operand::Number(n, None), Operand::RBracket] => {
                (AddressMode::Indirect, Some(n.value))
            }
            // X-indexed, indirect
            [Operand::LBracket, Operand::Number(n, b), Operand::Idx, Operand::Register(Register::X), Operand::RBracket] =>
            {
                let value = byte_from_byte_select(*b, n.value).unwrap_or_else(|| {
                    self.error(
                        String::from("X-indexed, indirect mode argument cannot exceed 1 byte"),
                        n.span,
                        None,
                    );
                    0x0000
                });
                (AddressMode::IndirectXIdx, Some(value))
            }
            // X-indexed, indirect
            [Operand::LBracket, Operand::Ident(s, b), Operand::Idx, Operand::Register(Register::X), Operand::RBracket] => {
                if let Some(id) = self.get_definition_id(&s.value) {
                    if let Some(addr) = self.get_definition_value(id) {
                        let value = byte_from_byte_select(*b, addr).unwrap_or_else(|| {
                            self.error(
                                format!(
                                    "X-indexed, indirect mode argument cannot exceed 1 byte, got '{}'",
                                    s.value
                                ),
                                s.span,
                                None,
                            );
                            0x0000
                        });
                        (AddressMode::IndirectXIdx, Some(value))
                    } else {
                        self.add_unknown_label(&s.value, *b);
                        (AddressMode::IndirectXIdx, Some(UNKNOWN_ADDR))
                    }
                } else {
                    self.error(format!("Label '{}' not found", s.value), instr.span, None);
                    (AddressMode::IndirectXIdx, Some(0x0000))
                }
            }
            // Y-indexed, indirect
            [Operand::LBracket, Operand::Number(n, b), Operand::RBracket, Operand::Idx, Operand::Register(Register::Y)] =>
            {
                let value = byte_from_byte_select(*b, n.value).unwrap_or_else(|| {
                    self.error(
                        String::from("Y-indexed, indirect mode argument cannot exceed 1 byte"),
                        n.span,
                        None,
                    );
                    0x0000
                });
                (AddressMode::IndirectYIdx, Some(value))
            }
            // Y-indexed, indirect
            [Operand::LBracket, Operand::Ident(s, b), Operand::RBracket, Operand::Idx, Operand::Register(Register::Y)] => {
                if let Some(id) = self.get_definition_id(&s.value) {
                    if let Some(addr) = self.get_definition_value(id) {
                        let value = byte_from_byte_select(*b, addr).unwrap_or_else(|| {
                            self.error(
                                format!(
                                    "Y-indexed, indirect mode argument cannot exceed 1 byte, got '{}'",
                                    s.value
                                ),
                                s.span,
                                None,
                            );
                            0x0000
                        });
                        (AddressMode::IndirectYIdx, Some(value))
                    } else {
                        self.add_unknown_label(&s.value, *b);
                        (AddressMode::IndirectYIdx, Some(UNKNOWN_ADDR))
                    }
                } else {
                    self.error(format!("Label '{}' not found", s.value), instr.span, None);
                    (AddressMode::IndirectYIdx, Some(0x0000))
                }
            }
            // Zeropage, X-indexed or Absolute, X-indexed
            [Operand::Number(n, b), Operand::Idx, Operand::Register(Register::X)] => {
                match byte_from_byte_select(*b, n.value) {
                    Some(value) => (AddressMode::ZeroPageXIdx, Some(value)),
                    None => (AddressMode::AbsoluteXIdx, Some(n.value)),
                }
            }
            // Zeropage, Y-indexed or Absolute, Y-indexed
            [Operand::Number(n, b), Operand::Idx, Operand::Register(Register::Y)] => {
                match byte_from_byte_select(*b, n.value) {
                    Some(value) => (AddressMode::ZeroPageYIdx, Some(value)),
                    None => (AddressMode::AbsoluteYIdx, Some(n.value)),
                }
            }
            _ => {
                self.error(
                    String::from("Invalid addressing mode"),
                    instr.span,
                    Some(instr.opcode.as_help_str()),
                );
                (AddressMode::Immediate, None)
            }
        };

        if instr.opcode.value(mode).is_none() {
            self.error(
                format!(
                    "Invalid addressing mode '{}' for instruction '{}'",
                    mode.as_ref(),
                    instr.opcode.as_ref().to_uppercase()
                ),
                instr.span,
                Some(instr.opcode.as_help_str()),
            )
        };

        let new_instr = AnalysedInstruction {
            address: self.address,
            opcode: instr.opcode,
            mode,
            operand,
            span: instr.span,
        };
        self.address = self.address.wrapping_add(mode.num_bytes());

        new_instr
    }

    fn resolve_constant(&mut self, ident: &Ident, value: &Number) {
        self.constant_values.insert(ident.id, value.value);
        let unmapped_items = self.unknown_labels.remove(&ident.value);
        if let Some(items) = unmapped_items {
            for item in items {
                match &mut self.items[item.idx] {
                    AnalysedItem::Instruction(instr) => match instr.mode {
                        AddressMode::Absolute
                        | AddressMode::AbsoluteXIdx
                        | AddressMode::AbsoluteYIdx => {
                            instr.operand =
                                Some(value_from_byte_select(item.byte_select, value.value));
                        }
                        AddressMode::Immediate
                        | AddressMode::IndirectXIdx
                        | AddressMode::IndirectYIdx
                        | AddressMode::ZeroPage
                        | AddressMode::ZeroPageXIdx
                        | AddressMode::ZeroPageYIdx => {
                            match value_from_byte_select(item.byte_select, value.value) {
                                operand if operand > u8::MAX.into() => {
                                    let span = instr.span;
                                    self.error(
                                        format!("Variable '{}' exceeds 1 byte", ident.value),
                                        span,
                                        None,
                                    );
                                }
                                other => instr.operand = Some(other),
                            }
                        }
                        other => {
                            let span = instr.span;
                            self.error(
                                format!(
                                    "Invalid addressing mode '{:#?}' for label reference '{}'",
                                    other, ident.value
                                ),
                                span,
                                None,
                            )
                        }
                    },
                    AnalysedItem::Word(word) => word.value = value.value as u16,
                    AnalysedItem::Byte(byte) => {
                        if value.value < 0xFF {
                            byte.value = value.value as u8;
                        } else {
                            self.error(String::from("Variable exceeds 1 byte"), ident.span, None);
                        }
                    }
                }
            }
        }
    }

    /// Insert (label, addr) pair into lookup and resolve
    fn analyse_label(&mut self, label: &Label) {
        self.node_addresses.insert(label.id, self.address);
        let unmapped_items = self.unknown_labels.remove(&label.label);
        if let Some(items) = unmapped_items {
            for item in items {
                match &mut self.items[item.idx] {
                    AnalysedItem::Instruction(instr) => match instr.mode {
                        AddressMode::Absolute
                        | AddressMode::AbsoluteXIdx
                        | AddressMode::AbsoluteYIdx => {
                            let address = i32::from(self.address);
                            instr.operand = match item.byte_select {
                                Some(ByteSelect::Low) => Some(address & 0x00FF),
                                Some(ByteSelect::High) => Some((address & 0xFF00) >> 8),
                                None => Some(address),
                            };
                        }
                        AddressMode::Relative => {
                            let diff = i32::from(self.address) - i32::from(instr.address + 2);
                            if diff.abs() <= 0xFF {
                                instr.operand = Some(diff);
                            } else {
                                instr.operand = Some(0x00);
                                let span = instr.span;
                                self.error(
                                    format!("Jump out of range (-128, +127): {diff:+}"),
                                    span,
                                    None,
                                );
                            }
                        }
                        AddressMode::Immediate => {
                            if let Some(b) = item.byte_select {
                                instr.operand =
                                    Some(value_from_byte_select(Some(b), self.address.into()));
                            } else {
                                let span = instr.span;
                                self.error(
                                    format!("Address '{}' exceeds 1 byte argument size for immediate addressing", label.label),
                                    span,
                                    None
                                )
                            }
                        }
                        other => {
                            let span = instr.span;
                            self.error(
                                format!(
                                    "Invalid addressing mode '{:#?}' for label reference '{}'",
                                    other, label.label
                                ),
                                span,
                                None,
                            )
                        }
                    },
                    AnalysedItem::Word(word) => {
                        word.value = self.address;
                    }
                    AnalysedItem::Byte(byte) => {
                        byte.value = 0x00;
                        self.error(
                            format!(
                                "Address '{}' for label '{}' exceeds byte definiton size",
                                self.address, label.label
                            ),
                            label.span,
                            None,
                        );
                    }
                }
            }
        }
    }
}

/// The address modes for the 6502
#[derive(AsRefStr, Copy, Clone, Debug, PartialEq, Eq)]
pub enum AddressMode {
    /// OPC A
    ImpliedAccumulator,
    /// OPC $LLHH
    Absolute,
    /// OPC $LLHH,X
    AbsoluteXIdx,
    /// OPC $LLHH,Y
    AbsoluteYIdx,
    /// OPC #$BB
    Immediate,
    /// OPC
    Implied,
    /// OPC ($LLHH)
    Indirect,
    /// OPC ($LL,X)
    IndirectXIdx,
    /// OPC ($LL),Y
    IndirectYIdx,
    /// OPC $BB
    Relative,
    /// OPC $LL
    ZeroPage,
    /// OPC $LL,X
    ZeroPageXIdx,
    /// OPC $LL,Y
    ZeroPageYIdx,
}

impl AddressMode {
    /// Generate example instruction for given opcode
    pub fn as_help_str(&self, opcode: &str) -> String {
        match self {
            Self::ImpliedAccumulator => format!("{opcode}         (accumulator)"),
            Self::Absolute => format!("{opcode} $LLHH   (absolute)"),
            Self::AbsoluteXIdx => format!("{opcode} $LLHH,X (absolute, x-indexed)"),
            Self::AbsoluteYIdx => format!("{opcode} $LLHH,Y (absolute, y-indexed)"),
            Self::Immediate => format!("{opcode} #$BB    (immediate)"),
            Self::Implied => format!("{opcode}         (implied)"),
            Self::Indirect => format!("{opcode} ($HH)   (indirect)"),
            Self::IndirectXIdx => format!("{opcode} ($LL,X)   (indirect, x-indexed)"),
            Self::IndirectYIdx => format!("{opcode} ($LL),Y   (indirect, y-indexed)"),
            Self::Relative => format!("{opcode} $BB     (relative)"),
            Self::ZeroPage => format!("{opcode} $LL     (zero-page)"),
            Self::ZeroPageXIdx => format!("{opcode} $LL,X   (zero-page, x-indexed)"),
            Self::ZeroPageYIdx => format!("{opcode} $LL,Y   (zero-page, y-indexed)"),
        }
    }

    /// Number of bytes an addressing mode requires
    pub fn num_bytes(&self) -> u16 {
        match self {
            Self::ImpliedAccumulator => 1,
            Self::Absolute => 3,
            Self::AbsoluteXIdx => 3,
            Self::AbsoluteYIdx => 3,
            Self::Immediate => 2,
            Self::Implied => 1,
            Self::Indirect => 3,
            Self::IndirectXIdx => 2,
            Self::IndirectYIdx => 2,
            Self::Relative => 2,
            Self::ZeroPage => 2,
            Self::ZeroPageXIdx => 2,
            Self::ZeroPageYIdx => 2,
        }
    }

    /// Size of operand in bytes for a given addressing mode
    pub fn operand_bytes(&self) -> u8 {
        match self {
            Self::Absolute | Self::AbsoluteXIdx | Self::AbsoluteYIdx | Self::Indirect => 2,
            Self::ImpliedAccumulator | Self::Implied => 0,
            _ => 1,
        }
    }
}

/// Stores [`AddressMode`] and corresponding opcode
pub struct ModeDetails {
    pub mode: AddressMode,
    pub opcode: u8,
}

/// Map from opcode to all available addressing modes
pub static INSTRUCTION_SET: phf::Map<&'static str, &'static [ModeDetails]> = phf_map! {
    "Adc" => &[
        ModeDetails { mode: AddressMode::Immediate, opcode: 0x69 },
        ModeDetails { mode: AddressMode::ZeroPage, opcode: 0x65 },
        ModeDetails { mode: AddressMode::ZeroPageXIdx, opcode: 0x75 },
        ModeDetails { mode: AddressMode::Absolute, opcode: 0x6D },
        ModeDetails { mode: AddressMode::AbsoluteXIdx, opcode: 0x7D },
        ModeDetails { mode: AddressMode::AbsoluteYIdx, opcode: 0x79 },
        ModeDetails { mode: AddressMode::IndirectXIdx, opcode: 0x61 },
        ModeDetails { mode: AddressMode::IndirectYIdx, opcode: 0x71 },
    ],
    "And" => &[
        ModeDetails { mode: AddressMode::Immediate, opcode: 0x29 },
        ModeDetails { mode: AddressMode::ZeroPage, opcode: 0x25 },
        ModeDetails { mode: AddressMode::ZeroPageXIdx, opcode: 0x35 },
        ModeDetails { mode: AddressMode::Absolute, opcode: 0x2D },
        ModeDetails { mode: AddressMode::AbsoluteXIdx, opcode: 0x3D },
        ModeDetails { mode: AddressMode::AbsoluteYIdx, opcode: 0x39 },
        ModeDetails { mode: AddressMode::IndirectXIdx, opcode: 0x21 },
        ModeDetails { mode: AddressMode::IndirectYIdx, opcode: 0x31 },
    ],
    "Asl" => &[
        ModeDetails { mode: AddressMode::ImpliedAccumulator, opcode: 0x0A },
        ModeDetails { mode: AddressMode::ZeroPage, opcode: 0x06 },
        ModeDetails { mode: AddressMode::ZeroPageXIdx, opcode: 0x16 },
        ModeDetails { mode: AddressMode::Absolute, opcode: 0x0E },
        ModeDetails { mode: AddressMode::AbsoluteXIdx, opcode: 0x1E },
    ],
    "Bcc" => &[ModeDetails { mode: AddressMode::Relative, opcode: 0x90 }],
    "Bcs" => &[ModeDetails { mode: AddressMode::Relative, opcode: 0xB0 }],
    "Beq" => &[ModeDetails { mode: AddressMode::Relative, opcode: 0xF0 }],
    "Bit" => &[
        ModeDetails { mode: AddressMode::ZeroPage, opcode: 0x24 },
        ModeDetails { mode: AddressMode::Absolute, opcode: 0x2C },
    ],
    "Bmi" => &[ModeDetails { mode: AddressMode::Relative, opcode: 0x30 }],
    "Bne" => &[ModeDetails { mode: AddressMode::Relative, opcode: 0xD0 }],
    "Bpl" => &[ModeDetails { mode: AddressMode::Relative, opcode: 0x10 }],
    "Brk" => &[ModeDetails { mode: AddressMode::Implied, opcode: 0x00 }],
    "Bvc" => &[ModeDetails { mode: AddressMode::Relative, opcode: 0x50 }],
    "Bvs" => &[ModeDetails { mode: AddressMode::Relative, opcode: 0x70 }],
    "Clc" => &[ModeDetails { mode: AddressMode::Implied, opcode: 0x18 }],
    "Cld" => &[ModeDetails { mode: AddressMode::Implied, opcode: 0xD8 }],
    "Cli" => &[ModeDetails { mode: AddressMode::Implied, opcode: 0x58 }],
    "Clv" => &[ModeDetails { mode: AddressMode::Implied, opcode: 0xB8 }],
    "Cmp" => &[
        ModeDetails { mode: AddressMode::Immediate, opcode: 0xC9 },
        ModeDetails { mode: AddressMode::ZeroPage, opcode: 0xC5 },
        ModeDetails { mode: AddressMode::ZeroPageXIdx, opcode: 0xD5 },
        ModeDetails { mode: AddressMode::Absolute, opcode: 0xCD },
        ModeDetails { mode: AddressMode::AbsoluteXIdx, opcode: 0xDD },
        ModeDetails { mode: AddressMode::AbsoluteYIdx, opcode: 0xD9 },
        ModeDetails { mode: AddressMode::IndirectXIdx, opcode: 0xC1 },
        ModeDetails { mode: AddressMode::IndirectYIdx, opcode: 0xD1 },
    ],
    "Cpx" => &[
        ModeDetails { mode: AddressMode::Immediate, opcode: 0xE0 },
        ModeDetails { mode: AddressMode::ZeroPage, opcode: 0xE4 },
        ModeDetails { mode: AddressMode::Absolute, opcode: 0xEC },
    ],
    "Cpy" => &[
        ModeDetails { mode: AddressMode::Immediate, opcode: 0xC0 },
        ModeDetails { mode: AddressMode::ZeroPage, opcode: 0xC4 },
        ModeDetails { mode: AddressMode::Absolute, opcode: 0xCC },
    ],
    "Dec" => &[
        ModeDetails { mode: AddressMode::ZeroPage, opcode: 0xC6 },
        ModeDetails { mode: AddressMode::ZeroPageXIdx, opcode: 0xD6 },
        ModeDetails { mode: AddressMode::Absolute, opcode: 0xCE },
        ModeDetails { mode: AddressMode::AbsoluteXIdx, opcode: 0xDE },
    ],
    "Dex" => &[ModeDetails { mode: AddressMode::Implied, opcode: 0xCA }],
    "Dey" => &[ModeDetails { mode: AddressMode::Implied, opcode: 0x88 }],
    "Eor" => &[
        ModeDetails { mode: AddressMode::Immediate, opcode: 0x49 },
        ModeDetails { mode: AddressMode::ZeroPage, opcode: 0x45 },
        ModeDetails { mode: AddressMode::ZeroPageXIdx, opcode: 0x55 },
        ModeDetails { mode: AddressMode::Absolute, opcode: 0x4D },
        ModeDetails { mode: AddressMode::AbsoluteXIdx, opcode: 0x5D },
        ModeDetails { mode: AddressMode::AbsoluteYIdx, opcode: 0x59 },
        ModeDetails { mode: AddressMode::IndirectXIdx, opcode: 0x41 },
        ModeDetails { mode: AddressMode::IndirectYIdx, opcode: 0x51 },
    ],
    "Inc" => &[
        ModeDetails { mode: AddressMode::ZeroPage, opcode: 0xE6 },
        ModeDetails { mode: AddressMode::ZeroPageXIdx, opcode: 0xF6 },
        ModeDetails { mode: AddressMode::Absolute, opcode: 0xEE },
        ModeDetails { mode: AddressMode::AbsoluteXIdx, opcode: 0xFE },
    ],
    "Inx" => &[ModeDetails { mode: AddressMode::Implied, opcode: 0xE8 }],
    "Iny" => &[ModeDetails { mode: AddressMode::Implied, opcode: 0xC8 }],
    "Jmp" => &[
        ModeDetails { mode: AddressMode::Absolute, opcode: 0x4C },
        ModeDetails { mode: AddressMode::Indirect, opcode: 0x6C },
    ],
    "Jsr" => &[ModeDetails { mode: AddressMode::Absolute, opcode: 0x20 }],
    "Lda" => &[
        ModeDetails { mode: AddressMode::Immediate, opcode: 0xA9 },
        ModeDetails { mode: AddressMode::ZeroPage, opcode: 0xA5 },
        ModeDetails { mode: AddressMode::ZeroPageXIdx, opcode: 0xB5 },
        ModeDetails { mode: AddressMode::Absolute, opcode: 0xAD },
        ModeDetails { mode: AddressMode::AbsoluteXIdx, opcode: 0xBD },
        ModeDetails { mode: AddressMode::AbsoluteYIdx, opcode: 0xB9 },
        ModeDetails { mode: AddressMode::IndirectXIdx, opcode: 0xA1 },
        ModeDetails { mode: AddressMode::IndirectYIdx, opcode: 0xB1 },
    ],
    "Ldx" => &[
        ModeDetails { mode: AddressMode::Immediate, opcode: 0xA2 },
        ModeDetails { mode: AddressMode::ZeroPage, opcode: 0xA6 },
        ModeDetails { mode: AddressMode::ZeroPageYIdx, opcode: 0xB6 },
        ModeDetails { mode: AddressMode::Absolute, opcode: 0xAE },
        ModeDetails { mode: AddressMode::AbsoluteYIdx, opcode: 0xBE },
    ],
    "Ldy" => &[
        ModeDetails { mode: AddressMode::Immediate, opcode: 0xA0 },
        ModeDetails { mode: AddressMode::ZeroPage, opcode: 0xA4 },
        ModeDetails { mode: AddressMode::ZeroPageXIdx, opcode: 0xB4 },
        ModeDetails { mode: AddressMode::Absolute, opcode: 0xAC },
        ModeDetails { mode: AddressMode::AbsoluteXIdx, opcode: 0xBC },
    ],
    "Lsr" => &[
        ModeDetails { mode: AddressMode::ImpliedAccumulator, opcode: 0x4A },
        ModeDetails { mode: AddressMode::ZeroPage, opcode: 0x46 },
        ModeDetails { mode: AddressMode::ZeroPageXIdx, opcode: 0x56 },
        ModeDetails { mode: AddressMode::Absolute, opcode: 0x4E },
        ModeDetails { mode: AddressMode::AbsoluteXIdx, opcode: 0x5E },
    ],
    "Nop" => &[ModeDetails { mode: AddressMode::Implied, opcode: 0xEA }],
    "Ora" => &[
        ModeDetails { mode: AddressMode::Immediate, opcode: 0x09 },
        ModeDetails { mode: AddressMode::ZeroPage, opcode: 0x05 },
        ModeDetails { mode: AddressMode::ZeroPageXIdx, opcode: 0x15 },
        ModeDetails { mode: AddressMode::Absolute, opcode: 0x0D },
        ModeDetails { mode: AddressMode::AbsoluteXIdx, opcode: 0x1D },
        ModeDetails { mode: AddressMode::AbsoluteYIdx, opcode: 0x19 },
        ModeDetails { mode: AddressMode::IndirectXIdx, opcode: 0x01 },
        ModeDetails { mode: AddressMode::IndirectYIdx, opcode: 0x11 },
    ],
    "Pha" => &[ModeDetails { mode: AddressMode::Implied, opcode: 0x48 }],
    "Php" => &[ModeDetails { mode: AddressMode::Implied, opcode: 0x08 }],
    "Pla" => &[ModeDetails { mode: AddressMode::Implied, opcode: 0x68 }],
    "Plp" => &[ModeDetails { mode: AddressMode::Implied, opcode: 0x28 }],
    "Rol" => &[
        ModeDetails { mode: AddressMode::ImpliedAccumulator, opcode: 0x2A },
        ModeDetails { mode: AddressMode::ZeroPage, opcode: 0x26 },
        ModeDetails { mode: AddressMode::ZeroPageXIdx, opcode: 0x36 },
        ModeDetails { mode: AddressMode::Absolute, opcode: 0x2E },
        ModeDetails { mode: AddressMode::AbsoluteXIdx, opcode: 0x3E },
    ],
    "Ror" => &[
        ModeDetails { mode: AddressMode::ImpliedAccumulator, opcode: 0x6A },
        ModeDetails { mode: AddressMode::ZeroPage, opcode: 0x66 },
        ModeDetails { mode: AddressMode::ZeroPageXIdx, opcode: 0x76 },
        ModeDetails { mode: AddressMode::Absolute, opcode: 0x6E },
        ModeDetails { mode: AddressMode::AbsoluteXIdx, opcode: 0x7E },
    ],
    "Rti" => &[ModeDetails { mode: AddressMode::Implied, opcode: 0x40 }],
    "Rts" => &[ModeDetails { mode: AddressMode::Implied, opcode: 0x60 }],
    "Sbc" => &[
        ModeDetails { mode: AddressMode::Immediate, opcode: 0xE9 },
        ModeDetails { mode: AddressMode::ZeroPage, opcode: 0xE5 },
        ModeDetails { mode: AddressMode::ZeroPageXIdx, opcode: 0xF5 },
        ModeDetails { mode: AddressMode::Absolute, opcode: 0xED },
        ModeDetails { mode: AddressMode::AbsoluteXIdx, opcode: 0xFD },
        ModeDetails { mode: AddressMode::AbsoluteYIdx, opcode: 0xF9 },
        ModeDetails { mode: AddressMode::IndirectXIdx, opcode: 0xE1 },
        ModeDetails { mode: AddressMode::IndirectYIdx, opcode: 0xF1 },
    ],
    "Sec" => &[ModeDetails { mode: AddressMode::Implied, opcode: 0x38 }],
    "Sed" => &[ModeDetails { mode: AddressMode::Implied, opcode: 0xF8 }],
    "Sei" => &[ModeDetails { mode: AddressMode::Implied, opcode: 0x78 }],
    "Sta" => &[
        ModeDetails { mode: AddressMode::ZeroPage, opcode: 0x85 },
        ModeDetails { mode: AddressMode::ZeroPageXIdx, opcode: 0x95 },
        ModeDetails { mode: AddressMode::Absolute, opcode: 0x8D },
        ModeDetails { mode: AddressMode::AbsoluteXIdx, opcode: 0x9D },
        ModeDetails { mode: AddressMode::AbsoluteYIdx, opcode: 0x99 },
        ModeDetails { mode: AddressMode::IndirectXIdx, opcode: 0x81 },
        ModeDetails { mode: AddressMode::IndirectYIdx, opcode: 0x91 },
    ],
    "Stx" => &[
        ModeDetails { mode: AddressMode::ZeroPage, opcode: 0x86 },
        ModeDetails { mode: AddressMode::ZeroPageYIdx, opcode: 0x96 },
        ModeDetails { mode: AddressMode::Absolute, opcode: 0x8E },
    ],
    "Sty" => &[
        ModeDetails { mode: AddressMode::ZeroPage, opcode: 0x84 },
        ModeDetails { mode: AddressMode::ZeroPageXIdx, opcode: 0x94 },
        ModeDetails { mode: AddressMode::Absolute, opcode: 0x8C },
    ],
    "Tax" => &[ModeDetails { mode: AddressMode::Implied, opcode: 0xAA }],
    "Tay" => &[ModeDetails { mode: AddressMode::Implied, opcode: 0xA8 }],
    "Tsx" => &[ModeDetails { mode: AddressMode::Implied, opcode: 0xBA }],
    "Txa" => &[ModeDetails { mode: AddressMode::Implied, opcode: 0x8A }],
    "Txs" => &[ModeDetails { mode: AddressMode::Implied, opcode: 0x9A }],
    "Tya" => &[ModeDetails { mode: AddressMode::Implied, opcode: 0x98 }]
};

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assemble::assemble;

    #[test]
    fn test_past_label_resolution() {
        let program = "
            Label:
                JMP Label
        ";
        let ast = assemble(program, "");
        match semantic_analysis(&ast) {
            Ok(program) => {
                assert_eq!(program.items.len(), 1);
                match program.items.first() {
                    Some(AnalysedItem::Instruction(instr)) => {
                        assert_eq!(instr.operand, Some(0x0000));
                    }
                    _ => panic!("Expected instruction"),
                }
            }
            Err(e) => panic!("Failed to analyse program: {:#?}", e),
        }
    }

    #[test]
    fn test_forward_label_resolution() {
        let program = "
                JMP Label
            Label:
                NOP
        ";
        let ast = assemble(program, "");
        match semantic_analysis(&ast) {
            Ok(program) => {
                assert_eq!(program.items.len(), 2);
                match program.items.first() {
                    Some(AnalysedItem::Instruction(instr)) => {
                        assert_eq!(instr.operand, Some(0x0003));
                    }
                    _ => panic!("Expected instruction"),
                }
            }
            Err(e) => panic!("Failed to analyse program: {:#?}", e),
        }
    }

    #[test]
    fn test_past_definitions() {
        let program = "
            .set test $02
            LDA test
        ";
        let ast = assemble(program, "");
        match semantic_analysis(&ast) {
            Ok(program) => {
                assert_eq!(program.items.len(), 1);
                match program.items.first() {
                    Some(AnalysedItem::Instruction(instr)) => {
                        assert_eq!(instr.operand, Some(0x0002));
                    }
                    _ => panic!("Expected instruction"),
                }
            }
            Err(e) => panic!("Failed to analyse program: {:#?}", e),
        }
    }

    #[test]
    fn test_forward_definitions() {
        let program = "
            LDA test
            .set test $02
        ";
        let ast = assemble(program, "");
        match semantic_analysis(&ast) {
            Ok(program) => {
                assert_eq!(program.items.len(), 1);
                match program.items.first() {
                    Some(AnalysedItem::Instruction(instr)) => {
                        assert_eq!(instr.operand, Some(0x0002));
                    }
                    _ => panic!("Expected instruction"),
                }
            }
            Err(e) => panic!("Failed to analyse program: {:#?}", e),
        }
    }

    #[test]
    fn test_relative_jumps() {
        let program = "
                BEQ Label
            Label:
                NOP
        ";
        let ast = assemble(program, "");
        match semantic_analysis(&ast) {
            Ok(program) => {
                assert_eq!(program.items.len(), 2);
                match program.items.first() {
                    Some(AnalysedItem::Instruction(instr)) => {
                        assert_eq!(instr.operand, Some(0x0000));
                    }
                    _ => panic!("Expected instruction"),
                }
            }
            Err(e) => panic!("Failed to analyse program: {:#?}", e),
        }
    }

    #[test]
    fn test_byte_index() {
        let program = "
            .org $C000
            .set var $0102

            Label:
                LDA <Label
                LDX >Label
                LDY <var
                LDA >var
        ";
        let ast = assemble(program, "");
        match semantic_analysis(&ast) {
            Ok(program) => {
                assert_eq!(program.items.len(), 4);
                match program.items.first() {
                    Some(AnalysedItem::Instruction(instr)) => {
                        assert_eq!(instr.address, 0xC000);
                        assert_eq!(instr.operand, Some(0x00));
                    }
                    _ => panic!("Expected instruction"),
                }
                match program.items.get(1) {
                    Some(AnalysedItem::Instruction(instr)) => {
                        assert_eq!(instr.address, 0xC002);
                        assert_eq!(instr.operand, Some(0xC0));
                    }
                    _ => panic!("Expected instruction"),
                }
                match program.items.get(2) {
                    Some(AnalysedItem::Instruction(instr)) => {
                        assert_eq!(instr.address, 0xC004);
                        assert_eq!(instr.operand, Some(0x02));
                    }
                    _ => panic!("Expected instruction"),
                }
                match program.items.get(3) {
                    Some(AnalysedItem::Instruction(instr)) => {
                        assert_eq!(instr.address, 0xC006);
                        assert_eq!(instr.operand, Some(0x01));
                    }
                    _ => panic!("Expected instruction"),
                }
            }
            Err(e) => panic!("Failed to analyse progam: {:#?}", e),
        }
    }

    #[test]
    fn test_db_dw_preprocessor() {
        let program = "
            .db $01, $02
            .dw $01, $0200
        ";
        let ast = assemble(program, "");
        match semantic_analysis(&ast) {
            Ok(program) => {
                assert_eq!(program.items.len(), 4);
                match program.items.first() {
                    Some(AnalysedItem::Byte(b)) => {
                        assert_eq!(b.value, 0x01);
                    }
                    _ => panic!("Expected byte"),
                }
                match program.items.get(2) {
                    Some(AnalysedItem::Word(w)) => {
                        assert_eq!(w.value, 0x01);
                    }
                    _ => panic!("Expected word"),
                }
            }
            Err(e) => panic!("Failed to analyse progam: {:#?}", e),
        }
    }
}
