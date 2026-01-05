use std::collections::HashMap;

use phf::phf_map;
use strum::AsRefStr;

use crate::{ast::*, error::CompileError};

const UNKNOWN_ADDR: i32 = -0x0001;

pub fn semantic_analysis(ast: &Program) -> Result<AnalysedProgram, Vec<CompileError>> {
    let mut resolver = SymbolResolver::new(ast.clone());
    let new_ast = resolver.resolve();
    if !resolver.errors.is_empty() {
        return Err(resolver.errors);
    }

    let mut analyser = SemanticAnalyser::new(new_ast.clone());
    let program = analyser.analyse();
    if analyser.errors.is_empty() {
        Ok(program)
    } else {
        Err(analyser.errors)
    }
}

struct SymbolResolver {
    ast: Program,
    symbol_table: HashMap<String, DirectiveItem>,
    items: Vec<ProgramItem>,
    errors: Vec<CompileError>,
}

impl SymbolResolver {
    fn new(ast: Program) -> Self {
        SymbolResolver {
            ast,
            symbol_table: HashMap::new(),
            items: Vec::new(),
            errors: Vec::new(),
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

    fn resolve(&mut self) -> Program {
        for item in &self.ast.items.clone() {
            match item {
                ProgramItem::Preprocessor(pp) => self.resolve_preprocessor(pp),
                ProgramItem::Instruction(instr) => self.resolve_instruction(instr),
                _ => self.items.push(item.clone()),
            }
        }
        let mut new_ast = self.ast.clone();
        new_ast.items = self.items.clone();
        new_ast
    }

    fn resolve_preprocessor(&mut self, pp: &Preprocessor) {
        match pp.directive {
            Directive::Incbin => {
                let bin = self.resolve_binary(pp);
                self.items.push(ProgramItem::Binary(bin));
            }
            Directive::Set => {
                if pp.args.len() == 2 {
                    match &pp.args[0] {
                        DirectiveItem::Ident(ident) => {
                            self.symbol_table
                                .insert(ident.value.clone(), pp.args[1].clone());
                        }
                        _ => self.error(
                            String::from("'.set' requires a variable name"),
                            pp.span,
                            None,
                        ),
                    }
                } else {
                    self.error(
                        format!("'.set' requires 2 arguments, got {}", pp.args.len()),
                        pp.span,
                        None,
                    );
                }
            }
            _ => self.items.push(ProgramItem::Preprocessor(pp.clone())),
        }
    }

    fn resolve_binary(&mut self, pp: &Preprocessor) -> Binary {
        let (filename, span, bytes) = if let Some(DirectiveItem::String(s)) = pp.args.first() {
            let b = match std::fs::read(&s.value) {
                Ok(b) => b,
                Err(e) => {
                    println!("{e}");
                    self.error(format!("Unable to read file '{}'", s.value), s.span, None);
                    Vec::new()
                }
            };
            (s.value.clone(), s.span, b)
        } else {
            (String::new(), pp.span, Vec::new())
        };

        Binary {
            id: next_node_id(),
            span,
            filename,
            bytes,
        }
    }

    fn resolve_instruction(&mut self, instr: &Instruction) {
        let mut new_instr = instr.clone();
        new_instr.operands.clear();
        for item in &instr.operands {
            if let Operand::Ident(ident) = item {
                if let Some(v) = self.symbol_table.get(&ident.value) {
                    match v {
                        DirectiveItem::Number(n) => {
                            new_instr.operands.push(Operand::Number(n.clone()));
                        }
                        DirectiveItem::String(s) => {
                            self.error(
                                format!("Expected number, got string {}", s.value),
                                ident.span,
                                None,
                            );
                        }
                        DirectiveItem::Ident(i) => {
                            self.error(
                                format!("Expected number, got identity '{}'", i.value),
                                ident.span,
                                None,
                            );
                        }
                    }
                } else if self.ast.labels.contains(&ident.value) {
                    new_instr
                        .operands
                        .push(Operand::AddrLabel(ident.value.clone()));
                } else {
                    self.error(
                        format!("Could not find definition for '{}'", ident.value),
                        ident.span,
                        None,
                    );
                }
            } else {
                new_instr.operands.push(item.clone());
            }
        }
        self.items.push(ProgramItem::Instruction(new_instr));
    }
}

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

#[derive(Clone, Copy, Debug)]
pub struct AnalysedInstruction {
    address: u16,
    pub opcode: Opcode,
    pub mode: AddressMode,
    pub operand: Option<i32>,
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

struct SemanticAnalyser {
    ast: Program,
    /// Current address
    address: u16,
    /// Post-analysis items
    items: Vec<AnalysedItem>,
    /// Labels and corresponding addresses
    labels: HashMap<String, i32>,
    /// Labels not yet found
    unknown_labels: HashMap<String, Vec<usize>>,
    errors: Vec<CompileError>,
}

impl SemanticAnalyser {
    fn new(ast: Program) -> Self {
        SemanticAnalyser {
            ast,
            address: 0x0000,
            items: Vec::new(),
            labels: HashMap::new(),
            unknown_labels: HashMap::new(),
            errors: Vec::new(),
        }
    }

    fn analyse(&mut self) -> AnalysedProgram {
        let mut header = INesHeader::new();
        for item in self.ast.items.clone() {
            match item {
                ProgramItem::Instruction(instr) => {
                    let i = self.analyse_instruction(&instr);
                    self.items.push(AnalysedItem::Instruction(i));
                }
                ProgramItem::Preprocessor(pp) => match pp.directive {
                    Directive::Inesprg => {
                        header.prg_size_16kb = self.get_preprocessor_num(&pp) as u8;
                    }
                    Directive::Ineschr => {
                        header.chr_size_16kb = self.get_preprocessor_num(&pp) as u8;
                    }
                    Directive::Inesmap => header.mapper = self.get_preprocessor_num(&pp) as u8,
                    Directive::Inesmir => header.mirror = self.get_preprocessor_num(&pp) as u8,
                    Directive::Org => match pp.args.first() {
                        Some(DirectiveItem::Number(n)) => {
                            self.address = u16::try_from(n.value).unwrap();
                        }
                        _ => self.error(String::from("Incorrect/missing argument"), pp.span, None),
                    },
                    Directive::Db => {
                        let w = self.analyse_byte(&pp);
                        self.items.push(AnalysedItem::Byte(w));
                    }
                    Directive::Dw => {
                        let w = self.analyse_word(&pp);
                        self.items.push(AnalysedItem::Word(w));
                    }
                    Directive::Pad => {
                        self.analyse_pad(&pp);
                    }
                    _ => self.error(
                        format!(
                            "Preprocessor {:#?} should be resolved before semantic analysis",
                            pp.directive,
                        ),
                        pp.span,
                        None,
                    ),
                },
                ProgramItem::Label(label) => self.analyse_label(&label),
                ProgramItem::Binary(binary) => {
                    for b in binary.bytes {
                        self.items.push(AnalysedItem::Byte(AnalysedByte {
                            address: self.address,
                            value: b,
                        }));
                        self.address = self.address.saturating_add(1);
                    }
                }
            }
        }
        AnalysedProgram {
            items: self.items.clone(),
            header,
        }
    }

    fn get_preprocessor_num(&mut self, pp: &Preprocessor) -> i32 {
        if let Some(DirectiveItem::Number(n)) = pp.args.first() {
            n.value
        } else {
            self.error(
                String::from("Preprocessor requires number argument"),
                pp.span,
                None,
            );
            1
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

    fn analyse_word(&mut self, pp: &Preprocessor) -> AnalysedWord {
        let value = match pp.args.first() {
            Some(DirectiveItem::Number(n)) => n.value,
            Some(DirectiveItem::Ident(i)) => {
                if self.ast.labels.contains(&i.value) {
                    if let Some(addr) = self.labels.get(&i.value) {
                        *addr
                    } else {
                        self.unknown_labels
                            .entry(i.value.clone())
                            .or_default()
                            .push(self.items.len());
                        UNKNOWN_ADDR
                    }
                } else {
                    self.error(format!("Label '{}' not found", i.value), i.span, None);
                    UNKNOWN_ADDR
                }
            }
            _ => {
                self.error(
                    String::from("Expected number or label for .dw"),
                    pp.span,
                    None,
                );
                UNKNOWN_ADDR
            }
        };
        let word = AnalysedWord {
            address: self.address,
            value: value as u16,
        };
        self.address = self.address.wrapping_add(2);
        word
    }

    fn analyse_byte(&mut self, pp: &Preprocessor) -> AnalysedByte {
        let value = match pp.args.first() {
            Some(DirectiveItem::Number(n)) => n.value,
            Some(DirectiveItem::Ident(i)) => {
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
            _ => {
                self.error(String::from("Expected number for .dw"), pp.span, None);
                UNKNOWN_ADDR
            }
        };
        let byte = AnalysedByte {
            address: self.address,
            value: value as u8,
        };
        self.address = self.address.wrapping_add(1);
        byte
    }

    fn analyse_pad(&mut self, pp: &Preprocessor) {
        let target_addr = if let Some(DirectiveItem::Number(n)) = pp.args.first() {
            n.value as u16
        } else {
            self.error(
                String::from("Expected target address for .pad preprocessor"),
                pp.span,
                None,
            );
            0
        };
        while self.address < target_addr {
            self.items.push(AnalysedItem::Byte(AnalysedByte {
                address: self.address,
                value: 0x00,
            }));
            self.address = self.address.saturating_add(1);
        }
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
            [Operand::Number(n)] => {
                if n.value > 0xFF {
                    (AddressMode::Absolute, Some(n.value))
                } else {
                    (AddressMode::ZeroPage, Some(n.value))
                }
            }
            // Absolute or relative
            [Operand::AddrLabel(s)] => {
                let m = if instr.opcode.is_relative() {
                    AddressMode::Relative
                } else {
                    AddressMode::Absolute
                };
                if self.ast.labels.contains(s) {
                    if let Some(addr) = self.labels.get(s) {
                        if m == AddressMode::Relative {
                            let mut diff = *addr - i32::from(self.address);
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
                            (m, Some(*addr))
                        }
                    } else {
                        self.unknown_labels
                            .entry(s.clone())
                            .or_default()
                            .push(self.items.len());
                        (m, Some(UNKNOWN_ADDR))
                    }
                } else {
                    self.error(format!("Label '{s}' not found"), instr.span, None);
                    (m, Some(0x0000))
                }
            }
            // Absolute, X-indexed with label
            [Operand::AddrLabel(s), Operand::Idx, Operand::Register(Register::X)] => {
                if self.ast.labels.contains(s) {
                    if let Some(addr) = self.labels.get(s) {
                        (AddressMode::AbsoluteXIdx, Some(*addr))
                    } else {
                        self.unknown_labels
                            .entry(s.clone())
                            .or_default()
                            .push(self.items.len());
                        (AddressMode::AbsoluteXIdx, Some(UNKNOWN_ADDR))
                    }
                } else {
                    self.error(format!("Label '{s}' not found"), instr.span, None);
                    (AddressMode::AbsoluteXIdx, Some(0x0000))
                }
            }
            // Absolute, Y-indexed with label
            [Operand::AddrLabel(s), Operand::Idx, Operand::Register(Register::Y)] => {
                if self.ast.labels.contains(s) {
                    if let Some(addr) = self.labels.get(s) {
                        (AddressMode::AbsoluteYIdx, Some(*addr))
                    } else {
                        self.unknown_labels
                            .entry(s.clone())
                            .or_default()
                            .push(self.items.len());
                        (AddressMode::AbsoluteYIdx, Some(UNKNOWN_ADDR))
                    }
                } else {
                    self.error(format!("Label '{s}' not found"), instr.span, None);
                    (AddressMode::AbsoluteYIdx, Some(0x0000))
                }
            }
            // Immediate
            [Operand::Immediate, Operand::Number(n)] => {
                if n.value > 0xFF {
                    self.error(
                        format!(
                            "Immediate mode argument cannot exceed 1 byte, got '{}'",
                            n.value
                        ),
                        n.span,
                        None,
                    );
                }
                (AddressMode::Immediate, Some(n.value))
            }
            // Indirect
            [Operand::LBracket, Operand::Number(n), Operand::RBracket] => {
                (AddressMode::Indirect, Some(n.value))
            }
            // X-indexed, indirect
            [Operand::LBracket, Operand::Number(n), Operand::Idx, Operand::Register(Register::X), Operand::RBracket] =>
            {
                if n.value > 0xFF {
                    self.error(
                        format!(
                            "X-indexed, indirect mode argument cannot exceed 1 byte, got '{}'",
                            n.value
                        ),
                        n.span,
                        None,
                    );
                }
                (AddressMode::IndirectXIdx, Some(n.value))
            }
            // Y-indexed, indirect
            [Operand::LBracket, Operand::Number(n), Operand::RBracket, Operand::Idx, Operand::Register(Register::Y)] =>
            {
                if n.value > 0xFF {
                    self.error(
                        format!(
                            "Y-indexed, indirect mode argument cannot exceed 1 byte, got '{}'",
                            n.value
                        ),
                        n.span,
                        None,
                    );
                }
                (AddressMode::IndirectYIdx, Some(n.value))
            }
            // Zeropage, X-indexed or Absolute, X-indexed
            [Operand::Number(n), Operand::Idx, Operand::Register(Register::X)] => {
                if n.value > 0xFF {
                    (AddressMode::AbsoluteXIdx, Some(n.value))
                } else {
                    (AddressMode::ZeroPageXIdx, Some(n.value))
                }
            }
            // Zeropage, Y-indexed or Absolute, Y-indexed
            [Operand::Number(n), Operand::Idx, Operand::Register(Register::Y)] => {
                if n.value > 0xFF {
                    (AddressMode::AbsoluteYIdx, Some(n.value))
                } else {
                    (AddressMode::ZeroPageYIdx, Some(n.value))
                }
            }
            other => {
                println!("{:#?}", other);
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
        };
        self.address = self.address.wrapping_add(mode.num_bytes());

        new_instr
    }

    /// Insert (label, addr) pair into lookup and resolve
    fn analyse_label(&mut self, label: &Label) {
        self.labels.insert(label.label.clone(), self.address.into());
        let unmapped_items = self.unknown_labels.remove(&label.label);
        if let Some(items) = unmapped_items {
            for item in items {
                match &mut self.items[item] {
                    AnalysedItem::Instruction(instr) => match instr.mode {
                        AddressMode::Absolute
                        | AddressMode::AbsoluteXIdx
                        | AddressMode::AbsoluteYIdx => {
                            instr.operand = Some(self.address.into());
                        }
                        AddressMode::Relative => {
                            let diff = i32::from(instr.address) - i32::from(self.address);
                            if diff.abs() <= 0xFF {
                                instr.operand = Some(diff);
                            } else {
                                instr.operand = Some(0x00);
                                self.error(
                                    format!("Jump out of range (-128, +127): {diff:+}"),
                                    label.span,
                                    None,
                                );
                            }
                        }
                        other => self.error(
                            format!(
                                "Invalid addressing mode '{:#?}' for label reference '{}'",
                                other, label.label
                            ),
                            label.span,
                            None,
                        ),
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
    pub fn as_help_str(&self, opcode: &str) -> String {
        match self {
            Self::ImpliedAccumulator => format!("{opcode}         (accumulator)"),
            Self::Absolute => format!("{opcode} $LLHH   (absolute)"),
            Self::AbsoluteXIdx => format!("{opcode} $LLHH,X (absolute, x-indexed)"),
            Self::AbsoluteYIdx => format!("{opcode} $LLHH,Y (absolute, y-indexed)"),
            Self::Immediate => format!("{opcode} #$BB    (immediate)"),
            Self::Implied => format!("{opcode}         (implied)"),
            Self::Indirect => format!("{opcode} ($HH)   (indirect)"),
            Self::IndirectXIdx => format!("{opcode} ($,X)   (indirect, x-indexed)"),
            Self::IndirectYIdx => format!("{opcode} ($),Y   (indirect, y-indexed)"),
            Self::Relative => format!("{opcode} $BB     (relative)"),
            Self::ZeroPage => format!("{opcode} $LL     (zero-page)"),
            Self::ZeroPageXIdx => format!("{opcode} $LL,X   (zero-page, x-indexed)"),
            Self::ZeroPageYIdx => format!("{opcode} $LL,Y   (zero-page, y-indexed)"),
        }
    }

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

    pub fn operand_bytes(&self) -> u8 {
        match self {
            Self::Absolute | Self::AbsoluteXIdx | Self::AbsoluteYIdx | Self::Indirect => 2,
            Self::ImpliedAccumulator | Self::Implied => 0,
            _ => 1,
        }
    }
}

pub struct ModeDetails {
    pub mode: AddressMode,
    pub opcode: u8,
}

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
    "Nop" => &[ModeDetails { mode: AddressMode::Absolute, opcode: 0xEA }],
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
