use std::collections::HashMap;

use phf::phf_map;

use crate::{ast::*, error::CompileError};

pub fn semantic_analysis(ast: &Program) -> Result<(), Vec<CompileError>> {
    let mut resolver = SymbolResolver::new();
    let new_ast = resolver.resolve(ast);
    if !resolver.errors.is_empty() {
        return Err(resolver.errors);
    }

    let mut analyser = SemanticAnalyser::new();
    analyser.analyse(&new_ast);
    if analyser.errors.is_empty() {
        Ok(())
    } else {
        Err(analyser.errors)
    }
}

struct SymbolResolver {
    symbol_table: HashMap<String, DirectiveItem>,
    items: Vec<ProgramItem>,
    errors: Vec<CompileError>,
}

impl SymbolResolver {
    fn new() -> Self {
        SymbolResolver {
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

    fn resolve(&mut self, ast: &Program) -> Program {
        for item in &ast.items {
            match item {
                ProgramItem::Preprocessor(pp) => self.resolve_preprocessor(pp),
                ProgramItem::Instruction(instr) => self.resolve_instruction(instr),
                ProgramItem::Number(n) => {
                    self.error(String::from("Unexpected number definition"), n.span, None)
                }
                ProgramItem::Ident(ident) => self.error(
                    String::from("Unexpected identity definition"),
                    ident.span,
                    None,
                ),
            }
        }
        let mut new_ast = ast.clone();
        new_ast.items = self.items.clone();
        new_ast
    }

    fn resolve_preprocessor(&mut self, pp: &Preprocessor) {
        match pp.directive {
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
            _ => panic!("Not implemented"),
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

struct SemanticAnalyser {
    errors: Vec<CompileError>,
}

impl SemanticAnalyser {
    fn new() -> Self {
        SemanticAnalyser { errors: Vec::new() }
    }

    fn analyse(&mut self, ast: &Program) {
        for item in &ast.items {
            match item {
                ProgramItem::Instruction(instr) => self.analyse_instruction(instr),
                _ => (),
            }
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

    fn analyse_instruction(&mut self, instr: &Instruction) {
        let addr_mode = match &instr.operands[..] {
            // Accumulator or implied
            [] => {
                if instr.opcode.is_implied_accumulator() {
                    AddressMode::ImpliedAccumulator
                } else {
                    AddressMode::Implied
                }
            }
            // Absolute or zero-page
            [Operand::Number(n)] => {
                if n.value > 0xFF {
                    AddressMode::Absolute
                } else {
                    AddressMode::ZeroPage
                }
            }
            // Absolute, X-indexed
            [Operand::Number(_), Operand::Register(Register::X)] => AddressMode::AbsoluteXIdx,
            // Absolute, Y-indexed
            [Operand::Number(_), Operand::Register(Register::Y)] => AddressMode::AbsoluteYIdx,
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
                    )
                }
                AddressMode::Immediate
            }
            // Indirect
            [Operand::LBracket, Operand::Number(_), Operand::RBracket] => AddressMode::Indirect,
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
                    )
                }
                AddressMode::IndirectXIdx
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
                    )
                }
                AddressMode::IndirectYIdx
            }
            // Zeropage, X-indexed
            [Operand::Number(n), Operand::Idx, Operand::Register(Register::X)] => {
                if n.value > 0xFF {
                    self.error(
                        format!(
                            "Zeropage, X-indexed mode argument cannot exceed 1 byte, got '{}'",
                            n.value
                        ),
                        n.span,
                        None,
                    )
                }
                AddressMode::ZeroPageXIdx
            }
            // Zeropage, Y-indexed
            [Operand::Number(n), Operand::Idx, Operand::Register(Register::Y)] => {
                if n.value > 0xFF {
                    self.error(
                        format!(
                            "Zeropage, X-indexed mode argument cannot exceed 1 byte, got '{}'",
                            n.value
                        ),
                        n.span,
                        None,
                    )
                }
                AddressMode::ZeroPageYIdx
            }
            _ => {
                self.error(
                    String::from("Invalid addressing mode"),
                    instr.span,
                    Some(instr.opcode.as_help_str()),
                );
                AddressMode::Immediate
            }
        };
    }
}

#[derive(Copy, Clone)]
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
}

pub static INSTRUCTION_SET: phf::Map<&'static str, &'static [(AddressMode, u8)]> = phf_map! {
    "Adc" => &[
        (AddressMode::Immediate,    0x69),
        (AddressMode::ZeroPage,     0x65),
        (AddressMode::ZeroPageXIdx, 0x75),
        (AddressMode::Absolute,     0x6D),
        (AddressMode::AbsoluteXIdx, 0x7D),
        (AddressMode::AbsoluteYIdx, 0x79),
        (AddressMode::IndirectXIdx, 0x61),
        (AddressMode::IndirectYIdx, 0x71),
    ],
    "And" => &[
        (AddressMode::Immediate,    0x29),
        (AddressMode::ZeroPage,     0x25),
        (AddressMode::ZeroPageXIdx, 0x35),
        (AddressMode::Absolute,     0x2D),
        (AddressMode::AbsoluteXIdx, 0x3D),
        (AddressMode::AbsoluteYIdx, 0x39),
        (AddressMode::IndirectXIdx, 0x21),
        (AddressMode::IndirectYIdx, 0x31),
    ],
    "Asl" => &[
        (AddressMode::ImpliedAccumulator, 0x0A),
        (AddressMode::ZeroPage,           0x06),
        (AddressMode::ZeroPageXIdx,       0x16),
        (AddressMode::Absolute,           0x0E),
        (AddressMode::AbsoluteXIdx,       0x1E),
    ],
    "Bcc" => &[(AddressMode::Relative, 0x90)],
    "Bcs" => &[(AddressMode::Relative, 0xB0)],
    "Beq" => &[(AddressMode::Relative, 0xF0)],
    "Bit" => &[
        (AddressMode::ZeroPage, 0x24),
        (AddressMode::Absolute, 0x2C),
    ],
    "Bmi" => &[(AddressMode::Relative, 0x30)],
    "Bne" => &[(AddressMode::Relative, 0xD0)],
    "Bpl" => &[(AddressMode::Relative, 0x10)],
    "Brk" => &[(AddressMode::Implied,  0x00)],
    "Bvc" => &[(AddressMode::Relative, 0x50)],
    "Bvs" => &[(AddressMode::Relative, 0x70)],
    "Clc" => &[(AddressMode::Implied,  0x18)],
    "Cld" => &[(AddressMode::Implied,  0xD8)],
    "Cli" => &[(AddressMode::Implied,  0x58)],
    "Clv" => &[(AddressMode::Implied,  0xB8)],
    "Cmp" => &[
        (AddressMode::Immediate,    0xC9),
        (AddressMode::ZeroPage,     0xC5),
        (AddressMode::ZeroPageXIdx, 0xD5),
        (AddressMode::Absolute,     0xCD),
        (AddressMode::AbsoluteXIdx, 0xDD),
        (AddressMode::AbsoluteYIdx, 0xD9),
        (AddressMode::IndirectXIdx, 0xC1),
        (AddressMode::IndirectYIdx, 0xD1),
    ],
    "Cpx" => &[
        (AddressMode::Immediate, 0xE0),
        (AddressMode::ZeroPage,  0xE4),
        (AddressMode::Absolute,  0xEC),
    ],
    "Cpy" => &[
        (AddressMode::Immediate, 0xC0),
        (AddressMode::ZeroPage,  0xC4),
        (AddressMode::Absolute,  0xCC),
    ],
    "Dec" => &[
        (AddressMode::ZeroPage,     0xC6),
        (AddressMode::ZeroPageXIdx, 0xD6),
        (AddressMode::Absolute,     0xCE),
        (AddressMode::AbsoluteXIdx, 0xDE),
    ],
    "Dex" => &[(AddressMode::Implied, 0xCA)],
    "Dey" => &[(AddressMode::Implied, 0x88)],
    "Eor" => &[
        (AddressMode::Immediate,    0x49),
        (AddressMode::ZeroPage,     0x45),
        (AddressMode::ZeroPageXIdx, 0x55),
        (AddressMode::Absolute,     0x4D),
        (AddressMode::AbsoluteXIdx, 0x5D),
        (AddressMode::AbsoluteYIdx, 0x59),
        (AddressMode::IndirectXIdx, 0x41),
        (AddressMode::IndirectYIdx, 0x51),
    ],
    "Inc" => &[
        (AddressMode::ZeroPage,     0xE6),
        (AddressMode::ZeroPageXIdx, 0xF6),
        (AddressMode::Absolute,     0xEE),
        (AddressMode::AbsoluteXIdx, 0xFE),
    ],
    "Inx" => &[(AddressMode::Implied, 0xE8)],
    "Iny" => &[(AddressMode::Implied, 0xC8)],
    "Jmp" => &[
        (AddressMode::Absolute, 0x4C),
        (AddressMode::Indirect, 0x6C),
    ],
    "Jsr" => &[(AddressMode::Absolute, 0x20)],
    "Lda" => &[
        (AddressMode::Immediate,    0xA9),
        (AddressMode::ZeroPage,     0xA5),
        (AddressMode::ZeroPageXIdx, 0xB5),
        (AddressMode::Absolute,     0xAD),
        (AddressMode::AbsoluteXIdx, 0xBD),
        (AddressMode::AbsoluteYIdx, 0xB9),
        (AddressMode::IndirectXIdx, 0xA1),
        (AddressMode::IndirectYIdx, 0xB1),
    ],
    "Ldx" => &[
        (AddressMode::Immediate,    0xA2),
        (AddressMode::ZeroPage,     0xA6),
        (AddressMode::ZeroPageYIdx, 0xB6),
        (AddressMode::Absolute,     0xAE),
        (AddressMode::AbsoluteYIdx, 0xBE),
    ],
    "Ldy" => &[
        (AddressMode::Immediate,    0xA0),
        (AddressMode::ZeroPage,     0xA4),
        (AddressMode::ZeroPageXIdx, 0xB4),
        (AddressMode::Absolute,     0xAC),
        (AddressMode::AbsoluteXIdx, 0xBC),
    ],
    "Lsr" => &[
        (AddressMode::ImpliedAccumulator, 0x4A),
        (AddressMode::ZeroPage,           0x46),
        (AddressMode::ZeroPageXIdx,       0x56),
        (AddressMode::Absolute,           0x4E),
        (AddressMode::AbsoluteXIdx,       0x5E),
    ],
    "Nop" => &[(AddressMode::Absolute, 0xEA)],
    "Ora" => &[
        (AddressMode::Immediate,    0x09),
        (AddressMode::ZeroPage,     0x05),
        (AddressMode::ZeroPageXIdx, 0x15),
        (AddressMode::Absolute,     0x0D),
        (AddressMode::AbsoluteXIdx, 0x1D),
        (AddressMode::AbsoluteYIdx, 0x19),
        (AddressMode::IndirectXIdx, 0x01),
        (AddressMode::IndirectYIdx, 0x11),
    ],
    "Pha" => &[(AddressMode::Implied, 0x48)],
    "Php" => &[(AddressMode::Implied, 0x08)],
    "Pla" => &[(AddressMode::Implied, 0x68)],
    "Plp" => &[(AddressMode::Implied, 0x28)],
    "Rol" => &[
        (AddressMode::ImpliedAccumulator, 0x2A),
        (AddressMode::ZeroPage,           0x26),
        (AddressMode::ZeroPageXIdx,       0x36),
        (AddressMode::Absolute,           0x2E),
        (AddressMode::AbsoluteXIdx,       0x3E),
    ],
    "Ror" => &[
        (AddressMode::ImpliedAccumulator, 0x6A),
        (AddressMode::ZeroPage,           0x66),
        (AddressMode::ZeroPageXIdx,       0x76),
        (AddressMode::Absolute,           0x6E),
        (AddressMode::AbsoluteXIdx,       0x7E),
    ],
    "Rti" => &[(AddressMode::Implied, 0x40)],
    "Rts" => &[(AddressMode::Implied, 0x60)],
    "Sbc" => &[
        (AddressMode::Immediate,    0xE9),
        (AddressMode::ZeroPage,     0xE5),
        (AddressMode::ZeroPageXIdx, 0xF5),
        (AddressMode::Absolute,     0xED),
        (AddressMode::AbsoluteXIdx, 0xFD),
        (AddressMode::AbsoluteYIdx, 0xF9),
        (AddressMode::IndirectXIdx, 0xE1),
        (AddressMode::IndirectYIdx, 0xF1),
    ],
    "Sec" => &[(AddressMode::Implied, 0x38)],
    "Sed" => &[(AddressMode::Implied, 0xF8)],
    "Sei" => &[(AddressMode::Implied, 0x78)],
    "Sta" => &[
        (AddressMode::ZeroPage,     0x85),
        (AddressMode::ZeroPageXIdx, 0x95),
        (AddressMode::Absolute,     0x8D),
        (AddressMode::AbsoluteXIdx, 0x9D),
        (AddressMode::AbsoluteYIdx, 0x99),
        (AddressMode::IndirectXIdx, 0x81),
        (AddressMode::IndirectYIdx, 0x91),
    ],
    "Stx" => &[
        (AddressMode::ZeroPage,     0x86),
        (AddressMode::ZeroPageYIdx, 0x96),
        (AddressMode::Absolute,     0x8E),
    ],
    "Sty" => &[
        (AddressMode::ZeroPage,     0x84),
        (AddressMode::ZeroPageXIdx, 0x94),
        (AddressMode::Absolute,     0x8C),
    ],
    "Tax" => &[(AddressMode::Implied, 0xAA)],
    "Tay" => &[(AddressMode::Implied, 0xA8)],
    "Tsx" => &[(AddressMode::Implied, 0xBA)],
    "Txa" => &[(AddressMode::Implied, 0x8A)],
    "Txs" => &[(AddressMode::Implied, 0x9A)],
    "Tya" => &[(AddressMode::Implied, 0x98)]
};
