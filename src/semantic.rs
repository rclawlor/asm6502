use std::collections::HashMap;

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
                    AddressingMode::ImpliedAccumulator
                } else {
                    AddressingMode::Implied
                }
            }
            // Absolute or zero-page
            [Operand::Number(n)] => {
                if n.value > 0xFF {
                    AddressingMode::Absolute
                } else {
                    AddressingMode::ZeroPage
                }
            }
            // Absolute, X-indexed
            [Operand::Number(_), Operand::Register(Register::X)] => AddressingMode::AbsoluteXIdx,
            // Absolute, Y-indexed
            [Operand::Number(_), Operand::Register(Register::Y)] => AddressingMode::AbsoluteYIdx,
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
                AddressingMode::Immediate
            }
            // Indirect
            [Operand::LeftBracket, Operand::Number(_), Operand::RightBracket] => {
                AddressingMode::Indirect
            }
            // X-indexed, indirect
            [Operand::LeftBracket, Operand::Number(n), Operand::Index, Operand::Register(Register::X), Operand::RightBracket] =>
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
                AddressingMode::IndirectXIdx
            }
            // Y-indexed, indirect
            [Operand::LeftBracket, Operand::Number(n), Operand::RightBracket, Operand::Index, Operand::Register(Register::Y)] =>
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
                AddressingMode::IndirectYIdx
            }
            // Zeropage, X-indexed
            [Operand::Number(n), Operand::Index, Operand::Register(Register::X)] => {
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
                AddressingMode::ZeroPageXIdx
            }
            // Zeropage, Y-indexed
            [Operand::Number(n), Operand::Index, Operand::Register(Register::Y)] => {
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
                AddressingMode::ZeroPageYIdx
            }
            _ => {
                self.error(String::from("Invalid addressing mode"), instr.span, None);
                AddressingMode::Immediate
            }
        };
    }
}

enum AddressingMode {
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
