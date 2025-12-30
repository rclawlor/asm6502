use std::collections::HashMap;

use crate::{ast::*, error::CompileError};

pub fn semantic_analysis(ast: &Program) -> Result<(), Vec<CompileError>> {
    let mut resolver = SymbolResolver::new();
    resolver.resolve(ast);
    if !resolver.errors.is_empty() {
        return Err(resolver.errors);
    }

    let analyser = SemanticAnalyser::new();
    analyser.analyse(ast);
    Ok(())
}


struct SymbolResolver {
    symbol_table: HashMap<String, DirectiveItem>,
    errors: Vec<CompileError>,
}

impl SymbolResolver {
    fn new() -> Self {
        SymbolResolver {
            symbol_table: HashMap::new(),
            errors: Vec::new(),
        }
    }

    /// Append new error message
    fn error(&mut self, message: String, span: Span) {
        self.errors.push(CompileError { message, span });
    }

    fn resolve(&mut self, ast: &Program) {
        for item in &ast.items {
            println!("{:#?}", item);
            match item {
                ProgramItem::Preprocessor(pp) => self.resolve_preprocessor(pp),
                _ => ()
            }
        }
    }

    fn resolve_preprocessor(&mut self, pp: &Preprocessor) {
        match pp.directive {
            Directive::Set => {
                if pp.args.len() == 2 {
                    match &pp.args[0] {
                        DirectiveItem::Ident(ident) => {
                            self.symbol_table.insert(ident.value.clone(), pp.args[1].clone());
                        },
                        _ => self.error(format!("'.set' requires a variable name"), pp.span),
                    }
                } else {
                    self.error(
                        format!("'.set' requires 2 arguments, got {}", pp.args.len()),
                        pp.span
                    );
                }
            },
            _ => panic!("Not implemented")
        }
    }
}


struct SemanticAnalyser {}


impl SemanticAnalyser {
    fn new() -> Self {
        SemanticAnalyser {}
    }

    fn analyse(&self, ast: &Program) {
        for item in &ast.items {
            match item {
                ProgramItem::Instruction(instr) => {
                    self.analyse_instruction(instr)
                },
                _ => ()
            }
        }
    }

    fn analyse_instruction(&self, instr: &Instruction) {
        let addr_mode = match instr.operands[..] {
            _ => ()
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
