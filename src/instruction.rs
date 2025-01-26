// Standard library
use std::str::FromStr;

// Local
use crate::error::{OpCodeError, PreprocessorError, RegisterError};


/// The 6502 registers
#[derive(Clone, Debug, PartialEq)]
pub enum Register {
    A,
    X,
    Y
}

impl FromStr for Register {
    type Err = RegisterError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "A" => Ok(Self::A),
            "X" => Ok(Self::X),
            "Y" => Ok(Self::Y),
            _ => Err(
                RegisterError::InvalidRegister(
                    format!("Invalid register '{}'", s)
                )
            )
        }
    }
}


/// The 6502 addressing modes
#[derive(Debug)]
pub enum AddressingMode {
    Immediate,
    ZeroPage,
    ZeroPageX,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    IndirectX,
    IndirectY
}


/// The 6502 Op Codes
#[derive(Clone, Debug, PartialEq)]
pub enum OpCode {
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
}


impl FromStr for OpCode {

    type Err = OpCodeError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let opcode = s.to_uppercase();
        match opcode.as_str() {
            "ADC" => Ok(Self::ADC),
            "AND" => Ok(Self::AND),
            "ASL" => Ok(Self::ASL),
            "BCC" => Ok(Self::BCC),
            "BCS" => Ok(Self::BCS),
            "BEQ" => Ok(Self::BEQ),
            "BIT" => Ok(Self::BIT),
            "BMI" => Ok(Self::BMI),
            "BNE" => Ok(Self::BNE),
            "BPL" => Ok(Self::BPL),
            "BRK" => Ok(Self::BRK),
            "BVC" => Ok(Self::BVC),
            "BVS" => Ok(Self::BVS),
            "CLC" => Ok(Self::CLC),
            "CLD" => Ok(Self::CLD),
            "CLI" => Ok(Self::CLI),
            "CLV" => Ok(Self::CLV),
            "CMP" => Ok(Self::CMP),
            "CPX" => Ok(Self::CPX),
            "CPY" => Ok(Self::CPY),
            "DEC" => Ok(Self::DEC),
            "DEX" => Ok(Self::DEX),
            "DEY" => Ok(Self::DEY),
            "EOR" => Ok(Self::EOR),
            "INC" => Ok(Self::INC),
            "INX" => Ok(Self::INX),
            "INY" => Ok(Self::INY),
            "JMP" => Ok(Self::JMP),
            "JSR" => Ok(Self::JSR),
            "LDA" => Ok(Self::LDA),
            "LDX" => Ok(Self::LDX),
            "LDY" => Ok(Self::LDY),
            "LSR" => Ok(Self::LSR),
            "NOP" => Ok(Self::NOP),
            "ORA" => Ok(Self::ORA),
            "PHA" => Ok(Self::PHA),
            "PHP" => Ok(Self::PHP),
            "PLA" => Ok(Self::PLA),
            "PLP" => Ok(Self::PLP),
            "ROL" => Ok(Self::ROL),
            "ROR" => Ok(Self::ROR),
            "RTI" => Ok(Self::RTI),
            "RTS" => Ok(Self::RTS),
            "SBC" => Ok(Self::SBC),
            "SEC" => Ok(Self::SEC),
            "SED" => Ok(Self::SED),
            "SEI" => Ok(Self::SEI),
            "STA" => Ok(Self::STA),
            "STX" => Ok(Self::STX),
            "STY" => Ok(Self::STY),
            "TAX" => Ok(Self::TAX),
            "TAY" => Ok(Self::TAY),
            "TSX" => Ok(Self::TSX),
            "TXA" => Ok(Self::TXA),
            "TXS" => Ok(Self::TXS),
            "TYA" => Ok(Self::TYA),
            _ => Err(
                OpCodeError::InvalidOpCode(
                    format!("Invalid opcode '{}'", opcode)
                )
            )
        }
    }
}


/// Preprocessing instructions
#[derive(Clone, Debug, PartialEq)]
pub enum Preprocessor {
    /// Include another source file
    INCSRC,
    /// Include a binary file
    INCBIN,
    /// Store specified bytes
    DSB,
    /// Define symbol
    DEFINE,
    /// Assemble code if defined
    IFDEF,
    /// Assemble code if not defined
    IFNDEF,
    /// Finish if statement
    ENDIF,
}

impl FromStr for Preprocessor {
    type Err = PreprocessorError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let preprocessor = s.to_uppercase();
        match preprocessor.as_str() {
            "INCSRC" => Ok(Preprocessor::INCSRC),
            "INCBIN" => Ok(Preprocessor::INCBIN),
            "DSB" => Ok(Preprocessor::DSB),
            "DEFINE" => Ok(Preprocessor::DEFINE),
            "IFDEF" => Ok(Preprocessor::IFDEF),
            "IFNDEF" => Ok(Preprocessor::IFNDEF),
            "ENDIF" => Ok(Preprocessor::ENDIF),
            _ => Err(
                PreprocessorError::InvalidPreprocessor(
                    format!("Invalid preprocessor '{}'", preprocessor)
                )
            )
        }
    }
}
