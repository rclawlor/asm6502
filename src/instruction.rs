// Standard library
use std::str::FromStr;

// Local
use crate::error::{OpCodeError, PreprocessorError, RegisterError};

/// The 6502 registers
#[derive(Clone, Debug, PartialEq)]
pub enum Register {
    A,
    X,
    Y,
}

impl FromStr for Register {
    type Err = RegisterError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "A" => Ok(Self::A),
            "X" => Ok(Self::X),
            "Y" => Ok(Self::Y),
            _ => Err(RegisterError::InvalidRegister(format!(
                "Invalid register '{}'",
                s
            ))),
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
    IndirectY,
    Implied
}

/// Used to generate the match arm for an 'implied'
/// addressing mode
///
/// # Arguments
/// `addr` the hex opcode
/// `mode` the addressing mode
macro_rules! implied {
    ($addr:expr, $mode:ident) => {
        match $mode {
            AddressingMode::Implied => $addr,
            _ => return Err(OpCodeError::InvalidAddressingMode($mode))
        }
    };
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

impl OpCode {
    pub fn get_hex(&self, addressing_mode: AddressingMode) -> Result<u8, OpCodeError> {
        let hex = match self {
            Self::ADC => 0,
            Self::AND => 0,
            Self::ASL => 0,
            Self::BCC => 0,
            Self::BCS => 0,
            Self::BEQ => 0,
            Self::BIT => 0,
            Self::BMI => 0,
            Self::BNE => 0,
            Self::BPL => 0,
            Self::BRK => implied!(0x00, addressing_mode),
            Self::BVC => 0,
            Self::BVS => 0,
            Self::CLC => implied!(0x18, addressing_mode),
            Self::CLD => implied!(0xd8, addressing_mode),
            Self::CLI => implied!(0x58, addressing_mode),
            Self::CLV => implied!(0xb8, addressing_mode),
            Self::CMP => 0,
            Self::CPX => 0,
            Self::CPY => 0,
            Self::DEC => 0,
            Self::DEX => implied!(0xca, addressing_mode),
            Self::DEY => implied!(0x88, addressing_mode),
            Self::EOR => 0,
            Self::INC => 0,
            Self::INX => implied!(0xe8, addressing_mode),
            Self::INY => implied!(0xc8, addressing_mode),
            Self::JMP => 0,
            Self::JSR => 0,
            Self::LDA => 0,
            Self::LDX => 0,
            Self::LDY => 0,
            Self::LSR => 0,
            Self::NOP => implied!(0xea, addressing_mode),
            Self::ORA => 0,
            Self::PHA => implied!(0x48, addressing_mode),
            Self::PHP => implied!(0x08, addressing_mode),
            Self::PLA => implied!(0x68, addressing_mode),
            Self::PLP => implied!(0x28, addressing_mode),
            Self::ROL => 0,
            Self::ROR => 0,
            Self::RTI => implied!(0x40, addressing_mode),
            Self::RTS => implied!(0x60, addressing_mode),
            Self::SBC => 0,
            Self::SEC => implied!(0x38, addressing_mode),
            Self::SED => implied!(0xf8, addressing_mode),
            Self::SEI => implied!(0x78, addressing_mode),
            Self::STA => 0,
            Self::STX => 0,
            Self::STY => 0,
            Self::TAX => implied!(0xaa, addressing_mode),
            Self::TAY => implied!(0xa8, addressing_mode),
            Self::TSX => implied!(0xba, addressing_mode),
            Self::TXA => implied!(0x8a, addressing_mode),
            Self::TXS => implied!(0x9a, addressing_mode),
            Self::TYA => implied!(0x98, addressing_mode),
        };

        Ok(hex)
    }
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
            _ => Err(OpCodeError::InvalidOpCode(format!(
                "Invalid opcode '{}'",
                opcode
            ))),
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
            _ => Err(PreprocessorError::InvalidPreprocessor(format!(
                "Invalid preprocessor '{}'",
                preprocessor
            ))),
        }
    }
}
