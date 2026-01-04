use crate::{
    ast::INesHeader,
    semantic::{AnalysedItem, AnalysedProgram},
};

const NULL_BYTE: u8 = 0x00;

pub fn generate_binary(program: &AnalysedProgram, nes: bool) -> Vec<u8> {
    let mut bytestream = Bytestream::new();
    if nes {
        bytestream.write_ines_header(&program.header);
    }
    let mut address = match program.items.first() {
        Some(item) => item.address(),
        None => 0x0000,
    };
    for item in &program.items {
        if item.address() > address + 1 {
            let start_addr = address;
            for _ in start_addr..item.address() {
                bytestream.write_byte(NULL_BYTE);
                address += 1;
            }
        }
        match item {
            AnalysedItem::Instruction(instr) => {
                bytestream.write_byte(match instr.opcode.value(instr.mode) {
                    Some(b) => b,
                    None => panic!(),
                });
                address += 1;
                if instr.opcode.is_relative() {
                    bytestream.write_signed_byte(instr.operand.unwrap());
                    address += 1;
                } else if let Some(operand) = instr.operand {
                    if instr.mode.num_bytes() == 2 {
                        bytestream.write_little_endian_word(operand);
                        address += 2;
                    } else {
                        bytestream.write_byte(operand as u8);
                        address += 1;
                    }
                }
            }
            AnalysedItem::Word(w) => {
                // Little-endian
                bytestream.write_byte(w.lower_byte());
                bytestream.write_byte(w.upper_byte());
            }
        }
    }

    bytestream.bytes
}

#[derive(Default)]
pub struct Bytestream {
    bytes: Vec<u8>,
}

impl Bytestream {
    pub fn new() -> Self {
        Self { bytes: Vec::new() }
    }

    /// Write a single byte
    pub fn write_byte(&mut self, value: u8) {
        self.bytes.push(value);
    }

    /// Write a signed single byte
    pub fn write_signed_byte(&mut self, value: i32) {
        self.bytes.push(value as u8);
    }

    /// Write a little-endian word
    pub fn write_little_endian_word(&mut self, value: i32) {
        self.write_byte((value & 0x00ff).try_into().unwrap());
        self.write_byte(((value & 0xff00) >> 8).try_into().unwrap());
    }

    /// Write iNES header
    pub fn write_ines_header(&mut self, header: &INesHeader) {
        // Write NES + MSDOS newline
        for b in [0x4E, 0x45, 0x53, 0x1A] {
            self.write_byte(b);
        }

        self.write_byte(header.prg_size_16kb);
        self.write_byte(header.chr_size_16kb);
        let mapper_l = header.mapper & 0b00001111;
        let mapper_h = header.mapper & 0b11110000;
        let mirror = header.mirror & 0b00000001;
        self.write_byte((mapper_l << 4) | mirror);
        self.write_byte(mapper_h << 4);
        // Flags 8, 9 and 10
        self.write_byte(0x00);
        self.write_byte(0x00);
        self.write_byte(0x00);
        // Unused
        for _ in 11..=15 {
            self.write_byte(NULL_BYTE);
        }
    }
}
