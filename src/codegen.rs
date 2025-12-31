use crate::semantic::AnalysedInstruction;

const NULL_BYTE: u8 = 0x00;

pub fn generate_binary(instrs: &Vec<AnalysedInstruction>) -> Vec<u8> {
    let mut bytestream = Bytestream::new();
    let mut address = if instrs.len() > 0 {
        instrs[0].address
    } else {
        0x0000
    };
    for instr in instrs {
        if instr.address > address + 1 {
            for _ in address..instr.address {
                bytestream.write_byte(NULL_BYTE);
            }
        }
        bytestream.write_byte(
            match instr.opcode.value(instr.mode) {
                Some(b) => b,
                None => panic!(),
            }
        );
        if instr.opcode.is_relative() {
            bytestream.write_signed_byte(instr.operand.unwrap())
        } else {
            if let Some(operand) = instr.operand {
                bytestream.write_byte(operand as u8);
            }
        }
        address = instr.address;
    }

    bytestream.bytes
}


#[derive(Default)]
pub struct Bytestream {
    bytes: Vec<u8>,
}

impl Bytestream {
    pub fn new() -> Self {
        Self {
            bytes: Vec::new(),
        }
    }

    /// Write a single byte
    pub fn write_byte(&mut self, value: u8) {
        self.bytes.push(value);
    }

    /// Write a signed single byte
    pub fn write_signed_byte(&mut self, value: i32) {
        self.bytes.push(value as u8);
    }
}
