# asm6502
![Rust CI](https://github.com/rclawlor/asm6502/actions/workflows/ci.yml/badge.svg)

A 6502 assembler written in Rust.

# Requirements
asm6502 requires the Rust toolchain for building which can be found [here](https://rust-lang.org/tools/install/).

# Install
The latest asm6502 binaries can be found [here]() (once v0.1 has been released).

# Usage
asm6502.exe [OPTIONS] --file <FILE> --output <OUTPUT>

Options:
  -f, --file <FILE>      The path to the file to read
  -n, --nes              Compile to iNES format
  -o, --output <OUTPUT>  Output filename
  -p, --print            Print assembled program
  -h, --help             Print help
  -V, --version          Print version
