use std::{fs, path};

use clap::Parser;

mod assemble;
mod ast;
mod error;
mod lex;
mod parse;
mod semantic;

use crate::{assemble::assemble, semantic::semantic_analysis};

#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// The path to the file to read
    #[arg(short, long)]
    file: path::PathBuf,
}

fn main() {
    let args = Args::parse();

    let source = fs::read_to_string(&args.file).unwrap_or_else(|err| {
        eprintln!("Error reading file '{}': {}", args.file.display(), err);
        std::process::exit(1);
    });
    let filename = args.file.to_string_lossy().to_string();

    let ast = assemble(&source, &filename);
    let instrs = match semantic_analysis(&ast) {
        Ok(instrs) => instrs,
        Err(e) => {
            error::report_errors(&source, &filename, &e);
            std::process::exit(1);
        }
    };

    for instr in instrs {
        println!("{instr}");
    }
}
