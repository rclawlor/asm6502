use std::{fs, path};

use clap::Parser;

mod assemble;
mod ast;
mod codegen;
mod error;
mod lex;
mod parse;
mod semantic;

use crate::{assemble::assemble, codegen::generate_binary, semantic::semantic_analysis};

#[derive(Parser)]
#[command(version, about)]
struct Args {
    /// The path to the file to read
    #[arg(short, long)]
    file: path::PathBuf,
    /// Compile to iNES format
    #[arg(short, long)]
    nes: bool,
    /// Output filename
    #[arg(short, long)]
    output: path::PathBuf,
    /// Print assembled program
    #[arg(short, long)]
    print: bool,
}

fn main() {
    let args = Args::parse();

    let source = fs::read_to_string(&args.file).unwrap_or_else(|err| {
        eprintln!("Error reading file '{}': {}", args.file.display(), err);
        std::process::exit(1);
    });
    let filename = args.file.to_string_lossy().to_string();

    let ast = assemble(&source, &filename);
    let program = match semantic_analysis(&ast) {
        Ok(program) => program,
        Err(e) => {
            error::report_errors(&source, &filename, &e);
            std::process::exit(1);
        }
    };

    if args.print {
        for item in &program.items {
            println!("{item}");
        }
    }

    let binary = generate_binary(&program, args.nes);
    match std::fs::write(args.output, binary) {
        Ok(()) => (),
        Err(e) => {
            println!("{e}");
            std::process::exit(1);
        }
    }
}
