use std::{fs, path};

use clap::Parser;

mod assemble;
mod ast;
mod error;
mod lex;
mod parse;

use crate::assemble::assemble;

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

    assemble(&source, &args.file);
}
