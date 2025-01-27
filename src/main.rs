use clap::Parser;

// Local
mod assembler;
mod error;
mod instruction;
mod lexer;

use error::context_error;
use lexer::Lexer;

use crate::{assembler::Assembler, error::ErrorLevel};

#[derive(Parser)]
#[command(version, about)]
struct Cli {
    /// The path to the file to read
    #[arg(short, long)]
    path: std::path::PathBuf,
}

fn main() {
    let args = Cli::parse();

    // Parse inital file
    let mut lexer = Lexer::new(args.path);
    let tokens = match lexer.lex() {
        Ok(tokens) => tokens,
        Err(e) => {
            let error = e.generate_context_error(ErrorLevel::Error);
            println!("{}", error);
            return;
        }
    };

    let mut assembler = Assembler::new();
    let hex = match assembler.assemble(tokens) {
        Ok(hex) => hex,
        Err(e) => {
            let error = e.generate_context_error(ErrorLevel::Error);
            println!("{}", error);
            return;
        }
    };
}
