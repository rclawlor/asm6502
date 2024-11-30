// Standard library
use std::fs;
use std::io::{self, BufRead};

// Third party
use clap::Parser;

// Local
mod error;
mod instruction;
mod lexer;

use error::context_error;
use lexer::Lexer;

use crate::error::ErrorLevel;


/// Search for a pattern in a file and display the lines that contain it.
#[derive(Parser)]
#[command(version, about)]
struct Cli {
    /// The path to the file to read
    #[arg(short, long)]
    path: std::path::PathBuf
}


fn main() {
    let args = Cli::parse();
    let file = fs::File::open(args.path).expect("Unable to read file");
    let lines = io::BufReader::new(file).lines();

    let mut lexer = Lexer::new();
    let tokens = match lexer.lex(lines) {
        Ok(tokens) => tokens,
        Err(e) => {
            // eprintln!("Error lexing file: {}", e);
            let error = context_error(
                ErrorLevel::Error,
                &e.get_msg(),
                "main.asm",
                e.get_row() + 1,
                e.get_start() + 1,
                e.get_length(),
                &e.get_line()
            );
            println!("{}", error);
            return
        }
    };

    println!("Token output:");
    for (idx, token) in tokens.iter().enumerate() {
        println!("  {} {:?}", idx + 1, token);
    }
}
