// Standard library
use std::fs;
use std::io::{self, BufRead};

// Third party
use clap::Parser;

// Local
mod error;
mod lexer;
use lexer::Lexer;


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
        Err(e) => panic!("Error lexing file: {}", e)
    };

    println!("Token output: {:?}", tokens);
}
