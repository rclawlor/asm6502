// Standard library
use std::fs;

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
    let text = fs::read_to_string(args.path).expect("Unable to read file");

    let mut lexer = Lexer::new(&text);
    let tokens = match lexer.lex() {
        Ok(tokens) => tokens,
        Err(e) => panic!("Error lexing file: {}", e)
    };

    println!("Token output: {:?}", tokens);
}
