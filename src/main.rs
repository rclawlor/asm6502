// Third party
use clap::Parser;

// Local
mod error;
mod instruction;
mod lexer;

use error::context_error;
use lexer::Lexer;

use crate::error::ErrorLevel;


#[derive(Parser)]
#[command(version, about)]
struct Cli {
    /// The path to the file to read
    #[arg(short, long)]
    path: std::path::PathBuf
}


fn main() {
    let args = Cli::parse();

    // Parse inital file
    let mut lexer = Lexer::new(args.path);
    let tokens = match lexer.lex() {
        Ok(tokens) => tokens,
        Err(e) => {
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
