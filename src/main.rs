use clap::Parser;

// Local
mod ast;
mod error;
mod lex;
mod parse;

#[derive(Parser)]
#[command(version, about)]
struct Cli {
    /// The path to the file to read
    #[arg(short, long)]
    path: std::path::PathBuf,
}

fn main() {
    let args = Cli::parse();
    println!("Hello!");
}
