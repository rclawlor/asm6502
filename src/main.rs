mod error;
mod lexer;

use lexer::Lexer;


const EXAMPLE: &str = "
    #%1111\n
    #$05\n
    #d10
";


fn main() {
    let text = EXAMPLE.to_string();
    let mut lexer = Lexer::new(&text);
    let tokens = match lexer.lex() {
        Ok(tokens) => tokens,
        Err(e) => panic!("Error lexing file: {}", e)
    };

    println!("Token output: {:?}", tokens);
}
