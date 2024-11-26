#[derive(Debug)]
pub enum LexerError {
    InvalidNumber(String),
    InvalidInstruction(String),
    FileRead(String)
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
