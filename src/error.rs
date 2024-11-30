#[derive(Debug)]
pub enum LexerError {
    InvalidNumber(String),
    InvalidInstruction(String),
    InvalidAddress(String),
    FileRead(String)
}

impl LexerError {
    /// Extract message from error type
    pub fn get_msg(&self) -> String {
        let s = match self {
            Self::InvalidNumber(s) => s,
            Self::InvalidInstruction(s) => s,
            Self::InvalidAddress(s) => s,
            Self::FileRead(s) => s
        };

        s.to_string()
    }
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
