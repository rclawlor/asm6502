// Third party
use colored::Colorize;

/// Used to indiate severity of error
pub enum ErrorLevel {
    Warning,
    Error
}

impl std::fmt::Display for ErrorLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Warning => write!(f, "{}", "warning".yellow()),
            Self::Error => write!(f, "{}", "error".red())
        }
    }
}


#[derive(Debug)]
pub enum OpCodeError {
    InvalidOpCode(String),
}

impl OpCodeError {
    /// Extract message from error type
    pub fn get_msg(&self) -> String {
        let s = match self {
            Self::InvalidOpCode(s) => s
        };

        s.to_string()
    }
}


/// Errors generated during lexing
///
/// Each error contains the following:
/// 1. A helpful message
/// 2. The line number the error occured on
/// 3. The start index of the error
/// 4. The length of the error for line highlighting
#[allow(dead_code)]
#[derive(Debug)]
pub enum LexerError {
    InvalidNumber(String, usize, usize, usize, String),
    InvalidInstruction(String, usize, usize, usize, String),
    InvalidAddress(String, usize, usize, usize, String),
    FileRead(String)
}

impl LexerError {
    /// Extract message from error type
    pub fn get_msg(&self) -> String {
        let s = match self {
            Self::InvalidNumber(s, _, _, _, _) => s,
            Self::InvalidInstruction(s, _, _, _, _) => s,
            Self::InvalidAddress(s, _, _, _, _) => s,
            Self::FileRead(s) => s
        };

        s.to_string()
    }

    /// Extract row index from error type
    pub fn get_row(&self) -> usize {
        let l = match self {
            Self::InvalidNumber(_, l, _, _, _) => l,
            Self::InvalidInstruction(_, l, _, _, _) => l,
            Self::InvalidAddress(_, l, _, _, _) => l,
            Self::FileRead(_) => &0,
        };

        *l
    }

    /// Extract start index from error type
    pub fn get_start(&self) -> usize {
        let s = match self {
            Self::InvalidNumber(_, _, s, _, _) => s,
            Self::InvalidInstruction(_, _, s, _, _) => s,
            Self::InvalidAddress(_, _, s, _, _) => s,
            Self::FileRead(_) => &0,
        };

        *s
    }

    /// Extract length from error type
    pub fn get_length(&self) -> usize {
        let l = match self {
            Self::InvalidNumber(_, _, _, l, _) => l,
            Self::InvalidInstruction(_, _, _, l, _) => l,
            Self::InvalidAddress(_, _, _, l, _) => l,
            Self::FileRead(_) => &0,
        };

        *l
    }

    /// Extract line from error type
    pub fn get_line(&self) -> String {
        let l = match self {
            Self::InvalidNumber(_, _, _, _, l) => l,
            Self::InvalidInstruction(_, _, _, _, l) => l,
            Self::InvalidAddress(_, _, _, _, l) => l,
            Self::FileRead(_) => "",
        };

        l.to_string()
    }
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}


/// Format string with errors in context
///
/// # Example
/// error: invalid instruction 'ACD'
///  --> main.asm:10:1
///      |
///   10 | ACD #$10
///      | ^^^
///      |
///
pub fn context_error(
    level: ErrorLevel,
    msg: &str,
    filename: &str,
    line_idx: usize,
    char_idx: usize,
    length: usize,
    line: &str
) -> String {
    let idx_digits = usize::try_from(line_idx.checked_ilog10().unwrap_or(0) + 1).unwrap_or(4);
    let idx_spacing = " ".repeat(idx_digits + 1);
    let underline = format!("{}{}", " ".repeat(char_idx), "^".repeat(length));

    format!(
        "{level}: {msg}\n\
        \x20 --> {filename}:{row}:{col}\n\
        \x20 {spacing}|\n\
        \x20 {row:<width$}| {line}\n\
        \x20 {spacing}|{underline}\n\
        \x20 {spacing}|",
        level=level,
        msg=msg,
        filename=filename,
        row=line_idx,
        col=char_idx,
        spacing=idx_spacing,
        width=idx_digits + 1,
        line=line,
        underline=underline
    )
}
