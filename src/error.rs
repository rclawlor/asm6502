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
pub enum RegisterError {
    InvalidRegister(String),
}

impl RegisterError {
    /// Extract message from error type
    pub fn get_msg(&self) -> String {
        let s = match self {
            Self::InvalidRegister(s) => s
        };

        s.to_string()
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


#[derive(Debug)]
pub enum PreprocessorError {
    InvalidPreprocessor(String),
}

impl PreprocessorError {
    /// Extract message from error type
    pub fn get_msg(&self) -> String {
        let s = match self {
            Self::InvalidPreprocessor(s) => s
        };

        s.to_string()
    }
}


/// Errors generated during lexing
#[allow(dead_code)]
#[derive(Debug)]
pub enum LexerError {
    /// Used when an invalid number is parsed
    ///
    /// (msg, row, col, length, line)
    InvalidNumber(String, usize, usize, usize, String),
    /// Used when an invalid register is parsed
    ///
    /// (msg, row, col, length, line)
    InvalidRegister(String, usize, usize, usize, String),
    /// Used when an invalid character is encountered
    ///
    /// (msg, row, col, length, line)
    InvalidCharacter(String, usize, usize, usize, String),
    /// Used when an invalid instruction is encountered
    ///
    /// (msg, row, col, length, line)
    InvalidInstruction(String, usize, usize, usize, String),
    /// Used when an invalid preprocessor is encountered
    ///
    /// (msg, row, col, length, line)
    InvalidPreprocessor(String, usize, usize, usize, String),
    /// Used when an invalid address is encountered
    ///
    /// (msg, row, col, length, line)
    InvalidAddress(String, usize, usize, usize, String),
    /// Used when unable to read a file
    ///
    /// (msg, row, col, length, line)
    FileRead(String, usize, usize, usize, String)
}

impl LexerError {
    /// Extract message from error type
    pub fn get_msg(&self) -> String {
        let s = match self {
            Self::InvalidNumber(s, ..) => s,
            Self::InvalidRegister(s, ..) => s,
            Self::InvalidCharacter(s, ..) => s,
            Self::InvalidInstruction(s, ..) => s,
            Self::InvalidPreprocessor(s, ..) => s,
            Self::InvalidAddress(s, ..) => s,
            Self::FileRead(s, ..) => s
        };

        s.to_string()
    }

    /// Extract row index from error type
    pub fn get_row(&self) -> usize {
        let l = match self {
            Self::InvalidNumber(_, l, ..) => l,
            Self::InvalidRegister(_, l, ..) => l,
            Self::InvalidCharacter(_, l, ..) => l,
            Self::InvalidInstruction(_, l, ..) => l,
            Self::InvalidPreprocessor(_, l, ..) => l,
            Self::InvalidAddress(_, l, ..) => l,
            Self::FileRead(_, l, ..) => l,
        };

        *l
    }

    /// Extract start index from error type
    pub fn get_start(&self) -> usize {
        let s = match self {
            Self::InvalidNumber(_, _, s, ..) => s,
            Self::InvalidRegister(_, _, s, ..) => s,
            Self::InvalidCharacter(_, _, s, ..) => s,
            Self::InvalidInstruction(_, _, s, ..) => s,
            Self::InvalidPreprocessor(_, _, s, ..) => s,
            Self::InvalidAddress(_, _, s, ..) => s,
            Self::FileRead(_, _, s, ..) => s,
        };

        *s
    }

    /// Extract length from error type
    pub fn get_length(&self) -> usize {
        let l = match self {
            Self::InvalidNumber(.., l, _) => l,
            Self::InvalidRegister(.., l, _) => l,
            Self::InvalidCharacter(.., l, _) => l,
            Self::InvalidInstruction(.., l, _) => l,
            Self::InvalidPreprocessor(.., l, _) => l,
            Self::InvalidAddress(.., l, _) => l,
            Self::FileRead(.., l, _) => l,
        };

        *l
    }

    /// Extract line from error type
    pub fn get_line(&self) -> String {
        let l = match self {
            Self::InvalidNumber(.., l) => l,
            Self::InvalidRegister(.., l) => l,
            Self::InvalidCharacter(.., l) => l,
            Self::InvalidInstruction(.., l) => l,
            Self::InvalidPreprocessor(.., l) => l,
            Self::InvalidAddress(.., l) => l,
            Self::FileRead(.., l) => l,
        };

        l.to_string()
    }
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}


#[derive(Debug)]
pub enum AssemblerError {
    /// Used when an instruction has an invalid addressing mode
    InvalidAddressingMode(String),
    /// Used when a preprocessor has incorrect arguments
    InvalidPreprocessorArguments(String)
}

impl AssemblerError {
}

impl std::fmt::Display for AssemblerError {
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
