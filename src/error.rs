// Standard library
use std::{
    fs::File,
    io::{BufRead, BufReader},
    path::PathBuf,
};

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
    /// (msg, row, col, length, line, path)
    InvalidNumber(String, usize, usize, usize, String, PathBuf),
    /// Used when an invalid register is parsed
    ///
    /// (msg, row, col, length, line, path)
    InvalidRegister(String, usize, usize, usize, String, PathBuf),
    /// Used when an invalid character is encountered
    ///
    /// (msg, row, col, length, line, path)
    InvalidCharacter(String, usize, usize, usize, String, PathBuf),
    /// Used when an invalid instruction is encountered
    ///
    /// (msg, row, col, length, line, path)
    InvalidInstruction(String, usize, usize, usize, String, PathBuf),
    /// Used when an invalid preprocessor is encountered
    ///
    /// (msg, row, col, length, line, path)
    InvalidPreprocessor(String, usize, usize, usize, String, PathBuf),
    /// Used when an invalid address is encountered
    ///
    /// (msg, row, col, length, line, path)
    InvalidAddress(String, usize, usize, usize, String, PathBuf),
    /// Used when unable to read a file
    ///
    /// (msg, row, col, length, line, path)
    FileRead(String, usize, usize, usize, String, PathBuf)
}

impl LexerError {
    /// Extract message from error type
    fn get_msg(&self) -> String {
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
    fn get_row(&self) -> usize {
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
    fn get_start(&self) -> usize {
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
    fn get_length(&self) -> usize {
        let l = match self {
            Self::InvalidNumber(.., l, _, _) => l,
            Self::InvalidRegister(.., l, _, _) => l,
            Self::InvalidCharacter(.., l, _, _) => l,
            Self::InvalidInstruction(.., l, _, _) => l,
            Self::InvalidPreprocessor(.., l, _, _) => l,
            Self::InvalidAddress(.., l, _, _) => l,
            Self::FileRead(.., l, _, _) => l,
        };

        *l
    }

    /// Extract line from error type
    fn get_line(&self) -> String {
        let l = match self {
            Self::InvalidNumber(.., l, _) => l,
            Self::InvalidRegister(.., l, _) => l,
            Self::InvalidCharacter(.., l, _) => l,
            Self::InvalidInstruction(.., l, _) => l,
            Self::InvalidPreprocessor(.., l, _) => l,
            Self::InvalidAddress(.., l, _) => l,
            Self::FileRead(.., l, _) => l,
        };

        l.to_string()
    }

    /// Extract path from error type
    fn get_path(&self) -> PathBuf {
        let p = match self {
            Self::InvalidNumber(.., l) => l,
            Self::InvalidRegister(.., l) => l,
            Self::InvalidCharacter(.., l) => l,
            Self::InvalidInstruction(.., l) => l,
            Self::InvalidPreprocessor(.., l) => l,
            Self::InvalidAddress(.., l) => l,
            Self::FileRead(.., l) => l,
        };

        p.clone()
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
    pub fn generate_context_error(&self, level: ErrorLevel) -> String {
        let msg = self.get_msg();
        let line_idx = self.get_row() + 1;
        let char_idx = self.get_start() + 1;
        let length = self.get_length();
        let line = self.get_line();
        let path = self.get_path();

        let idx_digits = usize::try_from(line_idx.checked_ilog10().unwrap_or(0) + 1).unwrap_or(4);
        let idx_spacing = " ".repeat(idx_digits + 1);
        let underline = format!("{}{}", " ".repeat(char_idx), "^".repeat(length));

        format!(
            "{level}: {msg}\n\
            \x20 --> {path:?}:{row}:{col}\n\
            \x20 {spacing}|\n\
            \x20 {row:<width$}| {line}\n\
            \x20 {spacing}|{underline}\n\
            \x20 {spacing}|",
            level=level,
            msg=msg,
            path=path,
            row=line_idx,
            col=char_idx,
            spacing=idx_spacing,
            width=idx_digits + 1,
            line=line,
            underline=underline
        )
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
    ///
    /// (msg, row, file)
    InvalidAddressingMode(String, usize, PathBuf),
    /// Used when a preprocessor has incorrect arguments
    ///
    /// (msg, row, file)
    InvalidPreprocessorArguments(String, usize, PathBuf)
}

impl AssemblerError {
    /// Extract message from error type
    fn get_msg(&self) -> String {
        let s = match self {
            Self::InvalidAddressingMode(s, ..) => s,
            Self::InvalidPreprocessorArguments(s, ..) => s,
        };

        s.to_string()
    }

    /// Extract row index from error type
    fn get_row(&self) -> usize {
        let l = match self {
            Self::InvalidAddressingMode(_, l, _) => l,
            Self::InvalidPreprocessorArguments(_, l, _) => l,
        };

        *l
    }

    /// Extract path from error type
    fn get_path(&self) -> PathBuf {
        let p = match self {
            Self::InvalidAddressingMode(.., l) => l,
            Self::InvalidPreprocessorArguments(.., l) => l,
        };

        p.clone()
    }

    /// Extract line from error type
    fn get_line(&self) -> String {
        let file = File::open(
            &self.get_path()
        ).expect("File already opened during lexing");

        let lines = BufReader::new(file).lines();
        return lines.into_iter().nth(self.get_row()).unwrap().expect("Line exists")
    }

    /// Format string with errors in context
    ///
    /// # Example
    /// error: could not find definition for symbol 'var'
    ///  --> main.asm:10
    ///      |
    ///   10 | ACD var
    ///      | ^^^^^^^
    ///      |
    ///
    pub fn generate_context_error(&self, level: ErrorLevel) -> String {
        let msg = self.get_msg();
        let line_idx = self.get_row() + 1;
        let line = self.get_line();
        let char_idx = 1;
        let path = self.get_path();
        let length = line.len();

        let idx_digits = usize::try_from(line_idx.checked_ilog10().unwrap_or(0) + 1).unwrap_or(4);
        let idx_spacing = " ".repeat(idx_digits + 1);
        let underline = format!("{}{}", " ".repeat(char_idx), "^".repeat(length));

        format!(
            "{level}: {msg}\n\
            \x20 --> {path:?}:{row}\n\
            \x20 {spacing}|\n\
            \x20 {row:<width$}| {line}\n\
            \x20 {spacing}|{underline}\n\
            \x20 {spacing}|",
            level=level,
            msg=msg,
            path=path,
            row=line_idx,
            spacing=idx_spacing,
            width=idx_digits + 1,
            line=line,
            underline=underline
        )
    }
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
