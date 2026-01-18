//! Assemble a file into a [`Program`] containing AST nodes

use crate::{ast::Program, error, parse};

/// Assembles a file into a [`Program`]
///
/// If any errors occur during parsing, they will be printed
/// to stderr
pub fn assemble(source: &str, filename: &str) -> Program {
    match parse::parse(source) {
        Ok(program) => program,
        Err(errors) => {
            error::report_errors(source, filename, &errors);
            std::process::exit(1);
        }
    }
}
