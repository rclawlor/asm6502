use std::path::Path;

use crate::{error, parse};

pub fn assemble(source: &str, source_file: &Path) {
    let filename = source_file.to_string_lossy();
    let _ast = match parse::parse(source) {
        Ok(program) => program,
        Err(errors) => {
            error::report_errors(source, &filename, &errors);
            std::process::exit(1);
        }
    };
}
