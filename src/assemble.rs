use crate::{ast::Program, error, parse};

pub fn assemble(source: &str, filename: &String) -> Program {
    match parse::parse(source) {
        Ok(program) => program,
        Err(errors) => {
            error::report_errors(source, &filename, &errors);
            std::process::exit(1);
        }
    }
}
