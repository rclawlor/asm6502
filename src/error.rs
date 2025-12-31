use crate::ast::Span;
use ariadne::{Color, Label, Report, ReportKind, Source};
use std::fmt;

#[derive(Debug, Clone)]
pub struct CompileError {
    pub message: String,
    pub span: Span,
    pub help: Option<String>,
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for CompileError {}

pub fn report_errors(source: &str, filename: &str, errors: &[CompileError]) {
    for error in errors {
        Report::build(
            ReportKind::Error,
            (filename, error.span.start..error.span.end),
        )
        .with_message(&error.message)
        .with_label(
            Label::new((filename, error.span.start..error.span.end))
                .with_message(&error.message)
                .with_color(Color::Red),
        )
        .with_help(if let Some(h) = &error.help {
            h.as_str()
        } else {
            ""
        })
        .finish()
        .eprint((filename, Source::from(source)))
        .unwrap();
    }
}
