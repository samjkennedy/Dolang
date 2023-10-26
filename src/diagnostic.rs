use crate::lexer::Loc;

use std::fmt;

#[derive(Debug)]
pub enum Severity {
    Info,
    Warn,
    Error,
}

#[derive(Debug)]
pub struct Diagnostic {
    pub severity: Severity,
    pub loc: Loc,
    pub message: String,
    pub hint: Option<String>,
}

impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Info => write!(f, "INFO"),
            Self::Warn => write!(f, "WARN"),
            Self::Error => write!(f, "ERROR"),
        }
    }
}

pub trait Diagnoster {
    fn report(&mut self, loc: &Loc, severity: Severity, message: &str, hint: Option<&str>);
}

pub struct StdoutDiagnoster {}

impl Diagnoster for StdoutDiagnoster {
    fn report(&mut self, loc: &Loc, severity: Severity, message: &str, hint: Option<&str>) {
        eprintln!("{} {}: {}", severity, loc, message);
        if let Some(hint) = hint {
            eprintln!(" Hint: {}", hint);
        }
    }
}
