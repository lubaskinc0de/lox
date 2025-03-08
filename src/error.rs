use std::{error::Error, fmt, sync::Arc};

#[allow(dead_code)]
pub trait InterpreterError: Error + fmt::Debug + fmt::Display {}

#[derive(Debug)]
pub struct SyntaxError {
    pub line: u32,
    pub loc: u32,
    pub message: String,
}

impl InterpreterError for SyntaxError {}
impl Error for SyntaxError {}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{} <- {}", self.line, self.loc, self.message)
    }
}

#[derive(Debug, Clone)]
pub struct ErrorStack {
    pub stack: Vec<Arc<dyn InterpreterError>>,
}

impl InterpreterError for ErrorStack {}
impl Error for ErrorStack {}

impl fmt::Display for ErrorStack {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for err in &self.stack {
            writeln!(f, "{}", err).unwrap()
        }
        Ok(())
    }
}
