use std::{error::Error, fmt, rc::Rc};

use crate::scanner::Token;

#[allow(dead_code)]
pub trait InterpreterError: Error + fmt::Debug + fmt::Display {}

#[derive(Debug)]
pub struct SyntaxError {
    pub line: usize,
    pub loc: String,
    pub message: String,
}

impl InterpreterError for SyntaxError {}
impl Error for SyntaxError {}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}; line: {}\n Occured here: {}",
            self.message, self.line, self.loc
        )
    }
}

#[derive(Debug)]
pub struct ParserError {
    pub message: String,
    pub token: Token,
}

impl InterpreterError for ParserError {}
impl Error for ParserError {}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}\n Token: {}; Line: {}",
            self.message, self.token, self.token.line
        )
    }
}

#[derive(Debug, Clone)]
pub struct ErrorStack {
    pub stack: Vec<Rc<dyn InterpreterError>>,
}

impl InterpreterError for ErrorStack {}
impl Error for ErrorStack {}
impl ErrorStack {
    pub fn new(errors: Vec<Rc<dyn InterpreterError>>) -> Self {
        Self { stack: errors }
    }
}
impl fmt::Display for ErrorStack {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for err in &self.stack {
            writeln!(f, "Error: {}", err).unwrap()
        }
        Ok(())
    }
}
