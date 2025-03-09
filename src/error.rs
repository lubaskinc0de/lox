use std::rc::Rc;
use thiserror::Error;

use crate::scanner::Token;

#[derive(Debug, Error)]
pub enum InterpreterError {
    #[error("While scanning: {message}; line: {line}\n Occurred here: {loc}")]
    SyntaxError {
        line: usize,
        loc: String,
        message: String,
    },

    #[error("While parsing: {message}\n Token: {token}; Line: {line}")]
    ParserError {
        message: String,
        token: Token,
        line: usize,
    },

    #[error("Runtime: {message}\n Token: {token:?}; Line: {line}; Hint: {hint}")]
    RuntimeError {
        message: String,
        token: Option<Token>,
        line: usize,
        hint: String,
    },

    #[error("Multiple errors occurred:\n{}", .stack.iter().map(|err| format!("{}", err)).collect::<Vec<_>>().join("\n"))]
    ErrorStack { stack: Vec<Rc<InterpreterError>> },
}
