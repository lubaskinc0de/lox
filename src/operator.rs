use std::borrow::Cow;

use crate::{
    error::InterpreterError, token::{Literal, Token, TokenType},
};

pub fn unary(op: &TokenType, right: &Literal, line: usize) -> Result<Literal, InterpreterError> {
    match op {
        TokenType::MINUS => match right {
            Literal::NUMBER(number) => Ok(Literal::NUMBER(-number)),
            _ => Err(InterpreterError::Runtime {
                message: "Cannot apply minus to non-number".to_string(),
                token: None,
                line,
                hint: "Ensure the operand is a number".to_string(),
            }),
        },
        TokenType::BANG => Ok(Literal::BOOL(!right.is_truthy())),
        _ => Err(InterpreterError::Runtime {
            message: "Unhandled operator".to_string(),
            token: None,
            line,
            hint: "Check the operator and try again".to_string(),
        }),
    }
}

pub fn calc(
    op: &TokenType,
    left: &f64,
    right: &f64,
    line: usize,
) -> Result<Literal, InterpreterError> {
    match op {
        TokenType::MINUS => Ok(Literal::NUMBER(left - right)),
        TokenType::PLUS => Ok(Literal::NUMBER(left + right)),
        TokenType::STAR => Ok(Literal::NUMBER(left * right)),
        TokenType::Pow => Ok(Literal::NUMBER(left.powf(*right))),
        TokenType::SLASH => {
            if *right == 0.0 {
                Err(InterpreterError::Runtime {
                    message: "Division by zero".to_string(),
                    token: None,
                    line,
                    hint: "Ensure the divisor is not zero".to_string(),
                })
            } else {
                Ok(Literal::NUMBER(left / right))
            }
        }
        _ => Err(InterpreterError::Runtime {
            message: "Unsupported arithmetic operator".to_string(),
            token: None,
            line,
            hint: "Check the operator and try again".to_string(),
        }),
    }
}

pub fn cmp(
    op: &TokenType,
    left: &f64,
    right: &f64,
    line: usize,
) -> Result<Literal, InterpreterError> {
    match op {
        TokenType::LESS => Ok(Literal::BOOL(left < right)),
        TokenType::GREATER => Ok(Literal::BOOL(left > right)),
        TokenType::LessEqual => Ok(Literal::BOOL(left <= right)),
        TokenType::GreaterEqual => Ok(Literal::BOOL(left >= right)),
        _ => Err(InterpreterError::Runtime {
            message: "Unsupported comparison operator".to_string(),
            token: None,
            line,
            hint: "Check the operator and try again".to_string(),
        }),
    }
}

pub fn eq(
    op: &TokenType,
    left: &Literal,
    right: &Literal,
    line: usize,
) -> Result<Literal, InterpreterError> {
    match (left, right) {
        (Literal::NUMBER(left_val), Literal::NUMBER(right_val)) => match op {
            TokenType::BangEqual => Ok(Literal::BOOL(left_val != right_val)),
            TokenType::EqualEqual => Ok(Literal::BOOL(left_val == right_val)),
            _ => Err(InterpreterError::Runtime {
                message: "Unsupported equality operator".to_string(),
                token: None,
                line,
                hint: "Check the operator and try again".to_string(),
            }),
        },
        (Literal::STRING(left_val), Literal::STRING(right_val)) => match op {
            TokenType::BangEqual => Ok(Literal::BOOL(left_val != right_val)),
            TokenType::EqualEqual => Ok(Literal::BOOL(left_val == right_val)),
            _ => Err(InterpreterError::Runtime {
                message: "Unsupported equality operator".to_string(),
                token: None,
                line,
                hint: "Check the operator and try again".to_string(),
            }),
        },
        (Literal::BOOL(left_val), Literal::BOOL(right_val)) => match op {
            TokenType::BangEqual => Ok(Literal::BOOL(left_val != right_val)),
            TokenType::EqualEqual => Ok(Literal::BOOL(left_val == right_val)),
            _ => Err(InterpreterError::Runtime {
                message: "Unsupported equality operator".to_string(),
                token: None,
                line,
                hint: "Check the operator and try again".to_string(),
            }),
        },
        _ => Ok(Literal::BOOL(false)),
    }
}

pub fn logical<'a>(
    left: Cow<'a, Literal>,
    right: Cow<'a, Literal>,
    op: &Token,
) -> Result<Cow<'a, Literal>, InterpreterError> {
    match op.token_type {
        TokenType::OR => {
            if left.is_truthy() {
                Ok(left)
            } else {
                Ok(right)
            }
        }
        TokenType::AND => {
            if !left.is_truthy() {
                Ok(left)
            } else {
                Ok(right)
            }
        }
        _ => panic!("Runtime: Invalid logical op: {}", op),
    }
}