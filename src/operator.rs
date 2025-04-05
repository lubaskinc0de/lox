use std::{cell::RefCell, rc::Rc};

use crate::{
    error::InterpreterError,
    helper::{LitResult, RcMutLitResult},
    token::{Literal, RcMutLiteral, Token, TokenType},
};

pub fn unary(op: &TokenType, right: &Literal, line: usize) -> RcMutLitResult {
    match op {
        TokenType::MINUS => match right {
            Literal::NUMBER(number) => Ok(Rc::new(RefCell::new(Literal::NUMBER(-number)))),
            _ => Err(InterpreterError::Runtime {
                message: "Cannot apply minus to non-number".to_string(),
                token: None,
                line,
                hint: "Ensure the operand is a number".to_string(),
            }),
        },
        TokenType::BANG => Ok(Rc::new(RefCell::new(Literal::BOOL(!right.is_truthy())))),
        _ => Err(InterpreterError::Runtime {
            message: "Unhandled operator".to_string(),
            token: None,
            line,
            hint: "Check the operator and try again".to_string(),
        }),
    }
}

pub fn calc(op: &TokenType, left: &f64, right: &f64, line: usize) -> LitResult {
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

pub fn cmp(op: &TokenType, left: &f64, right: &f64, line: usize) -> LitResult {
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

pub fn eq(op: &TokenType, left: &Literal, right: &Literal, line: usize) -> RcMutLitResult {
    match (left, right) {
        (Literal::NUMBER(left_val), Literal::NUMBER(right_val)) => match op {
            TokenType::BangEqual => Ok(Rc::new(RefCell::new(Literal::BOOL(left_val != right_val)))),
            TokenType::EqualEqual => {
                Ok(Rc::new(RefCell::new(Literal::BOOL(left_val == right_val))))
            }
            _ => Err(InterpreterError::Runtime {
                message: "Unsupported equality operator".to_string(),
                token: None,
                line,
                hint: "Check the operator and try again".to_string(),
            }),
        },
        (Literal::STRING(left_val), Literal::STRING(right_val)) => match op {
            TokenType::BangEqual => Ok(Rc::new(RefCell::new(Literal::BOOL(left_val != right_val)))),
            TokenType::EqualEqual => {
                Ok(Rc::new(RefCell::new(Literal::BOOL(left_val == right_val))))
            }
            _ => Err(InterpreterError::Runtime {
                message: "Unsupported equality operator".to_string(),
                token: None,
                line,
                hint: "Check the operator and try again".to_string(),
            }),
        },
        (Literal::BOOL(left_val), Literal::BOOL(right_val)) => match op {
            TokenType::BangEqual => Ok(Rc::new(RefCell::new(Literal::BOOL(left_val != right_val)))),
            TokenType::EqualEqual => {
                Ok(Rc::new(RefCell::new(Literal::BOOL(left_val == right_val))))
            }
            _ => Err(InterpreterError::Runtime {
                message: "Unsupported equality operator".to_string(),
                token: None,
                line,
                hint: "Check the operator and try again".to_string(),
            }),
        },
        _ => Ok(Rc::new(RefCell::new(Literal::BOOL(false)))),
    }
}

pub fn logical<'a>(left: RcMutLiteral, right: RcMutLiteral, op: &Token) -> RcMutLitResult {
    match op.token_type {
        TokenType::OR => {
            if left.borrow().is_truthy() {
                Ok(left)
            } else {
                Ok(right)
            }
        }
        TokenType::AND => {
            if !left.borrow().is_truthy() {
                Ok(left)
            } else {
                Ok(right)
            }
        }
        _ => panic!("Runtime: Invalid logical op: {}", op),
    }
}
