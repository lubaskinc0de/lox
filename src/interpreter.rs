use crate::{
    error::InterpreterError,
    parser::Expr,
    scanner::{Literal, TokenType},
};

pub struct Interpreter {}

impl Interpreter {
    pub fn interpret(&self, expr: Expr) -> Result<Literal, InterpreterError> {
        match expr {
            Expr::Literal(val) => Ok(val.clone()),
            Expr::Grouping(expr) => self.interpret(*expr),
            Expr::Unary { right, op } => {
                let evaluated = self.interpret(*right)?;
                self.evaluate_unary(&op.token_type, evaluated, op.line)
            }
            Expr::Binary { left, op, right } => {
                let right_eval = self.interpret(*right)?;
                let left_eval = self.interpret(*left)?;

                match op.token_type {
                    TokenType::MINUS | TokenType::PLUS | TokenType::STAR | TokenType::SLASH => {
                        if let (Literal::NUMBER(left_val), Literal::NUMBER(right_val)) =
                            (&left_eval, &right_eval)
                        {
                            self.evaluate_arithmetic(&op.token_type, *left_val, *right_val, op.line)
                        } else {
                            Err(InterpreterError::RuntimeError {
                                message: "Cannot perform arithmetic on non-numbers".to_string(),
                                token: Some(op.clone()),
                                line: op.line,
                                hint: "Ensure both operands are numbers".to_string(),
                            })
                        }
                    }
                    TokenType::LESS
                    | TokenType::GREATER
                    | TokenType::LessEqual
                    | TokenType::GreaterEqual => {
                        if let (Literal::NUMBER(left_val), Literal::NUMBER(right_val)) =
                            (&left_eval, &right_eval)
                        {
                            self.evaluate_comparison(&op.token_type, *left_val, *right_val, op.line)
                        } else {
                            Err(InterpreterError::RuntimeError {
                                message: "Cannot perform comparison on non-numbers".to_string(),
                                token: Some(op.clone()),
                                line: op.line,
                                hint: "Ensure both operands are numbers".to_string(),
                            })
                        }
                    }
                    TokenType::BangEqual | TokenType::EqualEqual => {
                        self.evaluate_equality(&op.token_type, &left_eval, &right_eval, op.line)
                    }
                    _ => Err(InterpreterError::RuntimeError {
                        message: "Unsupported operator".to_string(),
                        token: Some(op.clone()),
                        line: op.line,
                        hint: "Check the operator and try again".to_string(),
                    }),
                }
            }
        }
    }

    fn evaluate_unary(
        &self,
        op: &TokenType,
        right: Literal,
        line: usize,
    ) -> Result<Literal, InterpreterError> {
        match op {
            TokenType::MINUS => match right {
                Literal::NUMBER(number) => Ok(Literal::NUMBER(-number)),
                _ => Err(InterpreterError::RuntimeError {
                    message: "Cannot apply minus to non-number".to_string(),
                    token: None,
                    line,
                    hint: "Ensure the operand is a number".to_string(),
                }),
            },
            TokenType::BANG => Ok(Literal::BOOL(!right.is_truthy())),
            _ => Err(InterpreterError::RuntimeError {
                message: "Unhandled operator".to_string(),
                token: None,
                line,
                hint: "Check the operator and try again".to_string(),
            }),
        }
    }

    fn evaluate_arithmetic(
        &self,
        op: &TokenType,
        left: f64,
        right: f64,
        line: usize,
    ) -> Result<Literal, InterpreterError> {
        match op {
            TokenType::MINUS => Ok(Literal::NUMBER(left - right)),
            TokenType::PLUS => Ok(Literal::NUMBER(left + right)),
            TokenType::STAR => Ok(Literal::NUMBER(left * right)),
            TokenType::SLASH => {
                if right == 0.0 {
                    Err(InterpreterError::RuntimeError {
                        message: "Division by zero".to_string(),
                        token: None,
                        line,
                        hint: "Ensure the divisor is not zero".to_string(),
                    })
                } else {
                    Ok(Literal::NUMBER(left / right))
                }
            }
            _ => Err(InterpreterError::RuntimeError {
                message: "Unsupported arithmetic operator".to_string(),
                token: None,
                line,
                hint: "Check the operator and try again".to_string(),
            }),
        }
    }

    fn evaluate_comparison(
        &self,
        op: &TokenType,
        left: f64,
        right: f64,
        line: usize,
    ) -> Result<Literal, InterpreterError> {
        match op {
            TokenType::LESS => Ok(Literal::BOOL(left < right)),
            TokenType::GREATER => Ok(Literal::BOOL(left > right)),
            TokenType::LessEqual => Ok(Literal::BOOL(left <= right)),
            TokenType::GreaterEqual => Ok(Literal::BOOL(left >= right)),
            _ => Err(InterpreterError::RuntimeError {
                message: "Unsupported comparison operator".to_string(),
                token: None,
                line,
                hint: "Check the operator and try again".to_string(),
            }),
        }
    }

    fn evaluate_equality(
        &self,
        op: &TokenType,
        left: &Literal,
        right: &Literal,
        line: usize,
    ) -> Result<Literal, InterpreterError> {
        match (left, right) {
            (Literal::NUMBER(left_val), Literal::NUMBER(right_val)) => match op {
                TokenType::BangEqual => Ok(Literal::BOOL(left_val != right_val)),
                TokenType::EqualEqual => Ok(Literal::BOOL(left_val == right_val)),
                _ => Err(InterpreterError::RuntimeError {
                    message: "Unsupported equality operator".to_string(),
                    token: None,
                    line,
                    hint: "Check the operator and try again".to_string(),
                }),
            },
            (Literal::STRING(left_val), Literal::STRING(right_val)) => match op {
                TokenType::BangEqual => Ok(Literal::BOOL(left_val != right_val)),
                TokenType::EqualEqual => Ok(Literal::BOOL(left_val == right_val)),
                _ => Err(InterpreterError::RuntimeError {
                    message: "Unsupported equality operator".to_string(),
                    token: None,
                    line,
                    hint: "Check the operator and try again".to_string(),
                }),
            },
            (Literal::BOOL(left_val), Literal::BOOL(right_val)) => match op {
                TokenType::BangEqual => Ok(Literal::BOOL(left_val != right_val)),
                TokenType::EqualEqual => Ok(Literal::BOOL(left_val == right_val)),
                _ => Err(InterpreterError::RuntimeError {
                    message: "Unsupported equality operator".to_string(),
                    token: None,
                    line,
                    hint: "Check the operator and try again".to_string(),
                }),
            },
            _ => Ok(Literal::BOOL(false)),
        }
    }
}
