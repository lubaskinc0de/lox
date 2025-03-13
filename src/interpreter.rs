use std::borrow::Cow;

use crate::{
    environment::Environment,
    error::InterpreterError,
    parser::Expr,
    scanner::{Literal, Token, TokenType},
    stmt::Stmt,
};

pub struct Interpreter<'a> {
    pub env: &'a mut Environment,
}

impl<'a> Interpreter<'a> {
    pub fn interpret(&mut self, program: &[Stmt]) -> Result<(), InterpreterError> {
        Ok(for stmt in program {
            self.execute(stmt)?;
        })
    }
    fn execute(&mut self, stmt: &Stmt) -> Result<(), InterpreterError> {
        match stmt {
            Stmt::Expression(expr) => self.evaluate(expr).map(|_| {}),
            Stmt::Print(expr) => {
                let evaluated = self.evaluate(expr)?;
                Ok(self.execute_print(evaluated.as_ref()))
            }
            Stmt::Var { expr, name } => self.execute_var(expr, name),
        }?;
        Ok(())
    }
    fn execute_print(&self, val: &Literal) {
        match val {
            Literal::IDENTIFIER(val) => println!("{}", val),
            Literal::STRING(val) => println!("{}", val),
            Literal::NUMBER(val) => println!("{}", val),
            Literal::BOOL(val) => println!("{}", val),
            Literal::NIL => println!("nil"),
        }
    }
    fn execute_var(&mut self, expr: &Option<Expr>, name: &Token) -> Result<(), InterpreterError> {
        let mut right: Option<Literal> = None;

        if let Some(t_expr) = expr {
            let evaluated = self.evaluate(t_expr)?;
            right = Some(evaluated.into_owned());
        }

        let identifier = name.expect_identifier()?;
        self.env.define(identifier, right)?;

        Ok(())
    }
    fn evaluate(&self, expr: &'a Expr) -> Result<Cow<'_, Literal>, InterpreterError> {
        match expr {
            Expr::Literal(val) => Ok(Cow::Borrowed(val)),
            Expr::Grouping(expr) => self.evaluate(&expr),
            Expr::Unary { right, op } => {
                let evaluated = self.evaluate(&right)?;
                let lit = self.evaluate_unary(&op.token_type, evaluated.as_ref(), op.line)?;
                Ok(Cow::Owned(lit))
            }
            Expr::Binary { left, op, right } => {
                let right_eval = self.evaluate(&right)?;
                let left_eval = self.evaluate(&left)?;

                match op.token_type {
                    TokenType::MINUS
                    | TokenType::PLUS
                    | TokenType::STAR
                    | TokenType::SLASH
                    | TokenType::Pow => {
                        if let (Literal::NUMBER(left_val), Literal::NUMBER(right_val)) =
                            (left_eval.as_ref(), right_eval.as_ref())
                        {
                            let lit = self.evaluate_arithmetic(
                                &op.token_type,
                                *left_val,
                                *right_val,
                                op.line,
                            )?;
                            Ok(Cow::Owned(lit))
                        } else {
                            Err(InterpreterError::Runtime {
                                message: "Cannot perform arithmetic on non-numbers".to_string(),
                                token: Some(op.to_owned().clone()),
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
                            (left_eval.as_ref(), right_eval.as_ref())
                        {
                            let lit = self.evaluate_comparison(
                                &op.token_type,
                                *left_val,
                                *right_val,
                                op.line,
                            )?;
                            Ok(Cow::Owned(lit))
                        } else {
                            Err(InterpreterError::Runtime {
                                message: "Cannot perform comparison on non-numbers".to_string(),
                                token: Some(op.to_owned().clone()),
                                line: op.line,
                                hint: "Ensure both operands are numbers".to_string(),
                            })
                        }
                    }
                    TokenType::BangEqual | TokenType::EqualEqual => {
                        let lit = self.evaluate_equality(
                            &op.token_type,
                            &left_eval,
                            &right_eval,
                            op.line,
                        )?;
                        Ok(Cow::Owned(lit))
                    }
                    _ => Err(InterpreterError::Runtime {
                        message: "Unsupported operator".to_string(),
                        token: Some(op.to_owned().clone()),
                        line: op.line,
                        hint: "Check the operator and try again".to_string(),
                    }),
                }
            }
            Expr::Variable(token) => {
                let var = self.env.get(&token)?;
                Ok(Cow::Borrowed(var))
            }
        }
    }

    fn evaluate_unary(
        &self,
        op: &TokenType,
        right: &Literal,
        line: usize,
    ) -> Result<Literal, InterpreterError> {
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
            TokenType::Pow => Ok(Literal::NUMBER(left.powf(right))),
            TokenType::SLASH => {
                if right == 0.0 {
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
            _ => Err(InterpreterError::Runtime {
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
}
