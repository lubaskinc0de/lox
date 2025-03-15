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
    pub fn interpret(&mut self, program: &'a [Stmt]) -> Result<(), InterpreterError> {
        Ok(for stmt in program {
            self.execute(stmt)?;
        })
    }
    fn execute(&mut self, stmt: &'a Stmt) -> Result<(), InterpreterError> {
        match stmt {
            Stmt::Expression(expr) => self.evaluate(expr).map(|_| {}),
            Stmt::Print(expr) => {
                let evaluated = self.evaluate(expr)?;
                Ok(Interpreter::execute_print(evaluated.as_ref()))
            }
            Stmt::VarDeclaration { expr, name } => self.execute_var(expr, name),
        }?;
        Ok(())
    }
    fn execute_print(val: &Literal) {
        match val {
            Literal::IDENTIFIER(val) => println!("{}", val),
            Literal::STRING(val) => println!("{}", val),
            Literal::NUMBER(val) => println!("{}", val),
            Literal::BOOL(val) => println!("{}", val),
            Literal::NIL => println!("nil"),
        }
    }
    fn execute_var(
        &mut self,
        expr: &'a Option<Expr>,
        name: &Token,
    ) -> Result<(), InterpreterError> {
        let mut right: Option<Literal> = None;

        if let Some(t_expr) = expr {
            let evaluated = self.evaluate(t_expr)?;
            right = Some(evaluated.into_owned());
        }

        let identifier = name.expect_identifier()?;
        self.env.define(identifier, right)?;

        Ok(())
    }

    fn evaluate<'b>(&'b mut self, expr: &'a Expr) -> Result<Cow<'b, Literal>, InterpreterError> {
        match expr {
            Expr::Literal(val) => Ok(Cow::Borrowed(val)),
            Expr::Grouping(expr) => self.evaluate(&expr),
            Expr::Unary { right, op } => {
                let evaluated = self.evaluate(&right)?;
                let lit = Interpreter::evaluate_unary(&op.token_type, evaluated.as_ref(), op.line)?;
                Ok(Cow::Owned(lit))
            }
            Expr::Binary { left, op, right } => match op.token_type {
                TokenType::MINUS
                | TokenType::PLUS
                | TokenType::STAR
                | TokenType::SLASH
                | TokenType::Pow => {
                    let rhs = self.evaluate(&right)?;
                    let l;
                    let r;

                    if let Literal::NUMBER(right_val) = rhs.as_ref() {
                        r = *right_val;
                    } else {
                        return Err(InterpreterError::Runtime {
                            message: "Cannot perform arithmetic on non-numbers".to_string(),
                            token: Some(op.to_owned().clone()),
                            line: op.line,
                            hint: "Ensure both operands are numbers".to_string(),
                        });
                    }

                    let lhs = self.evaluate(&left)?;
                    if let Literal::NUMBER(left_val) = lhs.as_ref() {
                        l = *left_val;
                    } else {
                        return Err(InterpreterError::Runtime {
                            message: "Cannot perform arithmetic on non-numbers".to_string(),
                            token: Some(op.to_owned().clone()),
                            line: op.line,
                            hint: "Ensure both operands are numbers".to_string(),
                        });
                    }

                    let lit = Interpreter::evaluate_arithmetic(&op.token_type, &l, &r, op.line)?;
                    Ok(Cow::Owned(lit))
                }
                TokenType::LESS
                | TokenType::GREATER
                | TokenType::LessEqual
                | TokenType::GreaterEqual => {
                    let rhs = self.evaluate(&right)?;
                    let l;
                    let r;

                    if let Literal::NUMBER(right_val) = rhs.as_ref() {
                        r = *right_val;
                    } else {
                        return Err(InterpreterError::Runtime {
                            message: "Cannot perform arithmetic on non-numbers".to_string(),
                            token: Some(op.to_owned().clone()),
                            line: op.line,
                            hint: "Ensure both operands are numbers".to_string(),
                        });
                    }

                    let lhs = self.evaluate(&left)?;
                    if let Literal::NUMBER(left_val) = lhs.as_ref() {
                        l = *left_val;
                    } else {
                        return Err(InterpreterError::Runtime {
                            message: "Cannot perform arithmetic on non-numbers".to_string(),
                            token: Some(op.to_owned().clone()),
                            line: op.line,
                            hint: "Ensure both operands are numbers".to_string(),
                        });
                    }

                    let lit = Interpreter::evaluate_comparison(
                        &op.token_type,
                        &l,
                        &r,
                        op.line,
                    )?;
                    Ok(Cow::Owned(lit))
                }
                TokenType::BangEqual | TokenType::EqualEqual => {
                    let rhs = self.evaluate(&right)?.into_owned();
                    let lhs = self.evaluate(&left)?;
                    let lit = Interpreter::evaluate_equality(
                        &op.token_type,
                        lhs.as_ref(),
                        &rhs,
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
            },
            Expr::VarRead(token) => {
                let var = self.env.get(&token)?;
                Ok(Cow::Borrowed(var))
            }
            Expr::Assign { name, value } => {
                let evaluated = self.evaluate(&value)?;
                let scalar = Some(evaluated.into_owned());
                let ret = self.env.assign(name, scalar)?;
                Ok(Cow::Owned(ret.clone()))
            }
        }
    }

    fn evaluate_unary(
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

    fn evaluate_comparison(
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

    fn evaluate_equality(
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
