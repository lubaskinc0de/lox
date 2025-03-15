use std::borrow::Cow;

use crate::{
    environment::Environment,
    error::InterpreterError,
    operator::{calc, cmp, eq, unary},
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
            self.execute_statement(stmt)?;
        })
    }

    fn execute_statement(&mut self, stmt: &'a Stmt) -> Result<(), InterpreterError> {
        match stmt {
            Stmt::Expression(expr) => self.eval(expr).map(|_| {}),
            Stmt::Print(expr) => {
                let evaluated = self.eval(expr)?;
                Ok(Interpreter::print(evaluated.as_ref()))
            }
            Stmt::VarDeclaration { expr, name } => self.declare_variable(expr, name),
        }?;
        Ok(())
    }

    fn print(val: &Literal) {
        match val {
            Literal::IDENTIFIER(val) => println!("{}", val),
            Literal::STRING(val) => println!("{}", val),
            Literal::NUMBER(val) => println!("{}", val),
            Literal::BOOL(val) => println!("{}", val),
            Literal::NIL => println!("nil"),
        }
    }

    fn declare_variable(
        &mut self,
        expr: &'a Option<Expr>,
        name: &Token,
    ) -> Result<(), InterpreterError> {
        let mut right: Option<Literal> = None;

        if let Some(t_expr) = expr {
            let evaluated = self.eval(t_expr)?;
            right = Some(evaluated.into_owned());
        }

        let identifier = name.expect_identifier()?;
        self.env.define(identifier, right)?;

        Ok(())
    }

    fn eval<'b>(&'b mut self, expr: &'a Expr) -> Result<Cow<'b, Literal>, InterpreterError> {
        match expr {
            Expr::Literal(val) => Ok(Cow::Borrowed(val)),
            Expr::Grouping(expr) => self.eval(&expr),
            Expr::Unary { right, op } => {
                let evaluated = self.eval(&right)?;
                let lit = unary(&op.token_type, evaluated.as_ref(), op.line)?;
                Ok(Cow::Owned(lit))
            }
            Expr::Binary { left, op, right } => match op.token_type {
                TokenType::MINUS
                | TokenType::PLUS
                | TokenType::STAR
                | TokenType::SLASH
                | TokenType::Pow => {
                    let rhs = self.eval(&right)?;
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

                    let lhs = self.eval(&left)?;
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

                    let lit = calc(&op.token_type, &l, &r, op.line)?;
                    Ok(Cow::Owned(lit))
                }
                TokenType::LESS
                | TokenType::GREATER
                | TokenType::LessEqual
                | TokenType::GreaterEqual => {
                    let rhs = self.eval(&right)?;
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

                    let lhs = self.eval(&left)?;
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

                    let lit = cmp(&op.token_type, &l, &r, op.line)?;
                    Ok(Cow::Owned(lit))
                }
                TokenType::BangEqual | TokenType::EqualEqual => {
                    let rhs = self.eval(&right)?.into_owned();
                    let lhs = self.eval(&left)?;
                    let lit = eq(&op.token_type, lhs.as_ref(), &rhs, op.line)?;
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
                let evaluated = self.eval(&value)?;
                let scalar = Some(evaluated.into_owned());
                let ret = self.env.assign(name, scalar)?;
                Ok(Cow::Owned(ret.clone()))
            }
        }
    }
}
