use std::borrow::Cow;

use crate::{
    environment::Environment,
    error::InterpreterError,
    operator::{calc, cmp, eq, unary},
    parser::Expr,
    scanner::{Literal, Token, TokenType},
    stmt::Stmt,
};

pub struct Interpreter {}

impl<'a> Interpreter {
    pub fn interpret(
        &'a mut self,
        program: &'a [Stmt],
        globals: &'a mut Environment,
    ) -> Result<(), InterpreterError> {
        Ok(for stmt in program {
            self.execute_statement(stmt, globals)?;
        })
    }

    fn execute_statement(
        &mut self,
        stmt: &'a Stmt,
        env: &'a mut Environment,
    ) -> Result<(), InterpreterError> {
        match stmt {
            Stmt::Expression(expr) => Interpreter::eval(expr, env).map(|_| {}),
            Stmt::Print(expr) => {
                let evaluated = Interpreter::eval(expr, env)?;
                Ok(Interpreter::print(evaluated.as_ref()))
            }
            Stmt::VarDeclaration { expr, name } => self.declare_variable(expr, name, env),
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
        env: &'a mut Environment,
    ) -> Result<(), InterpreterError> {
        let mut right: Option<Literal> = None;

        if let Some(t_expr) = expr {
            let evaluated = Interpreter::eval(t_expr, env)?;
            right = Some(evaluated.into_owned());
        }

        let identifier = name.expect_identifier()?;
        env.define(identifier, right)?;

        Ok(())
    }

    fn eval(
        expr: &'a Expr,
        env: &'a mut Environment,
    ) -> Result<Cow<'a, Literal>, InterpreterError> {
        match expr {
            Expr::Literal(val) => Ok(Cow::Borrowed(val)),
            Expr::Grouping(expr) => Interpreter::eval(&expr, env),
            Expr::Unary { right, op } => {
                let evaluated = Interpreter::eval(&right, env)?;
                let lit = unary(&op.token_type, evaluated.as_ref(), op.line)?;
                Ok(Cow::Owned(lit))
            }
            Expr::Binary { left, op, right } => match op.token_type {
                TokenType::MINUS
                | TokenType::PLUS
                | TokenType::STAR
                | TokenType::SLASH
                | TokenType::Pow => {
                    let rhs = Interpreter::eval(&right, env)?;
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

                    let lhs = Interpreter::eval(&left, env)?;
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
                    let rhs = Interpreter::eval(&right, env)?;
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

                    let lhs = Interpreter::eval(&left, env)?;
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
                    let rhs = Interpreter::eval(&right, env)?.into_owned();
                    let lhs = Interpreter::eval(&left, env)?;
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
                let var = env.get(&token)?;
                Ok(Cow::Borrowed(var))
            }
            Expr::Assign { name, value } => {
                let evaluated = Interpreter::eval(&value, env)?;
                let scalar = Some(evaluated.into_owned());
                let ret = env.assign(name, scalar)?;
                Ok(Cow::Owned(ret.clone()))
            }
        }
    }
}
