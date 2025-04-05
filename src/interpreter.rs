use std::{cell::RefCell, rc::Rc};

use crate::{
    callable::{CallArgs, Callable},
    environment::{Environment, RcMutEnv},
    error::InterpreterError,
    expr::Expr,
    helper::{RcMutLitResult, VoidResult},
    operator::{calc, cmp, eq, logical, unary},
    rc_cell,
    stmt::Stmt,
    token::{Literal, RcMutLiteral, Token, TokenType},
};

pub struct Interpreter {
    pub globals: RcMutEnv,
}

impl Interpreter {
    pub fn run(&self, program: &[&Stmt]) -> VoidResult {
        Interpreter::interpret(program, Rc::clone(&self.globals))
    }

    #[allow(dead_code)]
    fn populate_builtins(&self) {
        let _credits_func = |_: CallArgs| rc_cell!(Literal::STRING(String::from("lubaskinc0de")));
        let _credits_func_token = Token {
            token_type: TokenType::IDENTIFIER,
            lexeme: String::from("credits()"),
            literal: None,
            line: 0,
        };
    }

    pub fn interpret(code: &[&Stmt], env: RcMutEnv) -> VoidResult {
        for stmt in code {
            Interpreter::execute_statement(stmt, Rc::clone(&env))?;
        }
        Ok(())
    }

    fn execute_statement(stmt: &Stmt, env: RcMutEnv) -> VoidResult {
        match stmt {
            Stmt::Expression(expr) => Interpreter::eval(expr, Rc::clone(&env)).map(|_| {}),
            Stmt::Print(expr) => {
                let evaluated = Interpreter::eval(expr, Rc::clone(&env))?;
                Ok(Interpreter::print(&evaluated.borrow()))
            }
            Stmt::VarDeclaration { expr, name } => {
                Interpreter::declare_variable(expr, name, Rc::clone(&env))
            }
            Stmt::Block(code) => {
                let block: Vec<&Stmt> = code.iter().map(|x| x).collect();
                Interpreter::block(&block, env)?;
                Ok(())
            }
            Stmt::If { cond, then, else_ } => {
                let e_stmt = (*else_).as_deref();
                Interpreter::if_(cond, then, e_stmt, env)?;
                Ok(())
            }
            Stmt::While { cond, body } => {
                Interpreter::while_(cond, body, env)?;
                Ok(())
            }
            Stmt::Function(_decl) => todo!(),
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

    fn declare_variable(expr: &Option<Expr>, name: &Token, env: RcMutEnv) -> VoidResult {
        let mut right: Option<RcMutLiteral> = None;

        if let Some(t_expr) = expr {
            let evaluated = Interpreter::eval(t_expr, Rc::clone(&env))?;
            right = Some(evaluated);
        }

        env.borrow_mut().define(name, right)?;
        Ok(())
    }

    fn block(code: &[&Stmt], outer: RcMutEnv) -> VoidResult {
        let env = Environment::new(Some(Rc::clone(&outer)));
        Interpreter::interpret(code, rc_cell!(env))?;
        Ok(())
    }

    fn if_(cond: &Expr, then: &Stmt, else_: Option<&Stmt>, env: RcMutEnv) -> VoidResult {
        {
            let cond_eval = Interpreter::eval(cond, Rc::clone(&env))?;
            if cond_eval.borrow().is_truthy() {
                Interpreter::execute_statement(then, Rc::clone(&env))?;
            } else if let Some(else_stmt) = else_ {
                Interpreter::execute_statement(else_stmt, Rc::clone(&env))?;
            }
        }
        Ok(())
    }

    fn while_(cond: &Expr, body: &Stmt, env: RcMutEnv) -> VoidResult {
        loop {
            let cond_eval = { Interpreter::eval(cond, Rc::clone(&env))? };

            if !cond_eval.borrow().is_truthy() {
                break;
            }
            Interpreter::execute_statement(body, Rc::clone(&env))?;
        }
        Ok(())
    }

    #[allow(dead_code)]
    fn do_call(
        &self,
        mut calee: Box<dyn Callable>,
        paren: &Token,
        args: &[Box<Expr>],
        env: RcMutEnv,
    ) -> RcMutLitResult {
        let mut args_evaluated: Vec<RcMutLiteral> = Vec::new();

        for each in args {
            let evaluated = Interpreter::eval(&each, Rc::clone(&env))?;
            args_evaluated.push(evaluated);
        }

        if args.len() != calee.arity() {
            return Err(InterpreterError::Runtime {
                message: format!(
                    "Expected {} arguments but got {}",
                    calee.arity(),
                    args.len()
                ),
                token: Some(paren.clone()),
                line: paren.line,
                hint: "Check arguments you pass".to_string(),
            });
        }
        Ok(calee.do_call(&self, args_evaluated)?)
    }

    fn eval<'a>(expr: &'a Expr, env: RcMutEnv) -> RcMutLitResult {
        match expr {
            Expr::Literal(val) => Ok(rc_cell!(val.clone())),
            Expr::Grouping(expr) => Interpreter::eval(&*expr, env),
            Expr::Unary { right, op } => {
                let evaluated = Interpreter::eval(&*right, env)?;
                let lit = unary(&op.token_type, &evaluated.borrow(), op.line)?;
                Ok(lit)
            }
            Expr::Binary { left, op, right } => match op.token_type {
                TokenType::PLUS => {
                    let lhs = Interpreter::eval(&*left, Rc::clone(&env))?;
                    let l_borrow = lhs.borrow();
                    let lhs_str = format!("{:?}", l_borrow);
                    let rhs = Interpreter::eval(&*right, env)?;
                    let r_borrow = rhs.borrow();

                    let res = match (&*l_borrow, &*r_borrow) {
                        (Literal::NUMBER(l), Literal::NUMBER(r)) => Some(Rc::new(RefCell::new(
                            calc(&op.token_type, &l, &r, op.line)?,
                        ))),
                        (Literal::STRING(l), Literal::STRING(r)) => {
                            Some(Rc::new(RefCell::new(Literal::STRING(l.to_owned() + r))))
                        }
                        _ => None,
                    };

                    match res {
                        Some(result) => Ok(result),
                        None => Err(InterpreterError::Runtime {
                            message: format!("Cannot add {} with {}", lhs_str, r_borrow),
                            token: Some(op.clone().into_owned()),
                            line: op.line,
                            hint: "Ensure both operands are numbers".to_string(),
                        }),
                    }
                }
                TokenType::MINUS | TokenType::STAR | TokenType::SLASH | TokenType::Pow => {
                    let lhs = Interpreter::eval(&*left, Rc::clone(&env))?;
                    let bwed_l = lhs.borrow();
                    let left =
                        bwed_l
                            .extract_number()
                            .ok_or_else(|| InterpreterError::Runtime {
                                message: "Cannot perform arithmetic on non-numbers".to_string(),
                                token: Some(op.clone().into_owned()),
                                line: op.line,
                                hint: "Ensure both operands are numbers".to_string(),
                            })?;

                    let rhs = Interpreter::eval(&*right, env)?;
                    let bwed_r = rhs.borrow();
                    let right =
                        bwed_r
                            .extract_number()
                            .ok_or_else(|| InterpreterError::Runtime {
                                message: "Cannot perform arithmetic on non-numbers".to_string(),
                                token: Some(op.clone().into_owned()),
                                line: op.line,
                                hint: "Ensure both operands are numbers".to_string(),
                            })?;

                    Ok(Rc::new(RefCell::new(calc(
                        &op.token_type,
                        &left,
                        right,
                        op.line,
                    )?)))
                }
                TokenType::BangEqual | TokenType::EqualEqual => {
                    let rhs = Interpreter::eval(&*right, Rc::clone(&env))?;
                    let lhs = Interpreter::eval(&*left, env)?;
                    let lit = eq(&op.token_type, &lhs.borrow(), &rhs.borrow(), op.line)?;
                    Ok(lit)
                }
                TokenType::LESS
                | TokenType::GREATER
                | TokenType::LessEqual
                | TokenType::GreaterEqual => {
                    let rhs = Interpreter::eval(&*right, Rc::clone(&env))?;
                    let l;
                    let r;

                    if let Literal::NUMBER(right_val) = &*rhs.borrow() {
                        r = *right_val;
                    } else {
                        return Err(InterpreterError::Runtime {
                            message: "Cannot perform arithmetic on non-numbers".to_string(),
                            token: Some(op.clone().into_owned()),
                            line: op.line,
                            hint: "Ensure both operands are numbers".to_string(),
                        });
                    }

                    let lhs = Interpreter::eval(&*left, env)?;
                    if let Literal::NUMBER(left_val) = &*lhs.borrow() {
                        l = *left_val;
                    } else {
                        return Err(InterpreterError::Runtime {
                            message: "Cannot perform arithmetic on non-numbers".to_string(),
                            token: Some(op.clone().into_owned()),
                            line: op.line,
                            hint: "Ensure both operands are numbers".to_string(),
                        });
                    }

                    let lit = cmp(&op.token_type, &l, &r, op.line)?;
                    Ok(rc_cell!(lit))
                }
                _ => Err(InterpreterError::Runtime {
                    message: "Unsupported operator".to_string(),
                    token: Some(op.clone().into_owned()),
                    line: op.line,
                    hint: "Check the operator and try again".to_string(),
                }),
            },
            Expr::Logical { left, op, right } => {
                let left_lit = Interpreter::eval(&*left, Rc::clone(&env))?;
                let right_lit = Interpreter::eval(&*right, env)?;
                let lit = logical(left_lit, right_lit, op)?;
                Ok(lit)
            }
            Expr::VarRead(token) => {
                let var = env.borrow().get(&token)?;
                Ok(var)
            }
            Expr::Assign { name, value } => {
                let evaluated = Interpreter::eval(&*value, Rc::clone(&env))?;
                let scalar = Some(evaluated);
                let ret = env.borrow_mut().assign(name, scalar)?;
                Ok(ret)
            }
            Expr::Call {
                calee: _,
                paren: _,
                args: _,
            } => todo!(),
        }
    }
}
