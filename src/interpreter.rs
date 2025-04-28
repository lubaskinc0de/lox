use std::{collections::VecDeque, rc::Rc};

use crate::{
    callable::{CallArgs, DeclaredFunction},
    environment::{Environment, RcMutEnv},
    error::InterpreterError,
    expr::Expr,
    helper::{RcMutObjectResult, VoidResult},
    object::{LoxFn, Object},
    operator::{calc, cmp, eq, logical, unary},
    rc_cell,
    stmt::{FunctionDeclaration, Stmt},
    token::{Literal, RcMutObject, Token, TokenType},
};

pub struct Interpreter<'b> {
    pub globals: RcMutEnv<'b>,
}

impl<'a, 'b> Interpreter<'b>
where
    'a: 'b,
{
    pub fn run(&self, program: &[&'a Stmt]) -> VoidResult {
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

    pub fn interpret(code: &[&'a Stmt], env: RcMutEnv<'a>) -> VoidResult {
        for stmt in code {
            Interpreter::execute_statement(stmt, Rc::clone(&env))?;
        }
        Ok(())
    }

    fn execute_statement(stmt: &'a Stmt, env: RcMutEnv<'a>) -> VoidResult {
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
            Stmt::Function(decl) => Interpreter::function_declaration(decl, env),
        }?;
        Ok(())
    }

    fn print(val: &Object) {
        match val {
            Object::Literal(lit) => match &*lit.borrow() {
                Literal::IDENTIFIER(val) => println!("{}", val),
                Literal::STRING(val) => println!("{}", val),
                Literal::NUMBER(val) => println!("{}", val),
                Literal::BOOL(val) => println!("{}", val),
                Literal::NIL => println!("nil"),
            },
            Object::Function(_) => println!("function object"),
        }
    }

    fn declare_variable(expr: &'a Option<Expr>, name: &Token, env: RcMutEnv<'a>) -> VoidResult {
        let mut right: Option<RcMutObject> = None;

        if let Some(t_expr) = expr {
            let evaluated = Interpreter::eval(t_expr, Rc::clone(&env))?;
            right = Some(evaluated);
        }

        env.borrow_mut().define(name, right)?;
        Ok(())
    }

    fn block(code: &[&'a Stmt], outer: RcMutEnv<'a>) -> VoidResult {
        let env = Environment::new(Some(Rc::clone(&outer)));
        Interpreter::interpret(code, rc_cell!(env))?;
        Ok(())
    }

    fn if_(
        cond: &'a Expr,
        then: &'a Stmt,
        else_: Option<&'a Stmt>,
        env: RcMutEnv<'a>,
    ) -> VoidResult {
        {
            let cond_eval = Interpreter::eval(cond, Rc::clone(&env))?;
            if cond_eval.borrow().extract_literal().borrow().is_truthy() {
                Interpreter::execute_statement(then, Rc::clone(&env))?;
            } else if let Some(else_stmt) = else_ {
                Interpreter::execute_statement(else_stmt, Rc::clone(&env))?;
            }
        }
        Ok(())
    }

    fn while_(cond: &'a Expr, body: &'a Stmt, env: RcMutEnv<'a>) -> VoidResult {
        loop {
            let cond_eval = { Interpreter::eval(cond, Rc::clone(&env))? };

            if !cond_eval.borrow().extract_literal().borrow().is_truthy() {
                break;
            }
            Interpreter::execute_statement(body, Rc::clone(&env))?;
        }
        Ok(())
    }

    #[allow(dead_code)]
    fn do_call(
        calee: LoxFn<'a>,
        paren: &Token,
        args: &'a [Box<Expr<'a>>],
        env: RcMutEnv<'a>,
    ) -> RcMutObjectResult<'a> {
        let mut args_evaluated: VecDeque<RcMutObject> = VecDeque::new();

        for each in args {
            let evaluated = Interpreter::eval(&each, Rc::clone(&env))?;
            args_evaluated.push_back(evaluated);
        }

        if args.len() != calee.borrow().arity() {
            return Err(InterpreterError::Runtime {
                message: format!(
                    "Expected {} arguments but got {}",
                    calee.borrow().arity(),
                    args.len()
                ),
                token: Some(paren.clone()),
                line: paren.line,
                hint: "Check arguments you pass".to_string(),
            });
        }
        Ok(calee.borrow_mut().do_call(env, args_evaluated)?)
    }

    fn function_declaration(decl: &'a FunctionDeclaration, env: RcMutEnv<'a>) -> VoidResult {
        let func = DeclaredFunction { declaration: decl };
        let obj = Object::Function(rc_cell!(func));
        env.borrow_mut().define(decl.name, Some(rc_cell!(obj)))?;
        Ok(())
    }

    fn eval(expr: &'a Expr, env: RcMutEnv<'a>) -> RcMutObjectResult<'a> {
        match expr {
            Expr::Literal(val) => Ok(rc_cell!(Object::Literal(rc_cell!(val.clone())))),
            Expr::Grouping(expr) => Interpreter::eval(&*expr, env),
            Expr::Unary { right, op } => {
                let evaluated = Interpreter::eval(&*right, env)?;
                let lit = unary(
                    &op.token_type,
                    &evaluated.borrow().extract_literal().borrow(),
                    op.line,
                )?;
                Ok(rc_cell!(Object::Literal(lit)))
            }
            Expr::Binary { left, op, right } => match op.token_type {
                TokenType::PLUS => {
                    let lhs = Interpreter::eval(&*left, Rc::clone(&env))?;
                    let lhs_lit = lhs.borrow().extract_literal();

                    let l_borrow = lhs_lit.borrow();
                    let lhs_str = format!("{:?}", l_borrow);
                    let rhs = Interpreter::eval(&*right, env)?;
                    let rhs_lit = rhs.borrow().extract_literal();

                    let res = match (&*lhs_lit.borrow(), &*rhs_lit.borrow()) {
                        (Literal::NUMBER(l), Literal::NUMBER(r)) => {
                            Some(calc(&op.token_type, &l, &r, op.line)?)
                        }
                        (Literal::STRING(l), Literal::STRING(r)) => {
                            Some(Literal::STRING(l.to_owned() + &r))
                        }
                        _ => None,
                    };

                    match res {
                        Some(result) => Ok(rc_cell!(Object::Literal(rc_cell!(result)))),
                        None => Err(InterpreterError::Runtime {
                            message: format!("Cannot add {} with {}", lhs_str, rhs_lit.borrow()),
                            token: Some(op.clone().into_owned()),
                            line: op.line,
                            hint: "Ensure both operands are numbers".to_string(),
                        }),
                    }
                }
                TokenType::MINUS | TokenType::STAR | TokenType::SLASH | TokenType::Pow => {
                    let lhs = Interpreter::eval(&*left, Rc::clone(&env))?;
                    let literal_l = lhs.borrow().extract_literal();
                    let literal_l_ref = literal_l.borrow();

                    let left = literal_l_ref.extract_number().ok_or_else(|| {
                        InterpreterError::Runtime {
                            message: "Cannot perform arithmetic on non-numbers".to_string(),
                            token: Some(op.clone().into_owned()),
                            line: op.line,
                            hint: "Ensure both operands are numbers".to_string(),
                        }
                    })?;

                    let rhs = Interpreter::eval(&*right, env)?;
                    let literal_r = rhs.borrow().extract_literal();
                    let literal_r_ref = literal_r.borrow();

                    let right = literal_r_ref.extract_number().ok_or_else(|| {
                        InterpreterError::Runtime {
                            message: "Cannot perform arithmetic on non-numbers".to_string(),
                            token: Some(op.clone().into_owned()),
                            line: op.line,
                            hint: "Ensure both operands are numbers".to_string(),
                        }
                    })?;
                    let arihm_res = calc(&op.token_type, &left, right, op.line)?;
                    Ok(rc_cell!(Object::Literal(rc_cell!(arihm_res))))
                }
                TokenType::BangEqual | TokenType::EqualEqual => {
                    let rhs = Interpreter::eval(&*right, Rc::clone(&env))?;
                    let lhs = Interpreter::eval(&*left, env)?;
                    let lit = eq(
                        &op.token_type,
                        &lhs.borrow().extract_literal().borrow(),
                        &rhs.borrow().extract_literal().borrow(),
                        op.line,
                    )?;
                    Ok(rc_cell!(Object::Literal(rc_cell!(lit))))
                }
                TokenType::LESS
                | TokenType::GREATER
                | TokenType::LessEqual
                | TokenType::GreaterEqual => {
                    let rhs = Interpreter::eval(&*right, Rc::clone(&env))?;
                    let l;
                    let r;

                    let r_lit = rhs.borrow().extract_literal();
                    let r_lit_ref = r_lit.borrow();

                    r = match &*r_lit_ref {
                        Literal::NUMBER(right_val) => Ok(right_val),
                        _ => Err(InterpreterError::Runtime {
                            message: "Cannot perform arithmetic on non-numbers".to_string(),
                            token: Some(op.clone().into_owned()),
                            line: op.line,
                            hint: "Ensure both operands are numbers".to_string(),
                        }),
                    }?;
                    let lhs = Interpreter::eval(&*left, env)?;
                    let l_lit = lhs.borrow().extract_literal();
                    let l_lit_ref = l_lit.borrow();

                    l = match &*l_lit_ref {
                        Literal::NUMBER(val) => Ok(val),
                        _ => Err(InterpreterError::Runtime {
                            message: "Cannot perform arithmetic on non-numbers".to_string(),
                            token: Some(op.clone().into_owned()),
                            line: op.line,
                            hint: "Ensure both operands are numbers".to_string(),
                        }),
                    }?;

                    let lit = cmp(&op.token_type, &l, &r, op.line)?;
                    Ok(rc_cell!(Object::Literal(rc_cell!(lit))))
                }
                _ => Err(InterpreterError::Runtime {
                    message: "Unsupported operator".to_string(),
                    token: Some(op.clone().into_owned()),
                    line: op.line,
                    hint: "Check the operator and try again".to_string(),
                }),
            },
            Expr::Logical { left, op, right } => {
                let left_lit = Interpreter::eval(&*left, Rc::clone(&env))?
                    .borrow()
                    .extract_literal();
                let right_lit = Interpreter::eval(&*right, env)?.borrow().extract_literal();
                let lit = logical(left_lit, right_lit, op)?;
                Ok(rc_cell!(Object::Literal(lit)))
            }
            Expr::VarRead(token) => {
                let var = env.borrow().get(&token)?;
                Ok(var.clone())
            }
            Expr::Assign { name, value } => {
                let evaluated = Interpreter::eval(&*value, Rc::clone(&env))?;
                let scalar = Some(evaluated);
                let ret = env.borrow_mut().assign(name, scalar)?;
                Ok(ret.clone())
            }
            Expr::Call { calee, paren, args } => {
                let evaluated_calee = Interpreter::eval(&calee, env.clone())?;
                let lox_fn = evaluated_calee.borrow().extract_fn();
                let result = Interpreter::do_call(lox_fn, paren, args, env)?;
                Ok(result)
            }
        }
    }
}
