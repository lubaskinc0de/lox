use std::{fmt::Debug, rc::Rc};

use crate::{
    environment::Environment,
    error::InterpreterError,
    interpreter::Interpreter,
    object::Object,
    rc_cell,
    stmt::FunctionDeclaration,
    token::{Literal, RcMutObject},
};

pub type CallArgs<'a> = Vec<RcMutObject<'a>>;
pub type CallReturn<'a> = Result<RcMutObject<'a>, InterpreterError>;

pub trait LoxCallable<'a>: Debug {
    fn do_call(&mut self, interpreter: &Interpreter<'a>, args: CallArgs<'a>) -> CallReturn<'a>;

    fn arity(&self) -> usize;
}

#[allow(dead_code)]
#[derive(Debug)]
pub struct DeclaredFunction<'a> {
    pub declaration: &'a FunctionDeclaration<'a>,
}

impl<'a> LoxCallable<'a> for DeclaredFunction<'a> {
    fn do_call(&mut self, interpreter: &Interpreter<'a>, mut args: CallArgs<'a>) -> CallReturn<'a> {
        if args.len() != self.arity() {
            return Err(InterpreterError::Runtime {
                message: "Not enough arguments".to_string(),
                token: Some(self.declaration.name.clone()), // todo: better exception
                line: self.declaration.name.line,
                hint: "".to_string(),
            });
        }
        let mut env = Environment::new(Some(Rc::clone(&interpreter.globals)));

        for i in 0..self.declaration.params.len() {
            let param = self.declaration.params.get(i).unwrap();
            let arg_value = args.remove(i);
            env.define(&param, Some(arg_value))?;
        }

        let body = &*self.declaration.body;
        Interpreter::interpret(&[body], rc_cell!(env))?;
        Ok(rc_cell!(Object::Literal(rc_cell!(Literal::NIL))))
    }

    fn arity(&self) -> usize {
        self.declaration.arity
    }
}
