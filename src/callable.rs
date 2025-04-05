use std::rc::Rc;

use crate::{
    environment::Environment,
    error::InterpreterError,
    interpreter::Interpreter,
    rc_cell,
    stmt::FunctionDeclaration,
    token::{Literal, RcMutLiteral},
};

pub type CallArgs = Vec<RcMutLiteral>;
pub type CallReturn = Result<RcMutLiteral, InterpreterError>;

pub trait Callable {
    fn do_call(&mut self, interpreter: &Interpreter, args: CallArgs) -> CallReturn;

    fn arity(&self) -> usize;
}

struct BuiltinFunction<F>
where
    F: FnMut(CallArgs) -> CallReturn,
{
    arity: usize,
    callable: F,
}

impl<F> Callable for BuiltinFunction<F>
where
    F: FnMut(CallArgs) -> CallReturn,
{
    fn do_call(&mut self, _interpreter: &Interpreter, args: CallArgs) -> CallReturn {
        let func = &mut self.callable;
        func(args)
    }

    fn arity(&self) -> usize {
        self.arity
    }
}

struct DeclaredFunction<'a> {
    pub declaration: FunctionDeclaration<'a>,
}

impl<'a> Callable for DeclaredFunction<'a> {
    fn do_call(&mut self, interpreter: &Interpreter, mut args: CallArgs) -> CallReturn {
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
            env.define(&param, Some(arg_value));
        }

        let body = &*self.declaration.body;
        Interpreter::interpret(&[body], rc_cell!(env));
        Ok(rc_cell!(Literal::NIL))
    }

    fn arity(&self) -> usize {
        self.declaration.arity
    }
}
