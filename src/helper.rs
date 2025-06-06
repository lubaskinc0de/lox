use crate::{
    error::InterpreterError,
    token::{Literal, RcMutLiteral, RcMutObject},
};

#[macro_export]
macro_rules! rc_cell {
    ($value:expr) => {
        ::std::rc::Rc::new(::std::cell::RefCell::new($value))
    };
}

pub type VoidResult = Result<(), InterpreterError>;
pub type RcMutLitResult = Result<RcMutLiteral, InterpreterError>;
pub type RcMutObjectResult<'a> = Result<RcMutObject<'a>, InterpreterError>;
pub type LitResult = Result<Literal, InterpreterError>;
