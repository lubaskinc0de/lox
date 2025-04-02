use crate::token::RcMutLiteral;

pub trait Callable {
    fn do_call(&self, args: &[RcMutLiteral]) -> RcMutLiteral;

    fn arity(&self) -> usize;
}
