use crate::token::RcMutLiteral;

pub trait Callable {
    fn do_call(&mut self, args: &[RcMutLiteral]) -> RcMutLiteral;

    fn arity(&self) -> usize;
}

struct Function<F>
where
    F: FnMut(&[RcMutLiteral]) -> RcMutLiteral,
{
    arity: usize,
    callable: F,
}

impl<F> Callable for Function<F>
where
    F: FnMut(&[RcMutLiteral]) -> RcMutLiteral,
{
    fn do_call(&mut self, args: &[RcMutLiteral]) -> RcMutLiteral {
        let func = &mut self.callable;
        func(args)
    }

    fn arity(&self) -> usize {
        self.arity
    }
}
