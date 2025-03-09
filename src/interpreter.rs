use crate::{parser::Expr, scanner::Literal};

pub struct Interpreter {}

impl Interpreter {
    pub fn interpret(&self, expr: Expr) -> Literal {
        return expr.evaluate();
    }
}
