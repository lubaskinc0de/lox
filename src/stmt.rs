use crate::parser::Expr;

#[derive(Debug)]
pub enum Stmt {
    Expression(Expr),
    Print(Expr),
}
