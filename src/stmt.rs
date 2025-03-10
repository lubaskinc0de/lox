use crate::{parser::Expr, scanner::Token};

#[derive(Debug)]
pub enum Stmt {
    Expression(Expr),
    Print(Expr),
    Var { expr: Option<Expr>, name: Token },
}
