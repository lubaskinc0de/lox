use crate::{parser::Expr, scanner::Token};

#[derive(Debug, Clone)]
pub enum Stmt<'a> {
    Expression(Expr<'a>),
    Print(Expr<'a>),
    VarDeclaration {
        expr: Option<Expr<'a>>,
        name: &'a Token,
    },
    Block(Vec<Stmt<'a>>),
}
