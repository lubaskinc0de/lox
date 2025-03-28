use crate::{expr::Expr, token::Token};

#[derive(Debug, Clone)]
pub enum Stmt<'a> {
    Expression(Expr<'a>),
    Print(Expr<'a>),
    VarDeclaration {
        expr: Option<Expr<'a>>,
        name: &'a Token,
    },
    Block(Vec<Stmt<'a>>),
    If {
        cond: Expr<'a>,
        then: Box<Stmt<'a>>,
        else_: Option<Box<Stmt<'a>>>,
    },
    While {
        cond: Expr<'a>,
        body: Box<Stmt<'a>>,
    },
}
