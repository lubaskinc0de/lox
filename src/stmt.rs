use crate::{expr::Expr, token::Token};

#[allow(dead_code)]
#[derive(Debug)]
pub struct FunctionDeclaration<'a> {
    pub name: &'a Token,
    pub params: Vec<&'a Token>,
    pub body: Box<Stmt<'a>>,
    pub arity: usize,
}

#[derive(Debug)]
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
    Function(FunctionDeclaration<'a>),
}
