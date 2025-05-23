use std::borrow::Cow;

use crate::token::{Literal, Token};

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Expr<'a> {
    Binary {
        left: Box<Expr<'a>>,
        op: Cow<'a, Token>,
        right: Box<Expr<'a>>,
    },
    Logical {
        left: Box<Expr<'a>>,
        op: &'a Token,
        right: Box<Expr<'a>>,
    },
    Unary {
        right: Box<Expr<'a>>,
        op: &'a Token,
    },
    Literal(Literal),
    Grouping(Box<Expr<'a>>),
    VarRead(&'a Token),
    Assign {
        name: &'a Token,
        value: Box<Expr<'a>>,
    },
    Call {
        calee: Box<Expr<'a>>,
        paren: &'a Token,
        args: Vec<Box<Expr<'a>>>,
    },
}
