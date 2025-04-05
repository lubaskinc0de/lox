use std::{fmt::Debug, rc::Rc};

use crate::{callable::LoxCallable, token::RcMutLiteral};

#[derive(Debug)]
pub enum Object<'a> {
    Literal(RcMutLiteral),
    Function(Box<dyn LoxCallable<'a> + 'a>),
}

impl<'a> Object<'a> {
    pub fn extract_literal(&self) -> RcMutLiteral {
        match &self {
            Object::Literal(literal) => Rc::clone(&literal),
            Object::Function(_) => panic!("Expected literal"),
        }
    }
}
