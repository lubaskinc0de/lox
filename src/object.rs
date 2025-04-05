use std::{cell::RefCell, fmt::Debug, rc::Rc};

use crate::{callable::LoxCallable, token::RcMutLiteral};

pub type LoxFn<'a> = Rc<RefCell<dyn LoxCallable<'a> + 'a>>;

#[derive(Debug)]
#[allow(dead_code)]
pub enum Object<'a> {
    Literal(RcMutLiteral),
    Function(LoxFn<'a>),
}

impl<'a> Object<'a> {
    pub fn extract_literal(&self) -> RcMutLiteral {
        match &self {
            Object::Literal(literal) => Rc::clone(&literal),
            _ => panic!("Expected literal"),
        }
    }

    pub fn extract_fn(&self) -> LoxFn<'a> {
        match &self {
            Object::Function(fun) => Rc::clone(&fun),
            _ => panic!("Expected function"),
        }
    }
}
