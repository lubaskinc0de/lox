use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::error::InterpreterError;
use crate::helper::{RcMutLitResult, VoidResult};
use crate::rc_cell;
use crate::token::{Literal, RcMutLiteral, Token};

pub type RcMutEnv = Rc<RefCell<Environment>>;

#[derive(Debug, Clone)]
pub struct Environment {
    values: HashMap<String, RcMutLiteral>,
    outer: Option<RcMutEnv>,
}

impl Environment {
    pub fn new(outer: Option<RcMutEnv>) -> Self {
        Self {
            values: HashMap::new(),
            outer,
        }
    }

    pub fn define(&mut self, name: &Token, value: Option<RcMutLiteral>) -> VoidResult {
        let id = name.expect_identifier()?;
        if self.values.contains_key(id.as_str()) {
            return Err(InterpreterError::Runtime {
                message: "Variable already declared".to_string(),
                token: Some(name.clone()),
                line: name.line,
                hint: "You are trying to declare a var that already declared".to_string(),
            });
        }
        let val = value.unwrap_or(rc_cell!(Literal::NIL));
        self.values.insert(id, val);
        Ok(())
    }

    pub fn get(&self, name: &Token) -> RcMutLitResult {
        let var_name = name.expect_identifier()?;
        if let Some(val) = self.values.get(&var_name) {
            return Ok(Rc::clone(&val));
        }
        if let Some(outer) = &self.outer {
            let val = outer.borrow().get(name)?;
            return Ok(Rc::clone(&val));
        }
        Err(InterpreterError::Runtime {
            message: "Variable is not defined".to_string(),
            token: Some(name.clone()),
            line: name.line,
            hint: "Check your declarations".to_string(),
        })
    }

    pub fn assign(&mut self, name: &Token, value: Option<RcMutLiteral>) -> RcMutLitResult {
        let id = name.expect_identifier()?;

        if self.values.contains_key(&id) {
            let prev = self.get(name)?;
            self.values
                .insert(id, value.unwrap_or(rc_cell!(Literal::NIL)));
            return Ok(prev);
        }

        if let Some(outer) = &self.outer {
            return outer.borrow_mut().assign(name, value);
        }

        Err(InterpreterError::Runtime {
            message: "Variable is not defined".to_string(),
            token: Some(name.clone()),
            line: name.line,
            hint: "Check your declarations".to_string(),
        })
    }
}
