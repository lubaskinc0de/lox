use std::collections::HashMap;
use std::rc::Rc;
use std::{borrow::Cow, cell::RefCell};

use crate::error::InterpreterError;
use crate::token::{Literal, Token};

#[derive(Debug, Clone)]
pub struct Environment {
    values: HashMap<String, Literal>,
    outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new(outer: Option<Rc<RefCell<Environment>>>) -> Self {
        Self {
            values: HashMap::new(),
            outer,
        }
    }

    pub fn define(&mut self, name: &Token, value: Option<Literal>) -> Result<(), InterpreterError> {
        let id = name.expect_identifier()?;
        if self.values.contains_key(id.as_str()) {
            return Err(InterpreterError::Runtime {
                message: "Variable already declared".to_string(),
                token: Some(name.clone()),
                line: name.line,
                hint: "You are trying to declare a var that already declared".to_string(),
            });
        }
        self.values.insert(id, value.unwrap_or(Literal::NIL));
        Ok(())
    }

    pub fn get(&self, name: &Token) -> Result<Cow<'_, Literal>, InterpreterError> {
        let var_name = name.expect_identifier()?;
        if let Some(val) = self.values.get(&var_name) {
            return Ok(Cow::Borrowed(val));
        }
        if let Some(outer) = &self.outer {
            let val = outer.borrow().get(name)?.into_owned();
            return Ok(Cow::Owned(val));
        }
        Err(InterpreterError::Runtime {
            message: "Variable is not defined".to_string(),
            token: Some(name.clone()),
            line: name.line,
            hint: "Check your declarations".to_string(),
        })
    }

    pub fn assign(
        &mut self,
        name: &Token,
        value: Option<Literal>,
    ) -> Result<Literal, InterpreterError> {
        let id = name.expect_identifier()?;

        if self.values.contains_key(&id) {
            let prev = self.get(name)?.into_owned();
            self.values.insert(id, value.unwrap_or(Literal::NIL));
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
