use std::collections::HashMap;

use crate::{
    error::InterpreterError,
    scanner::{Literal, Token},
};

#[derive(Debug, Clone)]
pub struct Environment<'a> {
    pub values: HashMap<String, Literal>,
    pub enclosing: Option<&'a Environment<'a>>,
}

impl<'a> Environment<'a> {
    pub fn define(&mut self, name: String, value: Option<Literal>) -> Result<(), InterpreterError> {
        self.values
            .insert(name, value.or_else(|| Some(Literal::NIL)).unwrap());
        Ok(())
    }

    pub fn get(&'a self, name: &Token) -> Result<&'a Literal, InterpreterError> {
        let var_name = name.expect_identifier()?;

        self.values
            .get(var_name.as_str())
            .ok_or(InterpreterError::Runtime {
                message: "Variable is not defined".to_string(),
                token: Some(name.clone()),
                line: name.line,
                hint: "Check your declarations".to_string(),
            })
    }

    pub fn assign(
        &'a mut self,
        name: &Token,
        value: Option<Literal>,
    ) -> Result<&'a Literal, InterpreterError> {
        let id = name.expect_identifier()?;
        if !self.values.contains_key(id.as_str()) {
            return Err(InterpreterError::Runtime {
                message: "Variable is not defined".to_string(),
                token: Some(name.clone()),
                line: name.line,
                hint: "Check your declarations".to_string(),
            });
        }
        self.define(id, value)?;
        Ok(self.get(name)?)
    }
}
