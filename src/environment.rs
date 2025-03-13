use std::collections::HashMap;

use crate::{
    error::InterpreterError,
    scanner::{Literal, Token},
};

#[derive(Debug, Clone)]
pub struct Environment {
    pub values: HashMap<String, Literal>,
}

impl<'a> Environment {
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
}
