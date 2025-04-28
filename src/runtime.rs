use std::{fs::File, io::Read};

use crate::{
    environment::Environment,
    error::InterpreterError,
    interpreter::Interpreter,
    parser::Parser,
    rc_cell,
    scanner::Scanner,
    stmt::Stmt,
};

fn read_file_to_string(file_name: &str) -> String {
    let mut buf = String::new();
    File::open(file_name)
        .expect("File not found")
        .read_to_string(&mut buf)
        .unwrap();
    buf
}

#[allow(dead_code)]
pub struct Runtime();

impl Runtime {
    #[allow(dead_code)]
    pub fn repl(&mut self) {}

    pub fn run(&self, file_name: &str) {
        let line = read_file_to_string(file_name);
        let parser = Runtime::create_parser(&line).unwrap();
        let statements = parser.parse().unwrap();
        let globals = rc_cell!(Environment::new(None));
        let interpreter = Interpreter { globals };
        let refs_stmts: Vec<&Stmt> = statements.iter().map(|x| x).collect();
        interpreter.run(&refs_stmts).unwrap();
    }

    fn create_parser(line: &String) -> Result<Parser, InterpreterError> {
        let mut scanner = Scanner::new(line);
        let tokens = scanner.scan_tokens()?;

        let parser = Parser::new(tokens);
        Ok(parser)
    }
}
