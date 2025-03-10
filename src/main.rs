use std::{
    collections::HashMap,
    fs::File,
    io::{self, Read},
};

use crate::error::InterpreterError;
use clap::Parser as CliParser;
use environment::Environment;
use interpreter::Interpreter;
use parser::Parser;
use scanner::Scanner;
mod environment;
mod error;
mod interpreter;
mod parser;
mod scanner;
mod stmt;

fn read_file_to_string(file_name: &str) -> String {
    let mut buf = String::new();
    File::open(file_name)
        .expect("File not found")
        .read_to_string(&mut buf)
        .unwrap();
    buf
}

fn run_file(file_name: &str) {
    let source: String = read_file_to_string(&file_name);
    let mut env = Environment {
        values: HashMap::new(),
    };
    execute_panic(&source, &mut env);
}

fn run(line: &str, env: &mut Environment) -> Result<(), InterpreterError> {
    let mut scanner = Scanner::new(line);
    let mut interpreter = Interpreter { env };
    let tokens = scanner.scan_tokens()?;

    let mut parser = Parser::new(tokens.to_vec());
    let statements = parser.parse()?;

    interpreter.interpret(&statements)?;
    Ok(())
}

fn execute_safe(line: &str, env: &mut Environment) {
    match run(line, env) {
        Ok(_s) => {}
        Err(e) => println!("{}", e),
    }
}

fn execute_panic(line: &str, env: &mut Environment) {
    match run(line, env) {
        Ok(_s) => {}
        Err(e) => panic!("{}", e),
    }
}

fn run_prompt() {
    println!("RLox REPL:");
    let mut input = String::new();
    let mut env = Environment {
        values: HashMap::new(),
    };
    loop {
        input.clear();
        eprint!("> ");

        io::stdin().read_line(&mut input).unwrap();
        input = input.trim().to_string();

        if input == "exit" {
            break;
        }

        execute_safe(&input, &mut env);
    }
}

#[derive(CliParser, Debug)]
#[command(
    version,
    about = "RLox language",
    long_about = "lubaskinc0de's Lox language implementation on Rust"
)]
struct CliArgs {
    #[arg(short, long, default_value_t = true)]
    repl: bool,
    file_name: Option<String>,
}

fn main() {
    let cli = CliArgs::parse();
    let file_name = cli.file_name;

    if cli.repl && file_name.is_none() {
        run_prompt();
        return;
    }

    match file_name {
        Some(f_name) => run_file(&f_name),
        None => panic!("Error: You must provide file name."),
    }
}
