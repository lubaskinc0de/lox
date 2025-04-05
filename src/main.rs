mod callable;
mod environment;
mod error;
mod expr;
mod helper;
mod interpreter;
mod object;
mod operator;
mod parser;
mod scanner;
mod stmt;
mod token;

use std::{
    cell::RefCell,
    fs::File,
    io::{self, Read},
    rc::Rc,
};

use clap::Parser as CliParser;
use environment::{Environment, RcMutEnv};
use interpreter::Interpreter;
use parser::Parser;
use scanner::Scanner;
use stmt::Stmt;

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
    let env = Environment::new(None);
    run(&source, Rc::new(RefCell::new(env)), true);
}

#[allow(dead_code)]
fn run_line(source: String) {
    let env = Environment::new(None);
    run(&source, Rc::new(RefCell::new(env)), true);
}

fn run(line: &str, env: RcMutEnv, do_panic: bool) {
    let mut scanner = Scanner::new(line);

    let tokens = match scanner.scan_tokens() {
        Ok(v) => v,
        Err(e) => {
            if do_panic {
                panic!("{}", e)
            } else {
                println!("{}", e);
                return;
            }
        }
    };

    let parser = Parser::new(&tokens);
    let statements = match parser.parse() {
        Ok(v) => v,
        Err(e) => {
            if do_panic {
                panic!("{}", e)
            } else {
                println!("{}", e);
                return;
            }
        }
    };
    let statements_refs: Vec<&Stmt> = statements.iter().map(|x| x).collect();

    let interpreter = Interpreter {
        globals: Rc::clone(&env),
    };

    match interpreter.run(&statements_refs) {
        Ok(_) => {}
        Err(e) => {
            if do_panic {
                panic!("{}", e)
            } else {
                println!("{}", e);
            }
        }
    };
}

fn run_prompt() {
    println!("RLox REPL:");
    let mut input = String::new();
    let env = Rc::new(RefCell::new(Environment::new(None)));
    loop {
        input.clear();
        eprint!("> ");

        io::stdin().read_line(&mut input).unwrap();
        input = input.trim().to_string();

        if input == "exit" {
            break;
        }

        let env_clone = Rc::clone(&env);
        run(&input, env_clone, false);
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
