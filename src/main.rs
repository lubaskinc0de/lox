use std::{
    cell::RefCell,
    fs::File,
    io::{self, Read},
    rc::Rc,
};

use clap::Parser as CliParser;
use environment::Environment;
use interpreter::Interpreter;
use parser::Parser;
use scanner::Scanner;
mod environment;
mod error;
mod interpreter;
mod operator;
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
    let env = Environment::new(None);
    run(&source, Rc::new(RefCell::new(env)), true);
}

fn run(line: &str, mut env: Rc<RefCell<Environment>>, do_panic: bool) -> Rc<RefCell<Environment>> {
    let mut scanner = Scanner::new(line);

    let tokens = match scanner.scan_tokens() {
        Ok(v) => v,
        Err(e) => {
            if do_panic {
                panic!("{}", e)
            } else {
                println!("{}", e);
                return env;
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
                return env;
            }
        }
    };

    env = match Interpreter::interpret(&statements, Rc::clone(&env)) {
        Ok(v) => v,
        Err(e) => {
            if do_panic {
                panic!("{}", e)
            } else {
                println!("{}", e);
                return env;
            }
        }
    };
    env
}

fn run_prompt() {
    println!("RLox REPL:");
    let mut input = String::new();
    let mut env = Rc::new(RefCell::new(Environment::new(None)));
    loop {
        input.clear();
        eprint!("> ");

        io::stdin().read_line(&mut input).unwrap();
        input = input.trim().to_string();

        if input == "exit" {
            break;
        }

        env = run(&input, env, false);
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
