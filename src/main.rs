use std::{
    env,
    fs::File,
    io::{self, Read},
};

use error::ErrorStack;
use parser::Parser;
use scanner::Scanner;
mod error;
mod parser;
mod scanner;

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
    execute_panic(&source);
}

fn run(line: &str) -> Result<(), ErrorStack> {
    let mut scanner = Scanner::new(line);
    let tokens = scanner.scan_tokens()?;

    for token in tokens {
        println!("{}", token)
    }
    let mut parser = Parser::new(tokens.to_vec());
    let expr = parser.parse()?;

    println!("{:?}", expr);
    Ok(())
}

fn execute_safe(line: &str) {
    match run(line) {
        Ok(_s) => {}
        Err(e) => println!("{}", e),
    }
}

fn execute_panic(line: &str) {
    match run(line) {
        Ok(_s) => {}
        Err(e) => panic!("{}", e),
    }
}

fn run_prompt() {
    println!("RLox REPL:");
    let mut input = String::new();
    loop {
        input.clear();
        eprint!("> ");

        io::stdin().read_line(&mut input).unwrap();
        input = input.trim().to_string();

        if input == "exit" {
            break;
        }

        execute_safe(&input);
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_name = args.get(1);

    match file_name {
        Some(f_name) => run_file(f_name),
        None => run_prompt(),
    }
}
