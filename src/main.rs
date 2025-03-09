use std::{
    fs::File,
    io::{self, Read},
};

use clap::Parser as CliParser;
use error::ErrorStack;
use interpreter::Interpreter;
use parser::Parser;
use scanner::Scanner;
mod error;
mod interpreter;
mod parser;
mod scanner;

fn read_file_to_string(file_name: &str) -> String {
    let mut buf = String::new();
    File::open(file_name)
        .expect("File not found")
        .read_to_string(&mut buf)
        .unwrap();
    println!("{}", buf);
    buf
}

fn run_file(file_name: &str) {
    let source: String = read_file_to_string(&file_name);
    execute_panic(&source);
}

fn run(line: &str) -> Result<(), ErrorStack> {
    let mut scanner = Scanner::new(line);
    let interpreter = Interpreter {};
    let tokens = scanner.scan_tokens()?;

    for token in tokens {
        println!("{}", token)
    }
    let mut parser = Parser::new(tokens.to_vec());
    let expr = parser.parse()?;

    let evaluated = interpreter.interpret(expr);
    println!("Evaluated: {:#?}", evaluated);
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
