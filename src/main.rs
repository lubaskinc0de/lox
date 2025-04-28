mod callable;
mod environment;
mod error;
mod expr;
mod helper;
mod interpreter;
mod object;
mod operator;
mod parser;
mod runtime;
mod scanner;
mod stmt;
mod token;

use clap::Parser as CliParser;
use runtime::Runtime;

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
        panic!("REPL is not supported yet.");
    }

    let runtime = Runtime();

    match file_name {
        Some(f_name) => runtime.run(&f_name),
        None => panic!("Error: You must provide file name."),
    }
}
