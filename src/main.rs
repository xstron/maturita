use std::{env, fs};

mod tokenizer;
mod parser;
mod interpreter;

fn main() {
    println!("Hello, world!");

    let args = env::args().collect::<Vec<String>>();

    if args.len() < 2 {
        panic!("Usage: {} <filename>", args[0]);
    }

    let contents = fs::read_to_string(&args[1]).expect("Unable to read file");

    dbg!(&contents);

    let tokens = tokenizer::tokenize(&contents, &args[1]);

    dbg!(&tokens);

    let ast = parser::parse(tokens);

    dbg!(&ast);

    interpreter::interpret(ast);
}
