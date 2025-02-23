use std::{env, fs, process::exit};

mod tokenizer;
mod parser;
mod interpreter;

fn main() {
    let args = env::args().collect::<Vec<String>>();

    if args.len() < 2 || args.iter().any(|arg| arg == "-h" || arg == "--help") {
        eprintln!("Usage: {} [-d|--debug] <filename>", args[0]);
        exit(1);
    }
    let debug = args.iter().any(|arg| arg == "-d" || arg == "--debug");
    let path = &args[args.len() - 1];

    let contents = match fs::read_to_string(path) {
        Ok(contents) => contents,
        Err(e) => {
            eprintln!("Error reading file {}: {}", path, e);
            exit(1);
        }
    };

    if debug {
        dbg!(&contents);
    }

    let tokens = tokenizer::tokenize(&contents, path);

    if debug {
        dbg!(&tokens);
    }

    let ast = parser::parse(tokens);

    if debug {
        dbg!(&ast);
    }

    interpreter::interpret(ast);
}
