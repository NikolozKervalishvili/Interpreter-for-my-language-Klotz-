mod lexer;
use lexer::*;

mod utils;
use utils::*;

mod parser;
use parser::*;

use std::{env, fs};

fn main() {
    let s = fs::read_to_string("src/input.txt").expect("file not found");
    let mut lexer = Lexer::new(&s);
    dbg!(&lexer.lex());
    // dbg!(lexer);
}
