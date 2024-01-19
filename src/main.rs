#![feature(box_patterns)]
mod lexer;
mod parser;
mod prelude;
mod pretty_print;

use parser::CoreExpr;

fn main() {
    println!("Hello, world!");
    println!("{}", std::mem::size_of::<CoreExpr>());
}
