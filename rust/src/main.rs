mod lexer;
mod parser;
mod prelude;
mod pretty_print;

use parser::*;

fn main() {
    let program = "main = let
  x = 26;
  y = x * x;
  z = y * y + y * y
in x + y + z";

    let program = parse(program).unwrap();

    println!("program ast:\n{program:?}");
    println!("pretty printed:");

    for def in program {
        println!("{def}\n");
    }
}
