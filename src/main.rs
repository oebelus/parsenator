use crate::parser::{digits, Parser};

mod parser;

fn main() {
    let parser = digits();

    println!("{:?}", parser.parse("a17a"));
}
