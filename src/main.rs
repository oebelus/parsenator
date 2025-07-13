use crate::parser::{digits, Parser};

mod parser;

fn main() {
    let parser = digits("17");

    println!("{:?}", parser.parse("17"));
}
