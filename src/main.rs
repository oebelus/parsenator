use crate::parser::{any_char, between, char, digits, Parser};

mod parser;

#[cfg(test)]
mod tests {
    mod sep_by;
}

fn main() {
    let parser = between(any_char(), char(&'('), char(&')'));

    println!("{:?}", parser.parse("(a)"));
}
