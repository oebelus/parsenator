use crate::parser::{digits, sep_by, Parser};

mod parser;

#[cfg(test)]
mod tests {
    mod sep_by;
}

fn main() {
    let parser = sep_by(digits(), ";;");

    println!("{:?}", parser.parse("1;;\t2;;\n3"));
}
