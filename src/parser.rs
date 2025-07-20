use core::panic;
use std::fmt::Debug;

pub type Parsed<'a, T> = Result<(&'a str, T), ParseError>; // Parser Result, T is the Output

pub trait Parser<'a, T> {
    fn parse(&self, input: &'a str) -> Parsed<'a, T>;
}

impl<'a, F, T> Parser<'a, T> for F
where
    F: Fn(&'a str) -> Parsed<'a, T>,
{
    fn parse(&self, input: &'a str) -> Parsed<'a, T> {
        self(input)
    }
}

impl<'a, T> Parser<'a, T> for Box<dyn Parser<'a, T> + 'a> {
    fn parse(&self, input: &'a str) -> Parsed<'a, T> {
        self.as_ref().parse(input)
    }
}

pub enum Either<A, B> {
    Left(A),
    Right(B),
}

#[derive(Debug, PartialEq)]
pub enum ParseError {
    Message(String),
    Expected(String),
    Unexpected(String),
    EOF,
}

#[derive(Debug, PartialEq)]
pub enum Types<'a> {
    Str(&'a str),
    String(String),
    TypesVec(Vec<Types<'a>>),
    Bool(bool),
    Unit(()),
}

impl<'a> FromIterator<char> for Types<'a> {
    fn from_iter<T: IntoIterator<Item = char>>(iter: T) -> Self {
        Types::String(iter.into_iter().collect())
    }
}

impl<'a> FromIterator<&'a str> for Types<'a> {
    fn from_iter<T: IntoIterator<Item = &'a str>>(iter: T) -> Self {
        Types::String(iter.into_iter().collect())
    }
}

impl<'a> FromIterator<Types<'a>> for Types<'a> {
    fn from_iter<T: IntoIterator<Item = Types<'a>>>(iter: T) -> Self {
        let mut iter = iter.into_iter();
        match iter.next() {
            Some(Types::Str(s)) => {
                let mut string = s.to_string();
                for item in iter {
                    if let Types::Str(s) = item {
                        string.push_str(s);
                    } else {
                        panic!("Inconsistent Types in iterator.");
                    }
                }
                Types::String(string)
            }
            Some(Types::String(s)) => {
                let mut string = s;
                for item in iter {
                    if let Types::String(s) = item {
                        string.push_str(&s);
                    } else {
                        panic!("Inconsistent Types in iterator.");
                    }
                }
                Types::String(string)
            }
            Some(Types::TypesVec(v)) => {
                let mut vec = v;
                for item in iter {
                    if let Types::TypesVec(v) = item {
                        vec.extend(v);
                    } else {
                        panic!("Inconsistent Types in iterator.")
                    }
                }
                Types::TypesVec(vec)
            }

            Some(Types::Bool(_)) => panic!("Cannot collect bools."),

            Some(Types::Unit(_)) => panic!("Cannot collect units."),
            None => Types::Unit(()),
        }
    }
}

/* BASIC COMBINATORS */

// SPECIFIC MATCHERS
pub fn char<'a>(expected: char) -> Box<dyn Parser<'a, Types<'a>> + 'a> {
    Box::new(move |input: &'a str| match input.chars().nth(0).unwrap() {
        next if next == expected => Ok((&input[1..], Types::Unit(()))),
        _ => Err(ParseError::Expected(format!(
            "Expected the character '{}' from the input '{}'",
            expected, input,
        ))),
    })
}

pub fn literal<'a>(expected: &'a str) -> Box<dyn Parser<'a, Types<'a>> + 'a> {
    Box::new(move |input: &'a str| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], Types::Unit(()))),
        _ => Err(ParseError::Expected(format!(
            "Expected the literal '{}' from the input '{}'",
            expected, input,
        ))),
    })
}

pub fn starts_with<'a>(prefix: &'a str) -> Box<dyn Parser<'a, Types<'a>> + 'a> {
    Box::new(move |input: &'a str| match input.starts_with(prefix) {
        true => Ok((input, Types::Bool(true))),
        false => Err(ParseError::Expected(format!(
            "Expected the input '{}' to start with \"{prefix}\".",
            input
        ))),
    })
}

pub fn ends_with<'a>(suffix: &'a str) -> Box<dyn Parser<'a, Types<'a>> + 'a> {
    Box::new(move |input: &'a str| match input.ends_with(suffix) {
        true => Ok((input, Types::Bool(true))),
        false => Err(ParseError::Expected(format!(
            "Expected the input '{}' to end with \"{suffix}\".",
            input
        ))),
    })
}

// GENERAL MATCHERS
pub fn any_char<'a>() -> Box<dyn Parser<'a, Types<'a>> + 'a> {
    Box::new(move |input: &'a str| match input.chars().next() {
        Some(next) => Ok((&input[1..], Types::String(next.to_string()))),
        _ => Err(ParseError::Expected(format!(
            "Expected a character from input '{}'",
            input,
        ))),
    })
}

pub fn digit<'a>() -> Box<dyn Parser<'a, Types<'a>> + 'static> {
    Box::new(move |input: &'a str| match input.chars().next() {
        Some(next) => {
            if next.is_numeric() {
                Ok((&input[1..], Types::String(next.to_string())))
            } else {
                Err(ParseError::Expected(format!(
                    "Expected a digit from input '{}', but got '{}' instead",
                    input, next
                )))
            }
        }
        _ => Err(ParseError::Expected(format!(
            "Expected a digit from input '{}'",
            input,
        ))),
    })
}

pub fn digits<'a>() -> Box<dyn Parser<'a, Types<'a>> + 'a> {
    Box::new(map(one_or_more(digit()), |chars: Vec<Types<'a>>| {
        chars.into_iter().collect()
    }))
}

pub fn letter<'a>() -> Box<dyn Parser<'a, Types<'a>> + 'static> {
    Box::new(move |input: &'a str| match input.chars().next() {
        Some(next) => {
            if next.is_alphabetic() {
                Ok((&input[1..], Types::String(next.to_string())))
            } else {
                Err(ParseError::Expected(format!(
                    "Expected a letter from input '{}', but got '{}' instead",
                    input, next
                )))
            }
        }
        _ => Err(ParseError::Expected(format!(
            "Expected a letter from input '{}'",
            input,
        ))),
    })
}

pub fn word<'a>() -> Box<dyn Parser<'a, Types<'a>> + 'a> {
    Box::new(map(one_or_more(letter()), |chars| {
        chars.into_iter().collect()
    }))
}

//xpected struct `Box<dyn Parser<'_, T>>`
pub fn alpha_num<'a>() -> Box<dyn Parser<'a, Types<'a>> + 'a> {
    Box::new(move |input: &'a str| match input.chars().next() {
        Some(next) => {
            if next.is_alphanumeric() {
                Ok((&input[1..], Types::String(next.to_string())))
            } else {
                Err(ParseError::Expected(format!(
                    "Expected an alphanumeric from input '{}', but got '{}' instead.",
                    input, next
                )))
            }
        }
        _ => Err(ParseError::Expected(format!(
            "Expected an alphanumeric from input {}",
            input,
        ))),
    })
}

pub fn alpha_num_word<'a>() -> Box<dyn Parser<'a, Types<'a>> + 'a> {
    Box::new(map(one_or_more(letter()), |chars| {
        chars.into_iter().collect()
    }))
}

pub fn whitespace<'a>() -> Box<dyn Parser<'a, Types<'a>> + 'static> {
    Box::new(move |input: &'a str| match input.chars().next() {
        Some(next) => {
            if next == '\r' || next == ' ' || next == '\t' || next == '\n' {
                Ok((&input[1..], Types::String(next.to_string())))
            } else {
                Err(ParseError::Expected(format!(
                    "Expected a whitespace from input '{}', but got '{}' instead.",
                    input, next
                )))
            }
        }
        _ => Err(ParseError::Expected(format!(
            "Expected a whitespace from input '{}'.",
            input
        ))),
    })
}

pub fn spaces<'a>() -> Box<dyn Parser<'a, Types<'a>> + 'a> {
    Box::new(map(one_or_more(whitespace()), |chars| {
        chars.into_iter().collect()
    }))
}

pub fn end_of_input<'a>() -> impl Parser<'a, Types<'a>> {
    move |input: &'a str| {
        if input.is_empty() {
            Ok(("", Types::Unit(())))
        } else {
            Err(ParseError::Expected(format!(
                "Expected an end of input, but got '{}' instead.",
                input
            )))
        }
    }
}

/* LOGICAL COMBINATORS */

pub fn and<'a, A, B, C, D>(parser_a: A, parser_b: B) -> impl Parser<'a, (C, D)>
where
    A: Parser<'a, C>,
    B: Parser<'a, D>,
{
    move |input| {
        parser_a.parse(input).and_then(|(next_input, result_a)| {
            parser_b
                .parse(next_input)
                .map(|(final_input, result_b)| (final_input, (result_a, result_b)))
        })
    }
}

pub fn or<'a, P1, P2, A, B>(parser_a: P1, parser_b: P2) -> impl Parser<'a, Either<A, B>>
where
    P1: Parser<'a, A>,
    P2: Parser<'a, B>,
{
    move |input| match parser_a.parse(input) {
        Ok((next_input, result)) => Ok((next_input, Either::Left(result))),
        Err(_) => match parser_b.parse(input) {
            Ok((next_input, result)) => {
                println!("space af");
                Ok((next_input, Either::Right(result)))
            }
            Err(e) => Err(e),
        },
    }
}

pub fn zero_or_more<'a, P>(parser: P) -> impl Parser<'a, Types<'a>>
where
    P: Parser<'a, Types<'a>>,
{
    move |mut input| {
        let mut result = Vec::new();

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, Types::TypesVec(result)))
    }
}

pub fn one_or_more<'a, A: 'a>(parser: Box<dyn Parser<'a, A>>) -> Box<dyn Parser<'a, Vec<A>> + 'a> {
    Box::new(move |mut input| {
        let mut result = Vec::new();

        if let Ok((next_input, first_item)) = parser.parse(input) {
            input = next_input;
            result.push(first_item);
        } else {
            return Err(ParseError::Message(format!(
                "A parser failed parsing the input '{}'",
                input
            )));
        }

        while let Ok((next_input, next_item)) = parser.parse(input) {
            input = next_input;
            result.push(next_item);
        }

        Ok((input, result))
    })
}

pub fn optional<'a, P, A>(parser: P) -> impl Parser<'a, Option<A>>
where
    P: Parser<'a, A>,
{
    move |input| match parser.parse(input) {
        Ok((rest, result)) => Ok((rest, Some(result))),
        Err(_) => Ok((input, None)),
    }
}

pub fn sep_by<'a, P>(parser: P, delimiter: &'a str) -> impl Parser<'a, Vec<String>>
where
    P: Parser<'a, String>,
{
    move |input: &'a str| {
        let mut result: Vec<String> = vec![];
        let mut remaining = input;

        match parser.parse(remaining) {
            Ok((next, res)) => {
                remaining = next;
                result.push(res);

                loop {
                    if let Ok((next, _)) = spaces().parse(remaining) {
                        remaining = next;
                    }

                    if let Ok((next, found_delimiter)) =
                        look_ahead::<P>(0, delimiter.len(), delimiter).parse(remaining)
                    {
                        if found_delimiter == delimiter {
                            remaining = &next[delimiter.len()..];
                        } else {
                            println!("Value {} found in the input '{}' is not matching the provided delimiter {}", found_delimiter, remaining, delimiter);
                        }
                    } else {
                        match parser.parse(remaining) {
                            Ok((next, res)) => {
                                result.push(res);
                                remaining = next;
                            }
                            Err(_) => {
                                break;
                            }
                        };
                    }
                }
                Ok((remaining, result))
            }
            Err(_) => Err(ParseError::Unexpected(format!(
                "Failed to parse the input '{}'.",
                input
            ))),
        }
    }
}

pub fn between<'a, A: 'a, B: 'a, C: 'a>(
    left: Box<dyn Parser<'a, B> + 'a>,
    parser: Box<dyn Parser<'a, A> + 'a>,
    right: Box<dyn Parser<'a, C> + 'a>,
) -> impl Parser<'a, A> + 'a {
    move |input| {
        let (input, _) = left.parse(input)?;
        let (input, result) = parser.parse(input)?;
        let (input, _) = right.parse(input)?;
        Ok((input, result))
    }
}

/* TRANSFORMATION */

pub fn map<'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, B>
where
    P: Parser<'a, A>,
    F: Fn(A) -> B,
{
    move |input| {
        parser
            .parse(input)
            .map(|(next_input, result)| (next_input, map_fn(result)))
    }
}

pub fn sequence<'a, P>(parsers: Vec<P>) -> impl Parser<'a, Types<'a>>
where
    P: Parser<'a, Types<'a>>,
{
    move |input| {
        let mut results: Vec<Types> = Vec::new();
        let mut mut_input = input;

        for parser in &parsers {
            if let Ok((next_input, result)) = parser.parse(mut_input) {
                mut_input = next_input;
                results.push(result);
            } else {
                return Err(ParseError::Message(format!(
                    "A parser failed parsing the input '{}'",
                    input
                )));
            }
        }

        Ok((mut_input, Types::TypesVec(results)))
    }
}

pub fn choice<'a, T: 'a>(parsers: Vec<Box<dyn Parser<'a, T> + 'a>>) -> Box<dyn Parser<'a, T> + 'a> {
    Box::new(move |input| {
        for parser in &parsers {
            if let Ok(result) = parser.parse(input) {
                return Ok(result);
            }
        }

        Err(ParseError::Unexpected(format!("anyt {input}")))
    })
}

pub fn left<'a, A, B, C, D>(parser_a: A, parser_b: B) -> impl Parser<'a, C>
where
    A: Parser<'a, C>,
    B: Parser<'a, D>,
{
    map(and(parser_a, parser_b), |(left, _right)| left)
}

pub fn right<'a, A, B, C, D>(parser_a: A, parser_b: B) -> impl Parser<'a, D>
where
    A: Parser<'a, C>,
    B: Parser<'a, D>,
{
    map(and(parser_a, parser_b), |(_left, right)| right)
}

pub fn look_ahead<'a, P>(
    position: usize,
    range: usize,
    delimiter: &'a str,
) -> impl Parser<'a, &'a str> {
    move |input: &'a str| {
        let length = input.len();

        if length <= position as usize {
            Err(ParseError::Unexpected(format!(
                "The position provided ('{}') is greater than the length ('{}') of the input '{}'.",
                position, length, input
            )))
        } else if length <= position + range {
            Err(ParseError::Unexpected(format!(
                "The sum of the position and the range ('{}') is greater than the length ('{}') of the input '{}'.",
                position + range, length, input
            )))
        } else {
            let delim = &input[position..range];

            if delimiter == delim {
                Ok((input, &input[position..range]))
            } else {
                Err(ParseError::Unexpected(format!(
                    "The provided delimiter '{}' doesn't match the found value '{}' in the input '{}'.",
                    delimiter, delim, input
                )))
            }
        }
    }
}

pub fn not_followed_by<'a>(value: &'a str) -> impl Parser<'a, bool> {
    move |input: &'a str| match input.get(1..value.len()) {
        Some(following) => {
            if following == value {
                Err(ParseError::Unexpected(format!(
                    "Unexpected value '{}' after '{}'",
                    following,
                    input.chars().next().unwrap()
                )))
            } else {
                Ok((input, true))
            }
        }
        None => Ok((input, true)),
    }
}

/* ERROR HANDLING */
pub fn fail<'a>(message: &'a str) -> impl Parser<'a, ParseError> {
    move |_input: &'a str| Err(ParseError::Message(message.to_string()))
}

/* UTILITY PARSERS */
pub fn identifier<'a>(input: &str) -> Parsed<String> {
    let mut matched = String::new();
    let mut chars = input.chars();

    match chars.next() {
        Some(next) if next.is_alphabetic() => matched.push(next),
        _ => {
            return Err(ParseError::Expected(format!(
                "Expected and identifier from the input '{}'.",
                input
            )))
        }
    }

    while let Some(next) = chars.next() {
        if next.is_alphanumeric() || next == '-' {
            matched.push(next);
        } else {
            break;
        }
    }

    let next_index = matched.len();
    Ok((&input[next_index..], matched))
}
