use core::panic;
use std::{
    fmt::{Debug, Display},
    ops::{Deref, Index, Range, RangeFrom},
};

pub type Parsed<'a, S, T> = Result<(&'a S, T), ParseError>; // Parser Result, T is the Output

pub trait Parser<'a, S, T> {
    fn parse(&self, input: &'a S) -> Parsed<'a, S, T>;
}

impl<'a, F, T, S: 'a> Parser<'a, S, T> for F
where
    F: Fn(&'a S) -> Parsed<'a, S, T>,
{
    fn parse(&self, input: &'a S) -> Parsed<'a, S, T> {
        self(input)
    }
}

impl<'a, T, S> Parser<'a, S, T> for Box<dyn Parser<'a, S, T> + 'a> {
    fn parse(&self, input: &'a S) -> Parsed<'a, S, T> {
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

impl<'a> ToString for Types<'a> {
    fn to_string(&self) -> String {
        match self {
            Types::Str(s) => s.to_string(),
            Types::String(s) => s.to_string(),
            Types::TypesVec(items) => items
                .iter()
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join(" "),
            Types::Bool(b) => b.to_string(),
            Types::Unit(_) => "".to_string(),
        }
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
pub fn char<'a, S>(expected: char) -> Box<dyn Parser<'a, S, Types<'a>> + 'a>
where
    S: AsRef<str> + Deref<Target = String> + 'a + Display + Index<RangeFrom<usize>, Output = S>,
{
    Box::new(move |input: &'a S| match input.chars().next() {
        next if next == Some(expected) => Ok((&input[1..], Types::Unit(()))),
        _ => Err(ParseError::Expected(format!(
            "Expected the character '{}' from the input '{}'",
            expected, input,
        ))),
    })
}

pub fn literal<'a, S>(expected: &'a str) -> Box<dyn Parser<'a, S, Types<'a>> + 'a>
where
    S: AsRef<str> + Deref<Target = String> + 'a + Display + Index<RangeFrom<usize>, Output = S>,
{
    Box::new(move |input: &'a S| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], Types::Unit(()))),
        _ => Err(ParseError::Expected(format!(
            "Expected the literal '{}' from the input '{}'",
            expected, input,
        ))),
    })
}

pub fn starts_with<'a, S>(prefix: &'a S) -> Box<dyn Parser<'a, S, Types<'a>> + 'a>
where
    S: AsRef<str> + Deref + 'a + Display + Index<RangeFrom<usize>, Output = S>,
{
    Box::new(move |input: &'a S| {
        let prefix_str = prefix.as_ref();
        match input.as_ref().starts_with(prefix_str) {
            true => Ok((&input[prefix_str.len()..], Types::Bool(true))),
            false => Err(ParseError::Expected(format!(
                "Expected the input '{}' to start with \"{}\".",
                input, prefix_str
            ))),
        }
    })
}

pub fn ends_with<'a, S>(suffix: &'a S) -> Box<dyn Parser<'a, S, Types<'a>> + 'a>
where
    S: AsRef<str> + Deref<Target = String> + 'a + Display + Index<RangeFrom<usize>, Output = S>,
{
    Box::new(move |input: &'a S| {
        let suffix_str = suffix.as_ref();
        match input.ends_with(suffix_str) {
            true => Ok((input, Types::Bool(true))),
            false => Err(ParseError::Expected(format!(
                "Expected the input '{}' to end with \"{suffix}\".",
                input
            ))),
        }
    })
}

// GENERAL MATCHERS
pub fn any_char<'a, S>() -> Box<dyn Parser<'a, S, Types<'a>> + 'a>
where
    S: AsRef<str> + Deref<Target = String> + 'a + Display + Index<RangeFrom<usize>, Output = S>,
{
    Box::new(move |input: &'a S| match input.chars().next() {
        Some(next) => Ok((&input[1..], Types::String(next.to_string()))),
        _ => Err(ParseError::Expected(format!(
            "Expected a character from input '{}'",
            input,
        ))),
    })
}

pub fn digit<'a, S>() -> Box<dyn Parser<'a, S, Types<'a>> + 'static>
where
    S: AsRef<str> + Deref<Target = String> + 'a + Display + Index<RangeFrom<usize>, Output = S>,
{
    Box::new(move |input: &'a S| match input.chars().next() {
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

pub fn digits<'a, S>() -> Box<dyn Parser<'a, S, Types<'a>> + 'a>
where
    S: AsRef<str> + Deref<Target = String> + 'a + Display + Index<RangeFrom<usize>, Output = S>,
{
    Box::new(map(one_or_more(digit()), |chars: Vec<Types<'a>>| {
        chars.into_iter().collect()
    }))
}

pub fn letter<'a, S>() -> Box<dyn Parser<'a, S, Types<'a>> + 'static>
where
    S: AsRef<str> + Deref<Target = String> + 'a + Display + Index<RangeFrom<usize>, Output = S>,
{
    Box::new(move |input: &'a S| match input.chars().next() {
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

pub fn word<'a, S>() -> Box<dyn Parser<'a, S, Types<'a>> + 'a>
where
    S: AsRef<str> + Deref<Target = String> + 'a + Display + Index<RangeFrom<usize>, Output = S>,
{
    Box::new(map(one_or_more(letter()), |chars| {
        chars.into_iter().collect()
    }))
}

pub fn alpha_num<'a, S>() -> Box<dyn Parser<'a, S, Types<'a>> + 'a>
where
    S: AsRef<str> + Deref<Target = String> + 'a + Display + Index<RangeFrom<usize>, Output = S>,
{
    Box::new(move |input: &'a S| match input.chars().next() {
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

pub fn alpha_num_word<'a, S>() -> Box<dyn Parser<'a, S, Types<'a>> + 'a>
where
    S: AsRef<str> + Deref<Target = String> + 'a + Display + Index<RangeFrom<usize>, Output = S>,
{
    Box::new(map(one_or_more(letter()), |chars| {
        chars.into_iter().collect()
    }))
}

pub fn whitespace<'a, S>() -> Box<dyn Parser<'a, S, Types<'a>> + 'static>
where
    S: AsRef<str> + Deref<Target = String> + 'a + Display + Index<RangeFrom<usize>, Output = S>,
{
    Box::new(move |input: &'a S| match input.chars().next() {
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

pub fn spaces<'a, S>() -> Box<dyn Parser<'a, S, Types<'a>> + 'a>
where
    S: AsRef<str> + Deref<Target = String> + 'a + Display + Index<RangeFrom<usize>, Output = S>,
{
    Box::new(map(one_or_more(whitespace()), |chars| {
        chars.into_iter().collect()
    }))
}

pub fn end_of_input<'a, S>() -> impl Parser<'a, S, Types<'a>>
where
    S: AsRef<str> + 'a + Display + ExactSizeIterator,
    &'a S: Default,
{
    move |input: &'a S| {
        if input.as_ref().is_empty() {
            Ok((Default::default(), Types::Unit(())))
        } else {
            Err(ParseError::Expected(format!(
                "Expected an end of input, but got '{}' instead.",
                input
            )))
        }
    }
}

/* LOGICAL COMBINATORS */

pub fn and<'a, S, A, B, C, D>(parser_a: A, parser_b: B) -> impl Parser<'a, S, (C, D)>
where
    A: Parser<'a, S, C>,
    B: Parser<'a, S, D>,
    S: 'a,
{
    move |input: &'a S| {
        parser_a.parse(input).and_then(|(next_input, result_a)| {
            parser_b
                .parse(next_input)
                .map(|(final_input, result_b)| (final_input, (result_a, result_b)))
        })
    }
}

pub fn or<'a, S, P1, P2, A, B>(parser_a: P1, parser_b: P2) -> impl Parser<'a, S, Either<A, B>>
where
    P1: Parser<'a, S, A>,
    P2: Parser<'a, S, B>,
    S: 'a,
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

pub fn zero_or_more<'a, S: 'a, P>(parser: P) -> impl Parser<'a, S, Types<'a>>
where
    P: Parser<'a, S, Types<'a>>,
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

pub fn one_or_more<'a, S, A: 'a>(
    parser: Box<dyn Parser<'a, S, A>>,
) -> Box<dyn Parser<'a, S, Vec<A>> + 'a>
where
    S: 'a + Display,
{
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

pub fn optional<'a, S: 'a, P, A>(parser: P) -> impl Parser<'a, S, Option<A>>
where
    P: Parser<'a, S, A>,
{
    move |input| match parser.parse(input) {
        Ok((rest, result)) => Ok((rest, Some(result))),
        Err(_) => Ok((input, None)),
    }
}

pub fn sep_by<'a, S, P, T>(parser: P, delimiter: &'a S) -> impl Parser<'a, S, Vec<T>>
where
    P: Parser<'a, S, T> + 'a,
    S: AsRef<str>
        + Deref<Target = String>
        + 'a
        + Display
        + Index<RangeFrom<usize>, Output = S>
        + Index<Range<usize>, Output = S>
        + PartialEq,
{
    move |input: &'a S| {
        let mut result = vec![];
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
                        look_ahead::<S, P>(0, delimiter.len(), delimiter).parse(remaining)
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

pub fn between<'a, A: 'a, B: 'a, C: 'a, S: 'a>(
    left: Box<dyn Parser<'a, S, B> + 'a>,
    parser: Box<dyn Parser<'a, S, A> + 'a>,
    right: Box<dyn Parser<'a, S, C> + 'a>,
) -> impl Parser<'a, S, A> + 'a {
    move |input| {
        let (input, _) = left.parse(input)?;
        let (input, result) = parser.parse(input)?;
        let (input, _) = right.parse(input)?;
        Ok((input, result))
    }
}

pub fn skip<'a, S: 'a, A: 'a>(
    parser: Box<dyn Parser<'a, S, A> + 'a>,
) -> Box<dyn Parser<'a, S, Types<'a>> + 'a>
where
    S: AsRef<str> + Deref + 'a + Display + Index<RangeFrom<usize>, Output = S>,
{
    Box::new(move |input| match parser.parse(input) {
        Ok((next, _result)) => Ok((next, Types::Unit(()))),
        Err(e) => Err(ParseError::Unexpected(format!(
            "Unexpected input '{}', error being : {:?}.",
            input, e
        ))),
    })
}

/* TRANSFORMATION */

pub fn map<'a, S: 'a, P, F, A, B>(parser: P, map_fn: F) -> impl Parser<'a, S, B>
where
    P: Parser<'a, S, A>,
    F: Fn(A) -> B,
{
    move |input| {
        parser
            .parse(input)
            .map(|(next_input, result)| (next_input, map_fn(result)))
    }
}

pub fn sequence<'a, S: 'a, P>(parsers: Vec<P>) -> impl Parser<'a, S, Types<'a>>
where
    P: Parser<'a, S, Types<'a>>,
    S: AsRef<str> + Deref + 'a + Display + Index<RangeFrom<usize>, Output = S>,
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

pub fn choice<'a, S: 'a + Display, T: 'a>(
    parsers: Vec<Box<dyn Parser<'a, S, T> + 'a>>,
) -> Box<dyn Parser<'a, S, T> + 'a> {
    Box::new(move |input| {
        for parser in &parsers {
            if let Ok(result) = parser.parse(input) {
                return Ok(result);
            }
        }

        Err(ParseError::Unexpected(format!("anyt {input}")))
    })
}

pub fn left<'a, S: 'a, A, B, C, D>(parser_a: A, parser_b: B) -> impl Parser<'a, S, C>
where
    A: Parser<'a, S, C>,
    B: Parser<'a, S, D>,
{
    map(and(parser_a, parser_b), |(left, _right)| left)
}

pub fn right<'a, S: 'a, A, B, C, D>(parser_a: A, parser_b: B) -> impl Parser<'a, S, D>
where
    A: Parser<'a, S, C>,
    B: Parser<'a, S, D>,
{
    map(and(parser_a, parser_b), |(_left, right)| right)
}

pub fn look_ahead<'a, S, P>(
    position: usize,
    range: usize,
    delimiter: &'a S,
) -> impl Parser<'a, S, &'a S>
where
    S: AsRef<str>
        + Deref<Target = String>
        + 'a
        + Display
        + Index<Range<usize>, Output = S>
        + PartialEq,
{
    move |input: &'a S| {
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

pub fn not_followed_by<'a, S>(value: &'a S) -> impl Parser<'a, S, bool>
where
    S: AsRef<str>
        + Deref<Target = String>
        + 'a
        + Display
        + Index<Range<usize>, Output = S>
        + PartialEq,
{
    move |input: &'a S| match input.get(1..value.len()) {
        Some(following) => {
            if following == value.as_ref() {
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
pub fn fail<'a, S>(message: &'a S) -> impl Parser<'a, S, ParseError>
where
    S: AsRef<str> + Deref + 'a + Display,
{
    move |_input: &'a S| Err(ParseError::Message(message.to_string()))
}

/* UTILITY PARSERS */
pub fn identifier<'a, S: 'a>(input: &S) -> Parsed<S, String>
where
    S: AsRef<str> + Deref<Target = String> + 'a + Display + Index<RangeFrom<usize>, Output = S>,
{
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
