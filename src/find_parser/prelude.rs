pub use crate::ast::{
    Action, Comparison, Expression as Exp, FileType, FormatElement, FormatField, FormatSpecial,
    GlobalOption, Operator as Ope, PermCheck, Permission, PositionalOption, Size, Test, TimeSpec,
};
pub use crate::RunOptions;
use winnow::error::StrContext;
pub use winnow::{
    ascii::{alpha1, digit1, multispace0, multispace1},
    combinator::{
        alt, cut_err, delimited, eof, fail, preceded, repeat, repeat_till, separated,
        separated_pair, terminated,
    },
    error::{ContextError, StrContext::Label, StrContextValue},
    prelude::*,
    token::{any, literal, one_of, take_until, take_while},
};

pub fn expected(reason: &'static str) -> StrContext {
    StrContext::Expected(StrContextValue::Description(reason))
}

pub fn label(name: &'static str) -> StrContext {
    StrContext::Label(name)
}

/// Trait used to add the ability to parse arbitrary types
pub trait Parseable {
    fn parse(input: &mut &str) -> PResult<Self>
    where
        Self: Sized;
}

/// Trait used to add the ability to parse arbitrary types, with a second argument called in case a
/// default categorization is needed
pub trait DefaultParseable<S> {
    fn parse(input: &mut &str, default: impl Fn(S) -> Self) -> PResult<Self>
    where
        Self: Sized;
}

impl Parseable for u32 {
    fn parse(input: &mut &str) -> PResult<u32> {
        digit1
            .try_map(|digit_str: &str| digit_str.parse::<u32>())
            .context(expected("unsigned_integer"))
            .parse_next(input)
    }
}

impl Parseable for u64 {
    fn parse(input: &mut &str) -> PResult<u64> {
        digit1
            .try_map(|digit_str: &str| digit_str.parse::<u64>())
            .context(expected("unsigned_integer"))
            .parse_next(input)
    }
}

/// Delimit a string from the input and return it with no processing
///
/// This function is similar to the parse trait for streams of characters. This allows to chain
/// another parser on top of the output, which is impossible if we return a string.
pub fn quote_delimiter<'a>() -> impl Parser<&'a str, &'a str, winnow::error::ContextError> {
    alt((
        delimited("\"", take_until(1.., "\""), "\""),
        delimited("'", take_until(1.., "'"), "'"),
        take_while(1.., |c| c != ' ' && c != '\n' && c != ')'),
    ))
}

impl Parseable for String {
    fn parse(input: &mut &str) -> PResult<String> {
        quote_delimiter()
            .context(expected("string"))
            .map(String::from)
            .parse_next(input)
    }
}

#[test]
fn test_parse_string() -> Result<(), Box<dyn std::error::Error>> {
    let res = String::parse(&mut "a_long_string").unwrap();
    assert_eq!(String::from("a_long_string"), res);

    let res = String::parse(&mut "a_long_string\n").unwrap();
    assert_eq!(String::from("a_long_string"), res);

    let res = String::parse(&mut "a_long_string another").unwrap();
    assert_eq!(String::from("a_long_string"), res);

    let res = String::parse(&mut "'a_long_string another' again").unwrap();
    assert_eq!(String::from("a_long_string another"), res);

    Ok(())
}
