pub use crate::ast::{
    Action, Comparison, Expression as Exp, FileType, FormatElement, FormatField, FormatSpecial,
    GlobalOption, Operator as Ope, PermCheck, Permission, PositionalOption, Size, Test, TimeSpec,
};
pub use crate::RunOptions;
pub use winnow::{
    ascii::{alpha1, digit1, multispace0, multispace1},
    combinator::{
        alt, cut_err, delimited, eof, fail, preceded, repeat, repeat_till, separated,
        separated_pair, terminated,
    },
    error::{StrContext, StrContextValue},
    prelude::*,
    token::{any, literal, one_of, take_until, take_while},
};

macro_rules! parse_string_stream {
    () => {
        alt((
            delimited("\"", take_until(0.., "\""), "\""),
            delimited("'", take_until(0.., "'"), "'"),
            take_while(0.., |c| c != ' ' && c != '\n' && c != ')'),
        ))
    };
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
            .context(StrContext::Expected(StrContextValue::Description(
                "unsigned_integer",
            )))
            .parse_next(input)
    }
}

impl Parseable for u64 {
    fn parse(input: &mut &str) -> PResult<u64> {
        digit1
            .try_map(|digit_str: &str| digit_str.parse::<u64>())
            .context(StrContext::Expected(StrContextValue::Description(
                "unsigned_integer",
            )))
            .parse_next(input)
    }
}

impl Parseable for String {
    fn parse(input: &mut &str) -> PResult<String> {
        parse_string_stream!()
            .context(StrContext::Expected(StrContextValue::Description("string")))
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
