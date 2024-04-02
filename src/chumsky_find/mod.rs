#![allow(dead_code)]
#![allow(unused_imports)]

use crate::ast::{Action, Comparison, Expression, GlobalOption, Operator, PositionalOption, Test};
use chumsky::{prelude::*, BoxStream, Flat};
use std::ops::Range;

fn parse_comp() -> impl Parser<char, Comparison, Error = Simple<char>> {
    let gt = one_of("+")
        .ignore_then(text::int(10))
        .map(|s: String| Comparison::GreaterThan(s.parse().unwrap()));

    let lt = one_of("-")
        .ignore_then(text::int(10))
        .map(|s: String| Comparison::LesserThan(s.parse().unwrap()));

    let eq = text::int(10).map(|s: String| Comparison::Equal(s.parse().unwrap()));

    gt.or(lt).or(eq)
}

fn parse_global_option() -> impl Parser<char, GlobalOption, Error = Simple<char>> {
    let depth = just('-')
        .ignore_then(text::keyword("depth"))
        .to(GlobalOption::Depth);

    let mindepth = just('-')
        .ignore_then(text::keyword("mindepth"))
        .ignore_then(text::whitespace())
        .ignore_then(text::int(10))
        .map(|d: String| GlobalOption::MinDepth(d.parse().unwrap()));

    let maxdepth = just('-')
        .ignore_then(text::keyword("maxdepth"))
        .ignore_then(text::whitespace())
        .ignore_then(text::int(10))
        .map(|d: String| GlobalOption::MaxDepth(d.parse().unwrap()));

    depth.or(mindepth).or(maxdepth)
}

fn parse_test() -> impl Parser<char, Test, Error = Simple<char>> {
    let true_ = just('-').ignore_then(text::keyword("true")).to(Test::True);
    let false_ = text::keyword("-false").to(Test::False);

    let name = text::keyword("-name")
        .ignore_then(text::ident())
        .map(|d: String| Test::Name(d));

    true_.or(false_).or(name)
}

//fn parse() -> impl Parser<char, Expression, Error = Simple<char>> {}

#[test]
fn test_parse_comparison() -> Result<(), Box<dyn std::error::Error>> {
    assert_eq!(
        parse_comp().parse("+33").unwrap(),
        Comparison::GreaterThan(33)
    );

    assert_eq!(
        parse_comp().parse("-10").unwrap(),
        Comparison::LesserThan(10)
    );

    assert_eq!(parse_comp().parse("999").unwrap(), Comparison::Equal(999));

    Ok(())
}

#[test]
fn test_parse_global_option() -> Result<(), Box<dyn std::error::Error>> {
    assert_eq!(
        parse_global_option().parse("-depth").unwrap(),
        GlobalOption::Depth
    );

    assert_eq!(
        parse_global_option().parse("-mindepth 40").unwrap(),
        GlobalOption::MinDepth(40u32)
    );

    assert_eq!(
        parse_global_option().parse("-maxdepth 40").unwrap(),
        GlobalOption::MaxDepth(40u32)
    );

    assert_eq!(parse_global_option().parse("-maxdepth -40").is_err(), true);
    assert_eq!(parse_global_option().parse("-maxdepth").is_err(), true);

    Ok(())
}

#[test]
fn test_parse_test() -> Result<(), Box<dyn std::error::Error>> {
    assert_eq!(parse_test().parse("-true").unwrap(), Test::True);

    Ok(())
}
