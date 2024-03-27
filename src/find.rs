#![allow(unused_imports)]
use crate::ast;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::bytes::complete::take_until1;
use nom::bytes::complete::take_while1;
use nom::combinator::map;
use nom::combinator::recognize;
use nom::combinator::value;
use nom::error::Error;
use nom::multi::many1;
use nom::sequence::delimited;
use nom::sequence::preceded;
use nom::sequence::separated_pair;
use nom::sequence::terminated;
use nom::IResult;
use nom::{bytes, character};

fn parse_int(input: &str) -> IResult<&str, i32> {
    character::complete::i32::<&str, Error<&str>>(input)
}

fn parse_global_option(input: &str) -> IResult<&str, ast::GlobalOption> {
    alt((
        value(ast::GlobalOption::Depth, tag("-depth")),
        map(
            separated_pair(
                tag("-mindepth"),
                character::complete::space1,
                character::complete::u32,
            ),
            |(_, depth)| ast::GlobalOption::MinDepth(depth),
        ),
        map(
            separated_pair(
                tag("-maxdepth"),
                character::complete::space1,
                character::complete::u32,
            ),
            |(_, depth)| ast::GlobalOption::MaxDepth(depth),
        ),
    ))(input)
}

fn parse_string(input: &str) -> IResult<&str, &str> {
    alt((
        delimited(tag("'"), take_until1("'"), tag("'")),
        take_while1(|c| c != ' ' && c != '\n' && c != ')'),
    ))(input)
}

fn parse_test(input: &str) -> IResult<&str, ast::Test> {
    alt((
        map(
            separated_pair(
                tag("-amin"),
                character::complete::space1,
                character::complete::i32,
            ),
            |(_, n)| ast::Test::AccessMin(n),
        ),
        map(
            separated_pair(tag("-anewer"), character::complete::space1, parse_string),
            |(_, n)| ast::Test::AccessNewer(String::from(n)),
        ),
        map(
            separated_pair(
                tag("-atime"),
                character::complete::space1,
                character::complete::i32,
            ),
            |(_, n)| ast::Test::AccessTime(n),
        ),
    ))(input)
}

fn parse_operator(input: &str) -> IResult<&str, ast::Operator> {
    log::info!("Parsing operator !");
    alt((
        map(
            separated_pair(parse_expression, tag(" -and "), parse_expression),
            |(lhs, rhs)| ast::Operator::And(lhs, rhs),
        ),
        map(
            delimited(
                terminated(tag("("), character::complete::space0),
                parse_expression,
                preceded(character::complete::space0, tag(")")),
            ),
            ast::Operator::Precedence,
        ),
        map(
            separated_pair(
                alt((tag("!"), tag("-not"))),
                character::complete::space0,
                parse_expression,
            ),
            |(_, e)| ast::Operator::Not(e),
        ),
    ))(input)
}

pub fn parse_expression(input: &str) -> IResult<&str, ast::Expression> {
    log::info!("Parsing expression !");
    alt((
        map(parse_operator, |val| {
            let val = std::rc::Rc::new(val);
            ast::Expression::Operator(val)
        }),
        map(parse_test, ast::Expression::Test),
        map(parse_global_option, ast::Expression::Global),
    ))(input)
}

#[test]
fn test_parse_int() {
    let (_, res) = character::complete::i32::<&str, Error<&str>>("+32").unwrap();
    assert_eq!(32, res);
    let (_, res) = character::complete::i32::<&str, Error<&str>>("-24").unwrap();
    assert_eq!(-24, res);
    let (_, res) = character::complete::i32::<&str, Error<&str>>("16").unwrap();
    assert_eq!(16, res)
}

#[test]
fn test_parse_global_option() -> Result<(), Box<dyn std::error::Error>> {
    let (_, res) = parse_global_option("-depth")?;
    assert_eq!(ast::GlobalOption::Depth, res);

    let (_, res) = parse_global_option("-maxdepth 44")?;
    assert_eq!(ast::GlobalOption::MaxDepth(44), res);

    let (_, res) = parse_global_option("-mindepth 44")?;
    assert_eq!(ast::GlobalOption::MinDepth(44), res);

    let res = parse_global_option("-maxdepth -44");
    assert!(res.is_err());

    let res = parse_global_option("-mindepth -44");
    assert!(res.is_err());

    Ok(())
}

#[test]
fn test_parse_test() -> Result<(), Box<dyn std::error::Error>> {
    let (_, res) = parse_test("-amin 44")?;
    assert_eq!(ast::Test::AccessMin(44), res);

    let (_, res) = parse_test("-anewer '/path/to/file with spaces'")?;
    assert_eq!(
        ast::Test::AccessNewer(String::from("/path/to/file with spaces")),
        res
    );

    let (_, res) = parse_test("-anewer /path/to/file\n")?;
    assert_eq!(ast::Test::AccessNewer(String::from("/path/to/file")), res);

    let (_, res) = parse_test("-anewer /path/to/file")?;
    assert_eq!(ast::Test::AccessNewer(String::from("/path/to/file")), res);

    Ok(())
}

#[test]
fn test_parse_expression() -> Result<(), Box<dyn std::error::Error>> {
    let (_, res) = parse_expression("-amin 44")?;
    assert_eq!(ast::Expression::Test(ast::Test::AccessMin(44)), res);

    let (_, res) = parse_expression("! -amin 44")?;
    assert_eq!(
        ast::Expression::Operator(std::rc::Rc::new(ast::Operator::Not(ast::Expression::Test(
            ast::Test::AccessMin(44)
        )))),
        res
    );

    Ok(())
}
