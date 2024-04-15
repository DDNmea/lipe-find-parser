#![allow(dead_code)]

use crate::ast::{Action, Comparison, Expression, GlobalOption, Operator, Test};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::bytes::complete::take_until1;
use nom::bytes::complete::take_while1;
use nom::character;
use nom::character::complete::u32 as parse_u32;
use nom::combinator::complete;
use nom::combinator::map;
use nom::combinator::value;
use nom::sequence::delimited;
use nom::sequence::preceded;
use nom::sequence::separated_pair;
use nom::sequence::terminated;
use nom::sequence::tuple;
use nom::IResult;
use nom_locate::LocatedSpan;
use nom_recursive::{recursive_parser, RecursiveInfo};
use std::rc::Rc;

pub type Span<'a> = LocatedSpan<&'a str, RecursiveInfo>;

macro_rules! parse_type_into {
    ($tag:expr, $target:expr, $parser:expr) => {
        map(
            preceded(
                tuple((tag($tag), character::complete::space1)),
                complete($parser),
            ),
            $target,
        )
    };
}

fn parse_comp(input: Span) -> IResult<Span, Comparison> {
    alt((
        map(preceded(tag("+"), parse_u32), Comparison::GreaterThan),
        map(preceded(tag("-"), parse_u32), Comparison::LesserThan),
        map(parse_u32, Comparison::Equal),
    ))(input)
}

fn parse_string(input: Span) -> IResult<Span, String> {
    map(
        alt((
            delimited(tag("'"), take_until1("'"), tag("'")),
            take_while1(|c| c != ' ' && c != '\n' && c != ')'),
        )),
        |s: Span| String::from(*s.fragment()),
    )(input)
}

fn parse_global_option(input: Span) -> IResult<Span, GlobalOption> {
    alt((
        value(GlobalOption::Depth, tag("-depth")),
        parse_type_into!("-mindepth", GlobalOption::MinDepth, parse_u32),
        parse_type_into!("-maxdepth", GlobalOption::MaxDepth, parse_u32),
    ))(input)
}

fn parse_action(input: Span) -> IResult<Span, Action> {
    alt((
        parse_type_into!("-fls", Action::FileList, parse_string),
        parse_type_into!("-fprint", Action::FilePrint, parse_string),
        parse_type_into!("-fprint0", Action::FilePrintNull, parse_string),
        //parse_type_into!("-fprintf", Action::FilePrintFormatted, parse_string),
        value(Action::List, tag("-ls")),
        value(Action::Print, tag("-print")),
        value(Action::PrintNull, tag("-print0")),
        parse_type_into!("-printf", Action::PrintFormatted, parse_string),
        value(Action::Prune, tag("-prune")),
        value(Action::Quit, tag("-quit")),
    ))(input)
}

fn parse_test(input: Span) -> IResult<Span, Test> {
    alt((
        alt((
            parse_type_into!("-amin", Test::AccessMin, parse_comp),
            parse_type_into!("-anewer", Test::AccessNewer, parse_string),
            parse_type_into!("-atime", Test::AccessTime, parse_comp),
            parse_type_into!("-cmin", Test::ChangeMin, parse_comp),
            parse_type_into!("-cnewer", Test::ChangeNewer, parse_string),
            parse_type_into!("-ctime", Test::ChangeTime, parse_comp),
            value(Test::Empty, tag("-empty")),
            value(Test::Executable, tag("-executable")),
            value(Test::False, tag("-false")),
            parse_type_into!("-fstype", Test::FsType, parse_string),
            parse_type_into!("-gid", Test::GroupId, parse_comp),
            parse_type_into!("-group", Test::Group, parse_string),
            parse_type_into!("-ilname", Test::InsensitiveLinkName, parse_string),
            parse_type_into!("-iname", Test::InsensitiveName, parse_string),
            parse_type_into!("-inum", Test::InodeNumber, parse_comp),
            parse_type_into!("-ipath", Test::InsensitivePath, parse_string),
            parse_type_into!("-iregex", Test::InsensitiveRegex, parse_string),
            parse_type_into!("-links", Test::Hardlinks, parse_u32),
            parse_type_into!("-mmin", Test::ModifyMin, parse_comp),
            parse_type_into!("-mnewer", Test::ModifyNewer, parse_string),
            parse_type_into!("-mtime", Test::ModifyTime, parse_comp),
        )),
        alt((
            parse_type_into!("-name", Test::Name, parse_string),
            value(Test::NoGroup, tag("-nouser")),
            value(Test::NoUser, tag("-nogroup")),
            parse_type_into!("-path", Test::Path, parse_string),
            parse_type_into!("-perm", Test::Perm, parse_string),
            parse_type_into!("-perm+", Test::PermAtLeast, parse_string),
            parse_type_into!("-perm/", Test::PermAny, parse_string),
            value(Test::Readable, tag("-readable")),
            parse_type_into!("-regex", Test::Regex, parse_string),
            parse_type_into!("-samefile", Test::Samefile, parse_string),
            parse_type_into!("-size", Test::Size, parse_string),
            value(Test::True, tag("-true")),
            parse_type_into!("-type", Test::Type, parse_string),
            parse_type_into!("-uid", Test::UserId, parse_comp),
            parse_type_into!("-user", Test::User, parse_string),
            value(Test::Writable, tag("-writable")),
        )),
    ))(input)
}

/// We need to split the operator parsing and the expression parsing in two scopes depending on
/// their precedence. This makes sure that a unary operator (not and precendence) will not glob a
/// binary expression it is actually a part of, but scanned first. As an example, with one
/// centralized method of parsing the operators, the lines:
///
/// ```txt
///     ! -uid 201 -a -uid +200
///     -uid +200 -a ! -uid 201
/// ```
///
/// Are not equivalent for the parser. This is because if we follow the definition of
/// GlobalOperator(GlobalExpression), Not(And(e1, e2)) is valid while being illegal
/// in the grammar without a precedence.
///
/// We then have:
/// ```txt
///     Not(And(Uid(Equal(201)), Uid(GreaterThan(200)))) -> Uid < 200
///     And(Uid(GreaterThan(200)), Not(Uid(Equal(201)))) -> Uid in {200, 202..}
/// ```
///
/// Splitting the methods transmits the information that the operator was unary
/// to ensure a valid output
#[recursive_parser]
fn parse_unary_operator(s: Span) -> IResult<Span, Operator> {
    alt((
        map(
            preceded(
                tuple((alt((tag("!"), tag("-not"))), character::complete::space0)),
                parse_unary_expression,
            ),
            Operator::Not,
        ),
        map(
            delimited(
                terminated(tag("("), character::complete::space0),
                parse_expression,
                preceded(character::complete::space0, tag(")")),
            ),
            Operator::Precedence,
        ),
    ))(s)
}

#[recursive_parser]
fn parse_binary_operator(s: Span) -> IResult<Span, Operator> {
    alt((
        map(
            separated_pair(
                parse_expression,
                delimited(
                    character::complete::space1,
                    alt((tag("-and"), tag("-a"))),
                    character::complete::space1,
                ),
                parse_expression,
            ),
            |(lhs, rhs)| Operator::And(lhs, rhs),
        ),
        map(
            separated_pair(
                parse_expression,
                delimited(
                    character::complete::space1,
                    alt((tag("-or"), tag("-o"))),
                    character::complete::space1,
                ),
                parse_expression,
            ),
            |(lhs, rhs)| Operator::Or(lhs, rhs),
        ),
        map(
            separated_pair(
                parse_expression,
                delimited(
                    character::complete::space0,
                    tag(","),
                    character::complete::space1,
                ),
                parse_expression,
            ),
            |(lhs, rhs)| Operator::List(lhs, rhs),
        ),
        map(
            separated_pair(
                parse_expression,
                character::complete::space1,
                parse_expression,
            ),
            |(lhs, rhs)| Operator::And(lhs, rhs),
        ),
    ))(s)
}

/// This exists to be a less powerful parse_expression if the parent operator was unary
pub fn parse_unary_expression(s: Span) -> IResult<Span, Expression> {
    alt((
        map(parse_unary_operator, |val| {
            let val = Rc::new(val);
            Expression::Operator(val)
        }),
        map(parse_test, Expression::Test),
        map(parse_global_option, Expression::Global),
    ))(s)
}

/// We make sure to parse binary operators first, as failing to do so may return lead to a partial
/// parse
fn parse_expression(s: Span) -> IResult<Span, Expression> {
    alt((
        map(parse_binary_operator, |val| {
            let val = std::rc::Rc::new(val);
            Expression::Operator(val)
        }),
        map(parse_unary_operator, |val| {
            let val = std::rc::Rc::new(val);
            Expression::Operator(val)
        }),
        map(parse_test, Expression::Test),
        map(parse_action, Expression::Action),
        map(parse_global_option, Expression::Global),
    ))(s)
}

pub fn parse<S: AsRef<str>>(input: S) -> Result<Expression, Box<dyn std::error::Error>> {
    let clc = String::from(input.as_ref());
    let wrapped_input = Span::new_extra(&clc, RecursiveInfo::new());
    match parse_expression(wrapped_input) {
        Ok((rem, _)) if rem.len() > 0 => {
            Err(format!("Failed to parse command line: {}", rem).into())
        }
        Ok((_, exp)) => Ok(exp),
        Err(e) => Err(e.to_string().into()),
    }
}

#[cfg(test)]
fn s(input: &str) -> Span {
    Span::new_extra(input, RecursiveInfo::new())
}

#[test]
fn test_parse_comparison() -> Result<(), Box<dyn std::error::Error>> {
    let (_, res) = parse_comp(s("44"))?;
    assert_eq!(Comparison::Equal(44), res);

    let (_, res) = parse_comp(s("+44"))?;
    assert_eq!(Comparison::GreaterThan(44), res);

    let (_, res) = parse_comp(s("-44"))?;
    assert_eq!(Comparison::LesserThan(44), res);

    Ok(())
}

#[test]
fn test_parse_string() -> Result<(), Box<dyn std::error::Error>> {
    let (_, res) = parse_string(s("a_long_string"))?;
    assert_eq!(String::from("a_long_string"), res);

    let (_, res) = parse_string(s("a_long_string\n"))?;
    assert_eq!(String::from("a_long_string"), res);

    let (_, res) = parse_string(s("a_long_string another"))?;
    assert_eq!(String::from("a_long_string"), res);

    let (_, res) = parse_string(s("'a_long_string another' again"))?;
    assert_eq!(String::from("a_long_string another"), res);

    Ok(())
}

#[test]
fn test_parse_global_option() -> Result<(), Box<dyn std::error::Error>> {
    let (_, res) = parse_global_option(s("-depth"))?;
    assert_eq!(GlobalOption::Depth, res);

    let (_, res) = parse_global_option(s("-maxdepth 44"))?;
    assert_eq!(GlobalOption::MaxDepth(44), res);

    let (_, res) = parse_global_option(s("-mindepth 44"))?;
    assert_eq!(GlobalOption::MinDepth(44), res);

    let res = parse_global_option(s("-maxdepth -44"));
    assert!(res.is_err());

    let res = parse_global_option(s("-mindepth -44"));
    assert!(res.is_err());

    Ok(())
}

#[test]
fn test_parse_test() -> Result<(), Box<dyn std::error::Error>> {
    let (_, res) = parse_test(s("-amin 44"))?;
    assert_eq!(Test::AccessMin(Comparison::Equal(44)), res);

    let (_, res) = parse_test(s("-true"))?;
    assert_eq!(Test::True, res);

    let (_, res) = parse_test(s("-false"))?;
    assert_eq!(Test::False, res);

    Ok(())
}

#[test]
fn test_parse_unary_operator() -> Result<(), Box<dyn std::error::Error>> {
    let (_, res) = parse_unary_operator(s("! -true"))?;
    assert_eq!(Operator::Not(Expression::Test(Test::True)), res);

    let (_, res) = parse_unary_operator(s("( -true )"))?;
    assert_eq!(Operator::Precedence(Expression::Test(Test::True)), res);

    Ok(())
}

#[test]
fn test_parse_binary_operator() -> Result<(), Box<dyn std::error::Error>> {
    let (_, res) = parse_binary_operator(s("-true -a -true"))?;
    assert_eq!(
        Operator::And(Expression::Test(Test::True), Expression::Test(Test::True)),
        res
    );

    let (_, other_and) = parse_binary_operator(s("-true -and -true"))?;
    assert_eq!(other_and, res);

    let (_, res) = parse_binary_operator(s("-true -o -true"))?;
    assert_eq!(
        Operator::Or(Expression::Test(Test::True), Expression::Test(Test::True)),
        res
    );

    let (_, other_or) = parse_binary_operator(s("-true -or -true"))?;
    assert_eq!(other_or, res);

    let (_, res) = parse_binary_operator(s("-true, -true"))?;
    assert_eq!(
        Operator::List(Expression::Test(Test::True), Expression::Test(Test::True)),
        res
    );

    Ok(())
}

#[test]
fn test_parse_expression() -> Result<(), Box<dyn std::error::Error>> {
    let (_, res) = parse_expression(s("-true"))?;
    assert_eq!(Expression::Test(Test::True), res);

    let (_, res) = parse_expression(s("-depth"))?;
    assert_eq!(Expression::Global(GlobalOption::Depth), res);

    let (_, res) = parse_expression(s("! -true"))?;
    assert_eq!(
        Expression::Operator(std::rc::Rc::new(Operator::Not(Expression::Test(
            Test::True
        )))),
        res
    );

    let (_, res) = parse_expression(s("-true -a ! -true"))?;
    assert_eq!(
        Expression::Operator(std::rc::Rc::new(Operator::And(
            Expression::Test(Test::True),
            Expression::Operator(Rc::new(Operator::Not(Expression::Test(Test::True)))),
        ))),
        res
    );

    let (_, res) = parse_expression(s("! -true -a -true"))?;
    assert_eq!(
        Expression::Operator(std::rc::Rc::new(Operator::And(
            Expression::Operator(Rc::new(Operator::Not(Expression::Test(Test::True)))),
            Expression::Test(Test::True),
        ))),
        res
    );

    let (_, res) = parse_expression(s("( -name test* -inum +8192 ) -o -user test-user"))?;
    assert_eq!(
        Expression::Operator(Rc::new(Operator::Or(
            Expression::Operator(Rc::new(Operator::Precedence(Expression::Operator(
                Rc::new(Operator::And(
                    Expression::Test(Test::Name(String::from("test*"))),
                    Expression::Test(Test::InodeNumber(Comparison::GreaterThan(8192u32)))
                ))
            )))),
            Expression::Test(Test::User(String::from("test-user")))
        ))),
        res
    );

    Ok(())
}
