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
use nom::sequence::tuple;
use nom::IResult;
use nom::{bytes, character};
use nom_locate::LocatedSpan;
use nom_recursive::{recursive_parser, RecursiveInfo};

pub type Span<'a> = LocatedSpan<&'a str, RecursiveInfo>;

fn parse_global_option(input: Span) -> IResult<Span, ast::GlobalOption> {
    alt((
        value(ast::GlobalOption::Depth, tag("-depth")),
        map(
            preceded(
                tuple((tag("-mindepth"), character::complete::space1)),
                character::complete::u32,
            ),
            ast::GlobalOption::MinDepth,
        ),
        map(
            preceded(
                tuple((tag("-maxdepth"), character::complete::space1)),
                character::complete::u32,
            ),
            ast::GlobalOption::MaxDepth,
        ),
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

fn parse_test(input: Span) -> IResult<Span, ast::Test> {
    alt((
        map(
            preceded(
                tuple((tag("-amin"), character::complete::space1)),
                character::complete::i32,
            ),
            ast::Test::AccessMin,
        ),
        map(
            preceded(
                tuple((tag("-anewer"), character::complete::space1)),
                parse_string,
            ),
            ast::Test::AccessNewer,
        ),
        map(
            preceded(
                tuple((tag("-atime"), character::complete::space1)),
                character::complete::i32,
            ),
            ast::Test::AccessTime,
        ),
        map(
            preceded(
                tuple((tag("-cmin"), character::complete::space1)),
                character::complete::i32,
            ),
            ast::Test::ChangeMin,
        ),
        map(
            preceded(
                tuple((tag("-cnewer"), character::complete::space1)),
                parse_string,
            ),
            ast::Test::ChangeNewer,
        ),
        map(
            preceded(
                tuple((tag("-ctime"), character::complete::space1)),
                character::complete::i32,
            ),
            ast::Test::ChangeTime,
        ),
        value(ast::Test::Empty, tag("-empty")),
        value(ast::Test::Executable, tag("-executable")),
        value(ast::Test::False, tag("-false")),
        map(
            preceded(
                tuple((tag("-fstype"), character::complete::space1)),
                parse_string,
            ),
            ast::Test::FsType,
        ),
        map(
            preceded(
                tuple((tag("-gid"), character::complete::space1)),
                character::complete::i32,
            ),
            ast::Test::GroupId,
        ),
        map(
            preceded(
                tuple((tag("-group"), character::complete::space1)),
                parse_string,
            ),
            ast::Test::Group,
        ),
        map(
            preceded(
                tuple((tag("-ilname"), character::complete::space1)),
                parse_string,
            ),
            ast::Test::InsensitiveLinkName,
        ),
        map(
            preceded(
                tuple((tag("-iname"), character::complete::space1)),
                parse_string,
            ),
            ast::Test::InsensitiveName,
        ),
        map(
            preceded(
                tuple((tag("-inum"), character::complete::space1)),
                character::complete::i32,
            ),
            ast::Test::InodeNumber,
        ),
        map(
            preceded(
                tuple((tag("-ipath"), character::complete::space1)),
                parse_string,
            ),
            ast::Test::InsensitivePath,
        ),
        map(
            preceded(
                tuple((tag("-iregex"), character::complete::space1)),
                parse_string,
            ),
            ast::Test::InsensitiveRegex,
        ),
        map(
            preceded(
                tuple((tag("-links"), character::complete::space1)),
                character::complete::i32,
            ),
            ast::Test::Hardlinks,
        ),
        map(
            preceded(
                tuple((tag("-mmin"), character::complete::space1)),
                character::complete::i32,
            ),
            ast::Test::ModifyMin,
        ),
        map(
            preceded(
                tuple((tag("-mnewer"), character::complete::space1)),
                parse_string,
            ),
            ast::Test::ModifyNewer,
        ),
        alt((
            map(
                preceded(
                    tuple((tag("-mtime"), character::complete::space1)),
                    character::complete::i32,
                ),
                ast::Test::ModifyTime,
            ),
            map(
                preceded(
                    tuple((tag("-name"), character::complete::space1)),
                    parse_string,
                ),
                ast::Test::Name,
            ),
            value(ast::Test::NoGroup, tag("-nouser")),
            value(ast::Test::NoUser, tag("-nogroup")),
            map(
                preceded(
                    tuple((tag("-path"), character::complete::space1)),
                    parse_string,
                ),
                ast::Test::Path,
            ),
            map(
                preceded(
                    tuple((tag("-perm"), character::complete::space1)),
                    parse_string,
                ),
                ast::Test::Perm,
            ),
            map(
                preceded(
                    tuple((tag("-perm+"), character::complete::space1)),
                    parse_string,
                ),
                ast::Test::PermAtLeast,
            ),
            map(
                preceded(
                    tuple((tag("-perm/"), character::complete::space1)),
                    parse_string,
                ),
                ast::Test::PermAny,
            ),
            value(ast::Test::Readable, tag("-readable")),
            map(
                preceded(
                    tuple((tag("-regex"), character::complete::space1)),
                    parse_string,
                ),
                ast::Test::Regex,
            ),
            map(
                preceded(
                    tuple((tag("-samefile"), character::complete::space1)),
                    parse_string,
                ),
                ast::Test::Samefile,
            ),
            map(
                preceded(
                    tuple((tag("-size"), character::complete::space1)),
                    parse_string,
                ),
                ast::Test::Size,
            ),
            value(ast::Test::True, tag("-true")),
            map(
                preceded(
                    tuple((tag("-type"), character::complete::space1)),
                    parse_string,
                ),
                ast::Test::Type,
            ),
            map(
                preceded(
                    tuple((tag("-uid"), character::complete::space1)),
                    character::complete::i32,
                ),
                ast::Test::UserId,
            ),
            map(
                preceded(
                    tuple((tag("-user"), character::complete::space1)),
                    parse_string,
                ),
                ast::Test::User,
            ),
            value(ast::Test::Writable, tag("-writable")),
        )),
    ))(input)
}

#[recursive_parser]
fn parse_operator(s: Span) -> IResult<Span, ast::Operator> {
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
            |(lhs, rhs)| ast::Operator::And(lhs, rhs),
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
            |(lhs, rhs)| ast::Operator::Or(lhs, rhs),
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
            |(lhs, rhs)| ast::Operator::List(lhs, rhs),
        ),
        map(
            separated_pair(
                parse_expression,
                character::complete::space1,
                parse_expression,
            ),
            |(lhs, rhs)| ast::Operator::And(rhs, lhs),
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
            preceded(
                tuple((alt((tag("!"), tag("-not"))), character::complete::space0)),
                parse_expression,
            ),
            ast::Operator::Not,
        ),
    ))(s)
}

pub fn parse_expression(input: Span) -> IResult<Span, ast::Expression> {
    alt((
        map(parse_operator, |val| {
            let val = std::rc::Rc::new(val);
            ast::Expression::Operator(val)
        }),
        map(parse_test, ast::Expression::Test),
        map(parse_global_option, ast::Expression::Global),
    ))(input)
}

#[cfg(test)]
fn s(input: &str) -> Span {
    Span::new_extra(input, RecursiveInfo::new())
}

#[test]
fn test_parse_global_option() -> Result<(), Box<dyn std::error::Error>> {
    let (_, res) = parse_global_option(s("-depth"))?;
    assert_eq!(ast::GlobalOption::Depth, res);

    let (_, res) = parse_global_option(s("-maxdepth 44"))?;
    assert_eq!(ast::GlobalOption::MaxDepth(44), res);

    let (_, res) = parse_global_option(s("-mindepth 44"))?;
    assert_eq!(ast::GlobalOption::MinDepth(44), res);

    let res = parse_global_option(s("-maxdepth -44"));
    assert!(res.is_err());

    let res = parse_global_option(s("-mindepth -44"));
    assert!(res.is_err());

    Ok(())
}

#[test]
fn test_parse_test() -> Result<(), Box<dyn std::error::Error>> {
    let (_, res) = parse_test(s("-amin 44"))?;
    assert_eq!(ast::Test::AccessMin(44), res);

    let (_, res) = parse_test(s("-anewer '/path/to/file with spaces'"))?;
    assert_eq!(
        ast::Test::AccessNewer(String::from("/path/to/file with spaces")),
        res
    );

    let (_, res) = parse_test(s("-anewer /path/to/file\n"))?;
    assert_eq!(ast::Test::AccessNewer(String::from("/path/to/file")), res);

    let (_, res) = parse_test(s("-anewer /path/to/file"))?;
    assert_eq!(ast::Test::AccessNewer(String::from("/path/to/file")), res);

    Ok(())
}

#[test]
fn test_parse_operator() -> Result<(), Box<dyn std::error::Error>> {
    let (_, res) = parse_operator(s("! -true"))?;
    assert_eq!(
        ast::Operator::Not(ast::Expression::Test(ast::Test::True)),
        res
    );

    let (_, res) = parse_operator(s("( -true )"))?;
    assert_eq!(
        ast::Operator::Precedence(ast::Expression::Test(ast::Test::True)),
        res
    );

    let (_, res) = parse_operator(s("-true -a -true"))?;
    assert_eq!(
        ast::Operator::And(
            ast::Expression::Test(ast::Test::True),
            ast::Expression::Test(ast::Test::True)
        ),
        res
    );

    let (_, other_and) = parse_operator(s("-true -and -true"))?;
    assert_eq!(other_and, res);

    let (_, res) = parse_operator(s("-true -o -true"))?;
    assert_eq!(
        ast::Operator::Or(
            ast::Expression::Test(ast::Test::True),
            ast::Expression::Test(ast::Test::True)
        ),
        res
    );

    let (_, other_or) = parse_operator(s("-true -or -true"))?;
    assert_eq!(other_or, res);

    let (_, res) = parse_operator(s("-true, -true"))?;
    assert_eq!(
        ast::Operator::List(
            ast::Expression::Test(ast::Test::True),
            ast::Expression::Test(ast::Test::True)
        ),
        res
    );

    Ok(())
}

#[test]
fn test_parse_expression() -> Result<(), Box<dyn std::error::Error>> {
    let (_, res) = parse_expression(s("-true"))?;
    assert_eq!(ast::Expression::Test(ast::Test::True), res);

    let (_, res) = parse_expression(s("-depth"))?;
    assert_eq!(ast::Expression::Global(ast::GlobalOption::Depth), res);

    let (_, res) = parse_expression(s("! -true"))?;
    assert_eq!(
        ast::Expression::Operator(std::rc::Rc::new(ast::Operator::Not(ast::Expression::Test(
            ast::Test::True
        )))),
        res
    );

    Ok(())
}
