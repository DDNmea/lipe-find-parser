#![allow(unused_imports)]
#![allow(dead_code)]
use crate::ast::{Action, Comparison, Expression, GlobalOption, Operator, PositionalOption, Test};
use std::rc::Rc;
use winnow::{
    ascii::{alpha1, digit1, multispace0, multispace1},
    combinator::alt,
    combinator::repeat,
    combinator::{cut_err, opt},
    combinator::{delimited, preceded, separated_pair, terminated},
    error::ContextError,
    error::StrContext,
    prelude::*,
    token::{literal, one_of, take_until, take_while},
};

macro_rules! parse_type_into {
    ($tag:expr, $target:expr, $parser:expr) => {
        preceded(terminated($tag, multispace1), cut_err($parser)).map($target)
    };
}

fn parse_u32(i: &mut &'_ str) -> PResult<u32> {
    digit1
        .try_map(|digit_str: &str| digit_str.parse::<u32>())
        .parse_next(i)
}

fn parse_comp(input: &mut &'_ str) -> PResult<Comparison> {
    alt((
        preceded("+", cut_err(parse_u32)).map(Comparison::GreaterThan),
        preceded("-", cut_err(parse_u32)).map(Comparison::LesserThan),
        parse_u32.map(Comparison::Equal),
    ))
    .parse_next(input)
}

fn parse_string(input: &mut &'_ str) -> PResult<String> {
    alt((
        delimited("'", take_until(0.., "'"), "'"),
        take_while(0.., |c| c != ' ' && c != '\n' && c != ')'),
    ))
    .map(String::from)
    .parse_next(input)
}

pub fn parse_global_option(input: &mut &'_ str) -> PResult<GlobalOption> {
    alt((
        literal("-depth").value(GlobalOption::Depth),
        preceded(terminated("-maxdepth", multispace1), cut_err(parse_u32))
            .map(GlobalOption::MaxDepth),
        preceded(terminated("-mindepth", multispace1), cut_err(parse_u32))
            .map(GlobalOption::MinDepth),
    ))
    .parse_next(input)
}

pub fn parse_positional(input: &mut &'_ str) -> PResult<PositionalOption> {
    literal("nope")
        .value(PositionalOption::XDev)
        .parse_next(input)
}

pub fn parse_action(input: &mut &'_ str) -> PResult<Action> {
    alt((
        preceded(terminated("-fls", multispace1), cut_err(parse_string)).map(Action::FileList),
        preceded(terminated("-fprint", multispace1), cut_err(parse_string)).map(Action::FilePrint),
        preceded(terminated("-fprint0", multispace1), cut_err(parse_string))
            .map(Action::FilePrintNull),
        //parse_type_into!("-fprintf", Action::FilePrintFormatted, parse_string),
        literal("-ls").value(Action::List),
        terminated("-print", multispace0).value(Action::Print),
        terminated("-print0", multispace0).value(Action::PrintNull),
        preceded(terminated("-printf", multispace1), cut_err(parse_string))
            .map(Action::PrintFormatted),
        literal("-prune").value(Action::Prune),
        literal("-quit").value(Action::Quit),
    ))
    .parse_next(input)
}

pub fn parse_test(input: &mut &'_ str) -> PResult<Test> {
    alt((
        alt((
            parse_type_into!("-amin", Test::AccessMin, parse_comp),
            parse_type_into!("-anewer", Test::AccessNewer, parse_string),
            parse_type_into!("-atime", Test::AccessTime, parse_comp),
            parse_type_into!("-cmin", Test::ChangeMin, parse_comp),
            parse_type_into!("-cnewer", Test::ChangeNewer, parse_string),
            parse_type_into!("-ctime", Test::ChangeTime, parse_comp),
            literal("-empty").value(Test::Empty),
            literal("-executable").value(Test::Executable),
            literal("-false").value(Test::False),
            parse_type_into!("-fstype", Test::FsType, parse_string),
            parse_type_into!("-gid", Test::GroupId, parse_u32),
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
            literal("-nouser").value(Test::NoGroup),
            literal("-nogroup").value(Test::NoUser),
            parse_type_into!("-path", Test::Path, parse_string),
            parse_type_into!("-perm", Test::Perm, parse_string),
            parse_type_into!("-perm+", Test::PermAtLeast, parse_string),
            parse_type_into!("-perm/", Test::PermAny, parse_string),
            literal("-readable").value(Test::Readable),
            parse_type_into!("-regex", Test::Regex, parse_string),
            parse_type_into!("-samefile", Test::Samefile, parse_string),
            parse_type_into!("-size", Test::Size, parse_string),
            literal("-true").value(Test::True),
            parse_type_into!("-type", Test::Type, parse_string),
            parse_type_into!("-uid", Test::UserId, parse_u32),
            parse_type_into!("-user", Test::User, parse_string),
            literal("-writable").value(Test::Writable),
        )),
    ))
    .parse_next(input)
}

/*
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
*/

/// We make sure to parse binary operators first, as failing to do so may return lead to a partial
/// parse
fn parse_expression(input: &mut &'_ str) -> PResult<Expression> {
    alt((
        parse_operator,
        /*
        map(parse_unary_operator, |val| {
            let val = std::rc::Rc::new(val);
            Expression::Operator(val)
        }),
        map(parse_test, Expression::Test),
        */
        parse_action.map(Expression::Action),
        parse_global_option.map(Expression::Global),
    ))
    .parse_next(input)
}

pub fn parse_operator(input: &mut &'_ str) -> PResult<Expression> {
    alt((
        preceded(
            (alt((literal("!"), literal("-not"))), multispace0),
            cut_err(parse_expression),
        )
        .map(Operator::Not),
        delimited(
            terminated(literal("("), multispace0),
            parse_expression,
            preceded(multispace0, literal(")")),
        )
        .map(Operator::Precedence),
        separated_pair(
            parse_expression,
            delimited(
                multispace1,
                alt((literal("-and"), literal("-a"))),
                multispace1,
            ),
            parse_expression,
        )
        .map(|(lhs, rhs)| Operator::And(lhs, rhs)),
        separated_pair(
            parse_expression,
            delimited(
                multispace1,
                alt((literal("-or"), literal("-o"))),
                multispace1,
            ),
            parse_expression,
        )
        .map(|(lhs, rhs)| Operator::Or(lhs, rhs)),
        separated_pair(
            parse_expression,
            delimited(multispace0, literal(","), multispace1),
            parse_expression,
        )
        .map(|(lhs, rhs)| Operator::List(lhs, rhs)),
        separated_pair(parse_expression, multispace1, parse_expression)
            .map(|(lhs, rhs)| Operator::And(lhs, rhs)),
    ))
    .map(Rc::new)
    .map(Expression::Operator)
    .parse_next(input)
}

#[test]
fn test_parse_comparison() -> Result<(), Box<dyn std::error::Error>> {
    let res = parse_comp(&mut "44").unwrap();
    assert_eq!(Comparison::Equal(44), res);

    let res = parse_comp(&mut "+44").unwrap();
    assert_eq!(Comparison::GreaterThan(44), res);

    let res = parse_comp(&mut "-44").unwrap();
    assert_eq!(Comparison::LesserThan(44), res);

    Ok(())
}

#[test]
fn test_parse_string() -> Result<(), Box<dyn std::error::Error>> {
    let res = parse_string(&mut "a_long_string").unwrap();
    assert_eq!(String::from("a_long_string"), res);

    let res = parse_string(&mut "a_long_string\n").unwrap();
    assert_eq!(String::from("a_long_string"), res);

    let res = parse_string(&mut "a_long_string another").unwrap();
    assert_eq!(String::from("a_long_string"), res);

    let res = parse_string(&mut "'a_long_string another' again").unwrap();
    assert_eq!(String::from("a_long_string another"), res);

    Ok(())
}

#[test]
fn test_parse_global_option() -> Result<(), Box<dyn std::error::Error>> {
    let res = parse_global_option(&mut "-depth").unwrap();
    assert_eq!(GlobalOption::Depth, res);

    let res = parse_global_option(&mut "-maxdepth 44").unwrap();
    assert_eq!(GlobalOption::MaxDepth(44), res);

    let res = parse_global_option(&mut "-mindepth 44").unwrap();
    assert_eq!(GlobalOption::MinDepth(44), res);

    let res = parse_global_option(&mut "-maxdepth -44");
    assert!(res.is_err());

    let res = parse_global_option(&mut "-mindepth -44");
    assert!(res.is_err());

    Ok(())
}

#[test]
fn test_parse_test() -> Result<(), Box<dyn std::error::Error>> {
    let res = parse_test(&mut "-amin 44").unwrap();
    assert_eq!(Test::AccessMin(Comparison::Equal(44)), res);

    let res = parse_test(&mut "-true").unwrap();
    assert_eq!(Test::True, res);

    let res = parse_test(&mut "-false").unwrap();
    assert_eq!(Test::False, res);

    Ok(())
}

/*
#[test]
fn test_parse_unary_operator() -> Result<(), Box<dyn std::error::Error>> {
    let res = parse_operator(&mut "! -depth").unwrap();
    assert_eq!(Operator::Not(Expression::Global(GlobalOption::Depth)), res);

    let res = parse_operator(&mut "( -depth )").unwrap();
    assert_eq!(
        Operator::Precedence(Expression::Global(GlobalOption::Depth)),
        res
    );

    Ok(())
}
*/

/*
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
    /*
    let (_, res) = parse_expression(s("-true"))?;
    assert_eq!(Expression::Test(Test::True), res);
    */

    let res = parse_expression(&mut "-depth").unwrap();
    assert_eq!(Expression::Global(GlobalOption::Depth), res);

    let res = parse_expression(&mut "! -ls").unwrap();
    assert_eq!(
        Expression::Operator(std::rc::Rc::new(Operator::Not(Expression::Action(
            Action::List
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
    */
