#![allow(unused_imports, dead_code)]
use crate::ast::{
    Action, Comparison, Expression as Exp, GlobalOption, Operator as Ope, PositionalOption, Test,
};
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
            parse_type_into!("-uid", Test::UserId, parse_comp),
            parse_type_into!("-user", Test::User, parse_string),
            literal("-writable").value(Test::Writable),
        )),
    ))
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

#[derive(Clone, PartialEq, Debug)]
pub enum Token {
    LParen,
    RParen,
    Or,
    And,
    Not,
    Comma,
    Test(Test),
    Action(Action),
    Global(GlobalOption),
    Positional(PositionalOption),
}

impl winnow::stream::ContainsToken<Token> for Token {
    #[inline(always)]
    fn contains_token(&self, token: Token) -> bool {
        *self == token
    }
}

impl winnow::stream::ContainsToken<Token> for &'_ [Token] {
    #[inline]
    fn contains_token(&self, token: Token) -> bool {
        self.iter().any(|t| *t == token)
    }
}

pub fn lex(input: &mut &str) -> PResult<Vec<Token>> {
    preceded(multispace0, repeat(1.., terminated(token, multispace0))).parse_next(input)
}

pub fn token(input: &mut &str) -> PResult<Token> {
    alt((
        literal("(").value(Token::LParen),
        literal(")").value(Token::RParen),
        literal("!").value(Token::Not),
        literal("-o").value(Token::Or),
        literal(",").value(Token::Comma),
        terminated(alt((literal("-a"), literal("-and"))), multispace1).value(Token::And),
        parse_test.map(Token::Test),
        parse_action.map(Token::Action),
        parse_global_option.map(Token::Global),
        parse_positional.map(Token::Positional),
    ))
    .parse_next(input)
}

fn list(input: &mut &[Token]) -> PResult<Exp> {
    let init = or.parse_next(input)?;

    repeat(0.., (one_of(Token::Comma), or))
        .fold(
            move || init.clone(),
            |acc, (_, val): (Token, Exp)| Exp::Operator(Rc::new(Ope::List(acc, val))),
        )
        .parse_next(input)
}

fn or(input: &mut &[Token]) -> PResult<Exp> {
    let init = and.parse_next(input)?;

    repeat(0.., preceded(one_of(Token::Or), and))
        .fold(
            move || init.clone(),
            |acc, val| Exp::Operator(Rc::new(Ope::Or(acc, val))),
        )
        .parse_next(input)
}

fn and(input: &mut &[Token]) -> PResult<Exp> {
    let init = atom.parse_next(input)?;

    // awful awful awful readability but here we check for (Token::And, atom) first. In case we are
    // not so lucky try for atom again, at which point we have two atoms together and this means we
    // are blessed with the presence of an implicit and.
    repeat(0.., alt((preceded(one_of(Token::And), atom), atom)))
        .fold(
            move || init.clone(),
            |acc, val| Exp::Operator(Rc::new(Ope::And(acc, val))),
        )
        .parse_next(input)
}

fn atom(input: &mut &[Token]) -> PResult<Exp> {
    alt((
        one_of(|t| {
            matches!(
                t,
                Token::Test(_) | Token::Action(_) | Token::Global(_) | Token::Positional(_)
            )
        })
        .map(|t| match t {
            Token::Test(v) => Exp::Test(v),
            Token::Action(v) => Exp::Action(v),
            Token::Global(v) => Exp::Global(v),
            Token::Positional(v) => Exp::Positional(v),
            _ => unreachable!(),
        }),
        not,
        parens,
    ))
    .parse_next(input)
}

fn not(input: &mut &[Token]) -> PResult<Exp> {
    preceded(one_of(Token::Not), atom)
        .map(|val| Exp::Operator(Rc::new(Ope::Not(val))))
        .parse_next(input)
}

fn parens(input: &mut &[Token]) -> PResult<Exp> {
    delimited(one_of(Token::LParen), list, one_of(Token::RParen)).parse_next(input)
}

pub fn parse<S: AsRef<str>>(input: S) -> PResult<Exp> {
    let mut clone = input.as_ref();
    let tokens = lex.parse_next(&mut clone)?;
    log::debug!("Tokens: {:?}", tokens);
    list.parse_next(&mut tokens.as_slice())
}

#[test]
fn test_token() {
    let res = token(&mut "(");
    assert_eq!(res, Ok(Token::LParen));

    let res = token(&mut "(");
    assert_eq!(res, Ok(Token::LParen));

    let res = token(&mut "-atime 77");
    assert_eq!(
        res,
        Ok(Token::Test(Test::AccessTime(Comparison::Equal(77))))
    );
}

#[test]
fn test_lex() {
    let res = lex(&mut "-atime 77");
    assert_eq!(
        res,
        Ok(vec![Token::Test(Test::AccessTime(Comparison::Equal(77)))])
    );

    let res = lex(&mut "! -atime 77 ( -name test )");
    assert_eq!(
        res,
        Ok(vec![
            Token::Not,
            Token::Test(Test::AccessTime(Comparison::Equal(77))),
            Token::LParen,
            Token::Test(Test::Name(String::from("test"))),
            Token::RParen,
        ])
    );

    let res = lex(&mut "-true -a -false");
    assert_eq!(
        res,
        Ok(vec![
            Token::Test(Test::True),
            Token::And,
            Token::Test(Test::False)
        ])
    );
}
