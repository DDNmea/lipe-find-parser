#![allow(unused_imports)]
#![allow(dead_code)]
use crate::ast::{Action, Comparison, Expression as Exp, GlobalOption, Operator as Ope, Test};
use crate::winnow_find::parse_test;
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

#[derive(Clone, PartialEq, Debug)]
pub enum Token {
    LParen,
    RParen,
    Or,
    And,
    Not,
    Comma,
    Test(Test),
    //Action(Action),
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

// We parse the command line to
pub fn token(input: &mut &str) -> PResult<Token> {
    alt((
        literal("(").value(Token::LParen),
        literal(")").value(Token::RParen),
        literal("!").value(Token::Not),
        literal("-o").value(Token::Or),
        literal(",").value(Token::Comma),
        terminated(alt((literal("-a"), literal("-and"))), multispace1).value(Token::And),
        parse_test.map(Token::Test),
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
        one_of(|t| matches!(t, Token::Test(_))).map(|t| match t {
            Token::Test(v) => Exp::Test(v),
            _ => unreachable!(),
        }),
        parens,
    ))
    .parse_next(input)
}

fn parens(input: &mut &[Token]) -> PResult<Exp> {
    delimited(one_of(Token::LParen), list, one_of(Token::RParen)).parse_next(input)
}

#[allow(dead_code)]
pub fn parse(input: &mut &str) -> PResult<Exp> {
    let tokens = lex.parse_next(input)?;
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

#[test]
fn test_parse_test() {
    let res = parse(&mut "-amin 44");
    assert_eq!(Ok(Exp::Test(Test::AccessMin(Comparison::Equal(44)))), res);

    let res = parse(&mut "-true");
    assert_eq!(Ok(Exp::Test(Test::True)), res);

    let res = parse(&mut "-false");
    assert_eq!(Ok(Exp::Test(Test::False)), res);

    let res = parse(&mut "-amin");
    assert!(res.is_err());

    let res = parse(&mut "-amin test");
    assert!(res.is_err());
}

#[test]
fn test_parse_operator() {
    let res = parse(&mut "-true -o -false");
    assert_eq!(
        Ok(Exp::Operator(Rc::new(Ope::Or(
            Exp::Test(Test::True),
            Exp::Test(Test::False)
        )))),
        res
    );

    let res = parse(&mut "-true -a -false");
    assert_eq!(
        Ok(Exp::Operator(Rc::new(Ope::And(
            Exp::Test(Test::True),
            Exp::Test(Test::False)
        )))),
        res
    );

    // Test operator precedence
    // and has a higher precedence than or, so we test this is reflected in the AST
    let res = parse(&mut "-true -a -false -o -name test");
    assert_eq!(
        Ok(Exp::Operator(Rc::new(Ope::Or(
            Exp::Operator(Rc::new(Ope::And(
                Exp::Test(Test::True),
                Exp::Test(Test::False)
            ))),
            Exp::Test(Test::Name(String::from("test")))
        )))),
        res
    );

    let res = parse(&mut "-true -o -false -a -name test");
    assert_eq!(
        Ok(Exp::Operator(Rc::new(Ope::Or(
            Exp::Test(Test::True),
            Exp::Operator(Rc::new(Ope::And(
                Exp::Test(Test::False),
                Exp::Test(Test::Name(String::from("test"))),
            ))),
        )))),
        res
    );

    let res = parse(&mut "-true -a (-false -o -name test)");
    assert_eq!(
        Ok(Exp::Operator(Rc::new(Ope::And(
            Exp::Test(Test::True),
            Exp::Operator(Rc::new(Ope::Or(
                Exp::Test(Test::False),
                Exp::Test(Test::Name(String::from("test"))),
            ))),
        )))),
        res
    );

    let res = parse(&mut "-true -false");
    assert_eq!(
        Ok(Exp::Operator(Rc::new(Ope::And(
            Exp::Test(Test::True),
            Exp::Test(Test::False)
        )))),
        res
    );
}
