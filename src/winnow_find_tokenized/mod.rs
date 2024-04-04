#![allow(unused_imports)]
#![allow(dead_code)]
use crate::ast::{Action, Comparison, Expression, GlobalOption, Operator, Test};
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

fn expr(input: &mut &[Token]) -> PResult<Expression> {
    let init = or.parse_next(input)?;

    repeat(0.., (one_of(Token::Comma), or))
        .fold(
            move || init.clone(),
            |acc, (_, val): (Token, Expression)| {
                Expression::Operator(Rc::new(Operator::List(acc, val)))
            },
        )
        .parse_next(input)
}

fn or(input: &mut &[Token]) -> PResult<Expression> {
    let init = and.parse_next(input)?;

    repeat(0.., (one_of(Token::Or), and))
        .fold(
            move || init.clone(),
            |acc, (_, val): (Token, Expression)| {
                Expression::Operator(Rc::new(Operator::Or(acc, val)))
            },
        )
        .parse_next(input)
}

fn and(input: &mut &[Token]) -> PResult<Expression> {
    let init = atom.parse_next(input)?;

    repeat(0.., (one_of(Token::And), atom))
        .fold(
            move || init.clone(),
            |acc, (_, val): (Token, Expression)| {
                Expression::Operator(Rc::new(Operator::And(acc, val)))
            },
        )
        .parse_next(input)
}

fn atom(input: &mut &[Token]) -> PResult<Expression> {
    alt((
        one_of(|t| matches!(t, Token::Test(_))).map(|t| match t {
            Token::Test(v) => Expression::Test(v),
            _ => unreachable!(),
        }),
        parens,
    ))
    .parse_next(input)
}

fn parens(input: &mut &[Token]) -> PResult<Expression> {
    delimited(one_of(Token::LParen), expr, one_of(Token::RParen)).parse_next(input)
}

#[allow(dead_code)]
pub fn parse(input: &mut &str) -> PResult<Expression> {
    let tokens = lex.parse_next(input)?;
    expr.parse_next(&mut tokens.as_slice())
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
    assert_eq!(
        Ok(Expression::Test(Test::AccessMin(Comparison::Equal(44)))),
        res
    );

    let res = parse(&mut "-true");
    assert_eq!(Ok(Expression::Test(Test::True)), res);

    let res = parse(&mut "-false");
    assert_eq!(Ok(Expression::Test(Test::False)), res);
}

#[test]
fn test_parse_operator() {
    let res = parse(&mut "-true -o -false");
    assert_eq!(
        Ok(Expression::Operator(Rc::new(Operator::Or(
            Expression::Test(Test::True),
            Expression::Test(Test::False)
        )))),
        res
    );

    let res = parse(&mut "-true -a -false");
    assert_eq!(
        Ok(Expression::Operator(Rc::new(Operator::And(
            Expression::Test(Test::True),
            Expression::Test(Test::False)
        )))),
        res
    );

    let res = parse(&mut "-true -a -false -o -name test");
    assert_eq!(
        Ok(Expression::Operator(Rc::new(Operator::Or(
            Expression::Operator(Rc::new(Operator::And(
                Expression::Test(Test::True),
                Expression::Test(Test::False)
            ))),
            Expression::Test(Test::Name(String::from("test")))
        )))),
        res
    );
}
