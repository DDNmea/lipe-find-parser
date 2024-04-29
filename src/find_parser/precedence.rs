/// Collection of parsers that operate on [crate::find_parser::Token] slices to do precedence parsing.
///
/// This parsing method will keep track of the last encountered operator to ensure
/// the operator precedence is respected in the final expression.
use crate::find_parser::prelude::*;
use crate::find_parser::Token;
use std::rc::Rc;

/// Precedence parser entry point
pub fn parser(input: &mut &[Token]) -> PResult<Exp> {
    let out = repeat_till(1.., list, eof)
        .context(StrContext::Label("grammar"))
        .parse_next(input)
        .map(|(list, _): (Vec<Exp>, _)| list)?;

    // We unwrap here as the repeat_till ensures at least one expression is returned
    Ok(out.first().unwrap().to_owned())
}

/// Lowest priority operator, if a [Token::Comma] is encountered in the token list we wait until
/// either side of it are resolved to expressions.
fn list(input: &mut &[Token]) -> PResult<Exp> {
    let init = or.parse_next(input)?;

    repeat(
        0..,
        preceded(
            one_of(Token::Comma),
            cut_err(or).context(StrContext::Expected(StrContextValue::Description(
                "missing_list_clause",
            ))),
        ),
    )
    .fold(
        move || init.clone(),
        |acc, val| Exp::Operator(Rc::new(Ope::List(acc, val))),
    )
    .parse_next(input)
}

/// Higher priority than list, but not as high as [Token::And]. We parse either side first.
fn or(input: &mut &[Token]) -> PResult<Exp> {
    let init = and.parse_next(input)?;

    repeat(
        0..,
        preceded(
            one_of(Token::Or),
            cut_err(and).context(StrContext::Expected(StrContextValue::Description(
                "missing_or_clause",
            ))),
        ),
    )
    .fold(
        move || init.clone(),
        |acc, val| Exp::Operator(Rc::new(Ope::Or(acc, val))),
    )
    .parse_next(input)
}

/// Highest priority operator. Surrounding it are atoms, and we attempt to parse them as such.
fn and(input: &mut &[Token]) -> PResult<Exp> {
    let init = atom.parse_next(input)?;

    // awful awful awful readability but here we check for (Token::And, atom) first. In case we are
    // not so lucky try for atom again, at which point we have two atoms together and this means we
    // are blessed with the presence of an implicit and.
    repeat(
        0..,
        alt((
            preceded(
                one_of(Token::And),
                cut_err(atom).context(StrContext::Expected(StrContextValue::Description(
                    "missing_and_clause",
                ))),
            ),
            atom,
        )),
    )
    .fold(
        move || init.clone(),
        |acc, val| Exp::Operator(Rc::new(Ope::And(acc, val))),
    )
    .parse_next(input)
}

/// Atom is either a null/un/binary directive, a negated atom, or an expression in parenthesis.
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
        preceded(
            any,
            fail.context(StrContext::Expected(StrContextValue::Description(
                "unexpected_token",
            ))),
        ),
    ))
    .parse_next(input)
}

/// Not operator before an atom.
fn not(input: &mut &[Token]) -> PResult<Exp> {
    preceded(
        one_of(Token::Not),
        cut_err(atom).context(StrContext::Expected(StrContextValue::Description(
            "missing_not_clause",
        ))),
    )
    .map(|val| Exp::Operator(Rc::new(Ope::Not(val))))
    .parse_next(input)
}

/// An expression delimited by parenthesis.
fn parens(input: &mut &[Token]) -> PResult<Exp> {
    delimited(
        one_of(Token::LParen),
        cut_err(list).context(StrContext::Expected(StrContextValue::Description(
            "missing_expression",
        ))),
        cut_err(one_of(Token::RParen)).context(StrContext::Expected(StrContextValue::Description(
            "missing_closing_parenthesis",
        ))),
    )
    .context(StrContext::Label("parens"))
    .parse_next(input)
}

#[test]
/// A naive implementation will create a parser unable to generate an error when extra tokens
/// are added to the command line. The parser works by repeatedly parsing each precedence
/// level, and knows when to stop when ErrMode::Backtrack is encountered. This leads to the
/// unability to parse extra tokens on the command line, as their presence would just stop the
/// parser. This test makes sure we handle such a case.
fn test_parse_complete() {
    // Equivalent to `-name test \) -uid 1000`. The parser used to see -name test, then stop
    // when encountering the extra parenthesis, and ignore everything else.
    let input = vec![
        Token::Test(Test::Name(String::from("test"))),
        Token::RParen,
        Token::Test(Test::UserId(Comparison::Equal(1000))),
    ];
    let res = parser(&mut input.as_slice());
    assert!(res.is_err());
}

#[test]
fn test_parse_parens() {
    let input = vec![Token::LParen, Token::Test(Test::True), Token::RParen];
    let res = parens(&mut input.as_slice());
    assert_eq!(res, Ok(Exp::Test(Test::True)));

    let input = vec![
        Token::LParen,
        Token::LParen,
        Token::Test(Test::True),
        Token::RParen,
        Token::RParen,
    ];
    let res = parens(&mut input.as_slice());
    assert_eq!(res, Ok(Exp::Test(Test::True)));
}

#[test]
fn test_parse_parens_error() {
    let input = vec![Token::LParen, Token::RParen];
    let res = parens(&mut input.as_slice());
    assert!(res.is_err());

    let input = vec![Token::LParen, Token::Test(Test::True)];
    let res = parens(&mut input.as_slice());
    assert!(res.is_err());

    let input = vec![Token::Test(Test::True), Token::RParen];
    let res = parens(&mut input.as_slice());
    assert!(res.is_err());
}

#[test]
fn test_parse_not() {
    let input = vec![Token::Not, Token::Test(Test::True)];
    let res = not(&mut input.as_slice());
    assert_eq!(
        res,
        Ok(Exp::Operator(Rc::new(Ope::Not(Exp::Test(Test::True)))))
    );
}

#[test]
fn test_parse_not_error() {
    let input = vec![Token::Not];
    let res = not(&mut input.as_slice());
    assert!(res.is_err());

    let input = vec![Token::Test(Test::True), Token::Not];
    let res = not(&mut input.as_slice());
    println!("{:?} {:?}", input, res);
    assert!(res.is_err());
}
