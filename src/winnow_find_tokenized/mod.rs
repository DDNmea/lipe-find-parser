#![allow(unused_imports, dead_code)]
use crate::ast::{
    Action, Comparison, Expression as Exp, GlobalOption, Operator as Ope, PositionalOption, Test,
};
use crate::RunOptions;
use std::rc::Rc;
use winnow::{
    ascii::{alpha1, digit1, multispace0, multispace1},
    combinator::{
        alt, cut_err, delimited, eof, fail, opt, preceded, repeat, repeat_till, separated_pair,
        terminated,
    },
    error::{ContextError, StrContext, StrContextValue},
    prelude::*,
    token::{literal, one_of, take_until, take_while},
};

macro_rules! parse_type_into {
    ($tag:expr, $target:expr, $parser:expr) => {
        preceded($tag, cut_err(preceded(multispace1, $parser)))
            .context(StrContext::Label($tag))
            .map($target)
    };
}

macro_rules! parse_unary_pretty {
    ($unary:expr, $target:expr, $expected:expr) => {
        preceded(
            $unary,
            cut_err(preceded(multispace1, $expected))
                .context(StrContext::Label($unary))
                .map($target),
        )
    };
}

fn parse_u32(i: &mut &'_ str) -> PResult<u32> {
    digit1
        .try_map(|digit_str: &str| digit_str.parse::<u32>())
        .context(StrContext::Expected(StrContextValue::Description(
            "unsigned_integer",
        )))
        .parse_next(i)
}

fn parse_comp(input: &mut &'_ str) -> PResult<Comparison> {
    cut_err(alt((
        preceded("+", parse_u32).map(Comparison::GreaterThan),
        preceded("-", parse_u32).map(Comparison::LesserThan),
        parse_u32.map(Comparison::Equal),
    )))
    .context(StrContext::Label("comparison"))
    .parse_next(input)
}

fn parse_string(input: &mut &'_ str) -> PResult<String> {
    alt((
        delimited("'", take_until(0.., "'"), "'"),
        take_while(0.., |c| c != ' ' && c != '\n' && c != ')'),
    ))
    .context(StrContext::Expected(StrContextValue::Description("string")))
    .map(String::from)
    .parse_next(input)
}

pub fn parse_global_option(input: &mut &'_ str) -> PResult<GlobalOption> {
    alt((
        literal("-depth").value(GlobalOption::Depth),
        parse_unary_pretty!("-maxdepth", GlobalOption::MaxDepth, parse_u32),
        parse_unary_pretty!("-mindepth", GlobalOption::MinDepth, parse_u32),
        parse_unary_pretty!("-threads", GlobalOption::Threads, parse_u32),
    ))
    .context(StrContext::Label("global_option"))
    .parse_next(input)
}

pub fn parse_positional(input: &mut &'_ str) -> PResult<PositionalOption> {
    literal("nope")
        .value(PositionalOption::XDev)
        .context(StrContext::Label("positional_option"))
        .parse_next(input)
}

pub fn parse_action(input: &mut &'_ str) -> PResult<Action> {
    alt((
        parse_unary_pretty!("-fls", Action::FileList, parse_string),
        parse_unary_pretty!("-fprint0", Action::FilePrintNull, parse_string),
        //parse_type_into!("-fprintf", Action::FilePrintFormatted, parse_string),
        parse_unary_pretty!("-fprint", Action::FilePrint, parse_string),
        terminated("-ls", multispace0).value(Action::List),
        parse_unary_pretty!("-printf", Action::PrintFormatted, parse_string),
        terminated("-print0", multispace0).value(Action::PrintNull),
        terminated("-print", multispace0).value(Action::Print),
        terminated("-prune", multispace0).value(Action::Prune),
        terminated("-quit", multispace0).value(Action::Quit),
    ))
    .context(StrContext::Label("action"))
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
    .context(StrContext::Label("test"))
    .parse_next(input)
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
    preceded(
        multispace0,
        repeat_till(1.., terminated(token, multispace0), eof),
    )
    .map(|(tks, _)| tks)
    .parse_next(input)
}

pub fn token(input: &mut &str) -> PResult<Token> {
    alt((
        literal("(").value(Token::LParen),
        literal(")").value(Token::RParen),
        literal("!").value(Token::Not),
        literal(",").value(Token::Comma),
        // We need the termination clause to ensure the start of an option does not get picked up
        // as an expression, eg `-atime` does not become `[Token::And, "time"]`
        terminated(alt(("-or", "-o")), alt((multispace1, eof))).value(Token::Or),
        terminated(alt(("-and", "-a")), alt((multispace1, eof))).value(Token::And),
        parse_test.map(Token::Test),
        parse_action.map(Token::Action),
        parse_global_option.map(Token::Global),
        parse_positional.map(Token::Positional),
        fail.context(StrContext::Expected(StrContextValue::Description(
            "invalid_token",
        ))),
    ))
    .context(StrContext::Label("token"))
    .parse_next(input)
}

mod precedence {
    use crate::winnow_find_tokenized::{Exp, Ope, Test, Token};
    use std::rc::Rc;
    use winnow::combinator::{alt, cut_err, delimited, fail, preceded, repeat};
    use winnow::error::{StrContext, StrContextValue};
    use winnow::{prelude::Parser, token::one_of, PResult};

    /// Precedence parser entry point
    pub fn parser(input: &mut &[Token]) -> PResult<Exp> {
        list.context(StrContext::Label("grammar")).parse_next(input)
    }

    /// Lowest priority operator, if a `Token::Comma` is encountered in the token list we wait until
    /// either side of it are resolved to expressions.
    fn list(input: &mut &[Token]) -> PResult<Exp> {
        let init = or.parse_next(input)?;

        repeat(0.., (one_of(Token::Comma), or))
            .fold(
                move || init.clone(),
                |acc, (_, val): (Token, Exp)| Exp::Operator(Rc::new(Ope::List(acc, val))),
            )
            .parse_next(input)
    }

    /// Higher priority than list, but not as high as Token::And. We parse either side first.
    fn or(input: &mut &[Token]) -> PResult<Exp> {
        let init = and.parse_next(input)?;

        repeat(0.., preceded(one_of(Token::Or), and))
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
        repeat(0.., alt((preceded(one_of(Token::And), atom), atom)))
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
            fail.context(StrContext::Expected(StrContextValue::Description(
                "invalid_syntax",
            ))),
        ))
        .parse_next(input)
    }

    /// Not operator before an atom.
    fn not(input: &mut &[Token]) -> PResult<Exp> {
        preceded(one_of(Token::Not), atom)
            .map(|val| Exp::Operator(Rc::new(Ope::Not(val))))
            .parse_next(input)
    }

    /// An expression delimited by parenthesis.
    fn parens(input: &mut &[Token]) -> PResult<Exp> {
        delimited(
            one_of(Token::LParen),
            cut_err(list).context(StrContext::Expected(StrContextValue::Description(
                "expression",
            ))),
            cut_err(one_of(Token::RParen)).context(StrContext::Expected(
                StrContextValue::Description("right_parenthesis"),
            )),
        )
        .context(StrContext::Label("parens"))
        .parse_next(input)
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
    }
}

fn _parse(input: &mut &str) -> PResult<(RunOptions, Exp)> {
    // Parse all the global options at the start of the command line
    let mut globals = RunOptions::default();
    winnow::Parser::<&str, Vec<GlobalOption>, winnow::error::ContextError>::parse_next(
        &mut preceded(
            multispace0,
            repeat(0.., terminated(parse_global_option, multispace0)),
        ),
        input,
    )?
    .iter()
    .for_each(|g: &GlobalOption| globals.update(g));

    // Tokenize the rest of the command line. If empty, we had only options and we insert True
    let tokens = if input.is_empty() {
        vec![Token::Test(Test::True)]
    } else {
        lex.parse_next(input)?
    };

    // Look through the token list to handle global options in the wrong order. Replace them by
    // True to minimize the impact on expected execution.
    let tokens: Vec<Token> = tokens
        .into_iter()
        .enumerate()
        .map(|(i, t)| match t {
            Token::Global(v) => {
                log::warn!(
                    "Found misplaced global option in command line: option {:?} at logical position {i}",
                    v
                );
                globals.update(&v);
                Token::Test(Test::True)
            }
            token => token,
        })
        .collect();

    log::debug!("Tokens: {:?}", tokens);

    // Transform the token list to AST
    Ok((
        globals,
        precedence::parser.parse_next(&mut tokens.as_slice())?,
    ))
}

pub fn parse<S: AsRef<str>>(input: S) -> PResult<(RunOptions, Exp)> {
    // Get a reference to the input to modify while parsing
    let mut input: &str = input.as_ref();

    match _parse(&mut input) {
        Err(e) => {
            let subject = e.clone();
            log::error!(
                "Error parsing input: {} ({:?})",
                parse_string.parse_peek(input)?.1,
                subject.into_inner().unwrap().context().collect::<Vec<_>>()
            );
            Err(e)
        }
        ok => ok,
    }
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
fn test_parse_comparison_error() {
    let res = parse_comp(&mut "not an int");
    assert!(res.is_err());

    let res = parse_comp(&mut "@44");
    assert!(res.is_err());
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

    for (operator, expected_token) in vec![
        ("-a", Token::And),
        ("-and", Token::And),
        ("-o", Token::Or),
        ("-or", Token::Or),
    ] {
        let res = token(&mut format!("{}", operator).as_str());
        assert_eq!(res, Ok(expected_token.clone()));

        // This case should be handled by the repeat combinator, and no token should have leading
        // whitespace. Could be enabled but would complicate the parsing.
        //let res = token(&mut format!(" {}", operator).as_str());
        //assert_eq!(res, Ok(expected_token.clone()));

        let res = token(&mut format!("{} ", operator).as_str());
        assert_eq!(res, Ok(expected_token.clone()));
    }
}

#[test]
fn test_token_error() {
    let res = token(&mut "-notanoption");
    assert!(res.is_err());
}

#[test]
fn test_lex_action() {
    let res = lex(&mut "-print");
    assert_eq!(res, Ok(vec![Token::Action(Action::Print)]));

    let res = lex(&mut "-fprint test.out");
    assert_eq!(
        res,
        Ok(vec![Token::Action(Action::FilePrint(String::from(
            "test.out"
        )))])
    );

    let res = lex(&mut "-fprint0 test.out");
    assert_eq!(
        res,
        Ok(vec![Token::Action(Action::FilePrintNull(String::from(
            "test.out"
        )))])
    );
}

#[test]
fn test_lex_action_error() {
    let res = lex(&mut "-printf");
    assert!(res.is_err());

    let res = lex(&mut "-fprint");
    assert!(res.is_err());

    let res = lex(&mut "-fprint0");
    assert!(res.is_err());
}

#[test]
fn test_lex() {
    let res = lex(&mut "-atime 77").unwrap();
    assert_eq!(
        res,
        vec![Token::Test(Test::AccessTime(Comparison::Equal(77)))]
    );

    let res = lex(&mut "! -atime 77 ( -name test )").unwrap();
    assert_eq!(
        res,
        vec![
            Token::Not,
            Token::Test(Test::AccessTime(Comparison::Equal(77))),
            Token::LParen,
            Token::Test(Test::Name(String::from("test"))),
            Token::RParen,
        ]
    );

    let res = lex(&mut "-true -a -false").unwrap();
    assert_eq!(
        res,
        vec![
            Token::Test(Test::True),
            Token::And,
            Token::Test(Test::False)
        ]
    );
}

#[test]
/// Asserts the parser does not try to recover and return a partial list on error
fn test_lex_error() {
    let res = lex(&mut "-anerr param -name test");
    assert!(res.is_err());

    let res = lex(&mut "-name test -anerr param");
    assert!(res.is_err());
}
