mod filetype;
mod format;
mod permission;
mod precedence;
#[macro_use]
mod prelude;
mod size;
mod timespec;

use prelude::*;
use timespec::{DayDefault, MinDefault};

/// Parse a unary operator with the provided methods
///
/// [$identifier] is the command line identifier for the unary operator, [$transform]
/// is a method to apply to the result of [$parser]. Whitespace between the two elements is
/// automatically discarded.
///
/// The matched command line can be expressed as the following regex:
/// ```text
/// $tag\s+$parser
/// ```
///
/// On error the context will contain a label with the provided identifier.
macro_rules! unary {
    ($identifier:expr, $transform:expr, $parser:expr) => {
        preceded($identifier, cut_err(preceded(multispace1, $parser)))
            .context(StrContext::Label($identifier))
            .map($transform)
    };
}

fn parse_comp_format<T, D>(input: &mut &'_ str) -> PResult<Comparison<T>>
where
    D: Parseable + Into<T>,
{
    cut_err(alt((
        preceded("+", D::parse).map(|v| Comparison::GreaterThan(v.into())),
        preceded("-", D::parse).map(|v| Comparison::LesserThan(v.into())),
        cut_err(D::parse).map(|v| Comparison::Equal(v.into())),
    )))
    .context(StrContext::Label("comparison"))
    .parse_next(input)
}

impl<P> Parseable for Comparison<P>
where
    P: Parseable,
{
    fn parse(input: &mut &'_ str) -> PResult<Comparison<P>> {
        parse_comp_format::<P, P>(input)
    }
}

impl Parseable for GlobalOption {
    fn parse(input: &mut &'_ str) -> PResult<GlobalOption> {
        alt((
            literal("-depth").value(GlobalOption::Depth),
            unary!("-maxdepth", GlobalOption::MaxDepth, u32::parse),
            unary!("-mindepth", GlobalOption::MinDepth, u32::parse),
            unary!("-threads", GlobalOption::Threads, u32::parse),
        ))
        .context(StrContext::Label("global_option"))
        .parse_next(input)
    }
}

impl Parseable for PositionalOption {
    fn parse(input: &mut &'_ str) -> PResult<PositionalOption> {
        literal("nope")
            .value(PositionalOption::XDev)
            .context(StrContext::Label("positional_option"))
            .parse_next(input)
    }
}

impl Parseable for Action {
    fn parse(input: &mut &'_ str) -> PResult<Action> {
        alt((
            unary!("-fls", Action::FileList, String::parse),
            unary!("-fprint0", Action::FilePrintNull, String::parse),
            preceded(
                "-fprintf",
                cut_err(preceded(
                    multispace1,
                    separated_pair(
                        String::parse,
                        multispace1,
                        parse_string_stream!().and_then(|&mut mut out: &mut &str| {
                            Vec::<FormatElement>::parse(&mut out)
                        }),
                    ),
                )),
            )
            .map(|(f, t)| Action::FilePrintFormatted(f, t)),
            unary!("-fprint", Action::FilePrint, String::parse),
            terminated("-ls", multispace0).value(Action::List),
            preceded(
                "-printf",
                cut_err(preceded(
                    multispace1,
                    parse_string_stream!()
                        .and_then(|&mut mut out: &mut &str| Vec::<FormatElement>::parse(&mut out)),
                )),
            )
            .map(Action::PrintFormatted),
            terminated("-print0", multispace0).value(Action::PrintNull),
            terminated("-print", multispace0).value(Action::Print),
            terminated("-prune", multispace0).value(Action::Prune),
            terminated("-quit", multispace0).value(Action::Quit),
        ))
        .context(StrContext::Label("action"))
        .parse_next(input)
    }
}

impl Parseable for Test {
    fn parse(input: &mut &'_ str) -> PResult<Test> {
        alt((
            alt((
                unary!(
                    "-amin",
                    Test::AccessTime,
                    parse_comp_format::<TimeSpec, MinDefault>
                ),
                unary!("-anewer", Test::AccessNewer, String::parse),
                unary!(
                    "-atime",
                    Test::AccessTime,
                    parse_comp_format::<TimeSpec, DayDefault>
                ),
                unary!(
                    "-cmin",
                    Test::ChangeTime,
                    parse_comp_format::<TimeSpec, MinDefault>
                ),
                unary!("-cnewer", Test::ChangeNewer, String::parse),
                unary!(
                    "-ctime",
                    Test::ChangeTime,
                    parse_comp_format::<TimeSpec, DayDefault>
                ),
                literal("-empty").value(Test::Empty),
                literal("-executable").value(Test::Executable),
                literal("-false").value(Test::False),
                unary!("-fstype", Test::FsType, String::parse),
                unary!("-gid", Test::GroupId, Comparison::<u32>::parse),
                unary!("-group", Test::Group, String::parse),
                unary!("-ilname", Test::InsensitiveLinkName, String::parse),
                unary!("-iname", Test::InsensitiveName, String::parse),
                unary!("-inum", Test::InodeNumber, Comparison::<u32>::parse),
                unary!("-ipath", Test::InsensitivePath, String::parse),
                unary!("-iregex", Test::InsensitiveRegex, String::parse),
                unary!("-links", Test::Links, Comparison::<u64>::parse),
                unary!(
                    "-mmin",
                    Test::ModifyTime,
                    parse_comp_format::<TimeSpec, MinDefault>
                ),
                unary!("-mnewer", Test::ModifyNewer, String::parse),
                unary!(
                    "-mtime",
                    Test::ModifyTime,
                    parse_comp_format::<TimeSpec, DayDefault>
                ),
            )),
            alt((
                unary!("-name", Test::Name, String::parse),
                literal("-nouser").value(Test::NoGroup),
                literal("-nogroup").value(Test::NoUser),
                unary!("-path", Test::Path, String::parse),
                unary!("-perm", Test::Perm, PermCheck::parse),
                literal("-readable").value(Test::Readable),
                unary!("-regex", Test::Regex, String::parse),
                unary!("-samefile", Test::Samefile, String::parse),
                unary!("-size", Test::Size, Comparison::<Size>::parse),
                literal("-true").value(Test::True),
                unary!("-type", Test::Type, Vec::<FileType>::parse),
                unary!("-uid", Test::UserId, Comparison::<u32>::parse),
                unary!("-user", Test::User, String::parse),
                literal("-writable").value(Test::Writable),
            )),
        ))
        .context(StrContext::Label("test"))
        .parse_next(input)
    }
}

/// [Expression](crate::ast::Expression) most basic components.
///
/// Tokens are the elements that make up the result of the command line parsing's first pass.
///
/// The command line will be broken down in its most basic elements to prepare the precendence
/// climbing. The elements of this enum are all of the allowed syntax and all proper syntax checks
/// for tests and actions are done when generating this list.
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

/// Convert a string literal to a list of [Token].
///
/// This will attempt to consume the entire input.
pub fn lex(input: &mut &str) -> PResult<Vec<Token>> {
    preceded(
        multispace0,
        repeat_till(1.., terminated(token, multispace0), eof),
    )
    .map(|(tks, _)| tks)
    .parse_next(input)
}

/// Consume a single token from the input.
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
        Test::parse.map(Token::Test),
        Action::parse.map(Token::Action),
        GlobalOption::parse.map(Token::Global),
        PositionalOption::parse.map(Token::Positional),
        preceded(
            any,
            fail.context(StrContext::Expected(StrContextValue::Description(
                "invalid_token",
            ))),
        ),
    ))
    .context(StrContext::Label("syntax"))
    .parse_next(input)
}

fn _parse(input: &mut &str) -> PResult<(RunOptions, Exp)> {
    // Parse all the global options at the start of the command line
    let mut globals = RunOptions::default();
    winnow::Parser::<&str, Vec<GlobalOption>, winnow::error::ContextError>::parse_next(
        &mut preceded(
            multispace0,
            repeat(0.., terminated(GlobalOption::parse, multispace0)),
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

/// Entrypoint of the parsing module.
pub fn parse<S: AsRef<str>>(input: S) -> PResult<(RunOptions, Exp)> {
    // Get a reference to the input to modify while parsing
    let mut input: &str = input.as_ref();

    match _parse(&mut input) {
        Err(e) => {
            let subject = e.clone();
            // TODO transform the below context into a user-friendly description of the mistake
            log::error!(
                "Error parsing input: `{}` ({:?})",
                input,
                subject.into_inner().unwrap().context().collect::<Vec<_>>()
            );
            Err(e)
        }
        ok => ok,
    }
}

#[test]
fn test_parse_comparison_uint() -> Result<(), Box<dyn std::error::Error>> {
    let res = Comparison::<u32>::parse(&mut "44").unwrap();
    assert_eq!(Comparison::Equal(44), res);

    let res = Comparison::<u32>::parse(&mut "+44").unwrap();
    assert_eq!(Comparison::GreaterThan(44), res);

    let res = Comparison::<u32>::parse(&mut "-44").unwrap();
    assert_eq!(Comparison::LesserThan(44), res);

    Ok(())
}

#[test]
fn test_parse_comparison_uint_error() {
    let res = Comparison::<u32>::parse(&mut "not an int");
    assert!(res.is_err());

    let res = Comparison::<u32>::parse(&mut "@44");
    assert!(res.is_err());
}

#[test]
fn test_parse_comparison_format() {
    let res = parse_comp_format::<TimeSpec, MinDefault>(&mut "20s");
    assert_eq!(res, Ok(Comparison::Equal(TimeSpec::Second(20))));

    let res = parse_comp_format::<TimeSpec, MinDefault>(&mut "+20");
    assert_eq!(res, Ok(Comparison::GreaterThan(TimeSpec::Minute(20))));

    let res = parse_comp_format::<TimeSpec, DayDefault>(&mut "-20");
    assert_eq!(res, Ok(Comparison::LesserThan(TimeSpec::Day(20))));
}

#[test]
fn test_parse_size_error() {
    let res = Size::parse(&mut "not_a_size");
    assert!(res.is_err());
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
        Ok(Token::Test(Test::AccessTime(Comparison::Equal(
            TimeSpec::Day(77)
        ))))
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
        vec![Token::Test(Test::AccessTime(Comparison::Equal(
            TimeSpec::Day(77)
        )))]
    );

    let res = lex(&mut "! -atime 77 ( -name test )").unwrap();
    assert_eq!(
        res,
        vec![
            Token::Not,
            Token::Test(Test::AccessTime(Comparison::Equal(TimeSpec::Day(77)))),
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
