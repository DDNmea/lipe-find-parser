use crate::ast::{
    Action, Comparison, Expression as Exp, FileType, GlobalOption, Operator as Ope, PermCheck,
    Permission, PositionalOption, Size, Test, TimeSpec,
};
use crate::{Mode, RunOptions};
use winnow::{
    ascii::{alpha1, digit1, multispace0, multispace1},
    combinator::{
        alt, cut_err, delimited, eof, fail, preceded, repeat, repeat_till, separated, terminated,
    },
    error::{StrContext, StrContextValue},
    prelude::*,
    token::{any, literal, one_of, take_until, take_while},
};

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

/// Trait used to add the ability to parse arbitrary types
trait Parseable {
    fn parse(input: &mut &str) -> PResult<Self>
    where
        Self: Sized;
}

/// Trait used to add the ability to parse arbitrary types, with a second argument called in case a
/// default categorization is needed
trait DefaultParseable<S> {
    fn parse(input: &mut &str, default: impl Fn(S) -> Self) -> PResult<Self>
    where
        Self: Sized;
}

impl Parseable for u32 {
    fn parse(input: &mut &str) -> PResult<u32> {
        digit1
            .try_map(|digit_str: &str| digit_str.parse::<u32>())
            .context(StrContext::Expected(StrContextValue::Description(
                "unsigned_integer",
            )))
            .parse_next(input)
    }
}

impl Parseable for u64 {
    fn parse(input: &mut &str) -> PResult<u64> {
        digit1
            .try_map(|digit_str: &str| digit_str.parse::<u64>())
            .context(StrContext::Expected(StrContextValue::Description(
                "unsigned_integer",
            )))
            .parse_next(input)
    }
}

impl Parseable for String {
    fn parse(input: &mut &str) -> PResult<String> {
        alt((
            delimited("'", take_until(0.., "'"), "'"),
            take_while(0.., |c| c != ' ' && c != '\n' && c != ')'),
        ))
        .context(StrContext::Expected(StrContextValue::Description("string")))
        .map(String::from)
        .parse_next(input)
    }
}

impl Parseable for Size {
    fn parse(input: &mut &str) -> PResult<Size> {
        alt((
            (u64::parse, one_of(|c| "bcwkMGT".contains(c))).map(|(num, unit)| match unit {
                'b' => Size::Block(num),
                'c' => Size::Byte(num),
                'w' => Size::Word(num),
                'k' => Size::KiloByte(num),
                'M' => Size::MegaByte(num),
                'G' => Size::GigaByte(num),
                'T' => Size::TeraByte(num),
                _ => unreachable!(),
            }),
            // Not very pretty, we check for a [0-9]+[a-z]+ and if met then fail with the proper
            // error. We do this once all the valid specs have been checked but before we attempt a
            // specless parse, doing so would end up leaving some junk in the input
            terminated(digit1, alpha1).and_then(cut_err(fail.context(StrContext::Expected(
                StrContextValue::Description("invalid_size_specifier"),
            )))),
            // Default. For Size this is Block
            u64::parse.map(Size::Block),
        ))
        .context(StrContext::Label("size"))
        .parse_next(input)
    }
}

impl DefaultParseable<u64> for TimeSpec {
    fn parse(input: &mut &str, default: impl Fn(u64) -> TimeSpec) -> PResult<TimeSpec> {
        alt((
            (u64::parse, one_of(|c| "smhd".contains(c))).map(|(num, unit)| match unit {
                's' => TimeSpec::Second(num),
                'm' => TimeSpec::Minute(num),
                'h' => TimeSpec::Hour(num),
                'd' => TimeSpec::Day(num),
                _ => unreachable!(),
            }),
            // Same as above
            terminated(digit1, alpha1).and_then(cut_err(fail.context(StrContext::Expected(
                StrContextValue::Description("invalid_time_specifier"),
            )))),
            // Here the default is the user-defined closure given as an argument
            u64::parse.map(default),
        ))
        .context(StrContext::Label("timespec"))
        .parse_next(input)
    }
}

/// Helper struct wrapping [TimeSpec] that when parsed will contain a value defaulting in
/// [TimeSpec::Minute].
struct MinDefault(TimeSpec);

impl Parseable for MinDefault {
    fn parse(input: &mut &str) -> PResult<MinDefault> {
        Ok(MinDefault(TimeSpec::parse(input, TimeSpec::Minute)?))
    }
}

impl Into<TimeSpec> for MinDefault {
    fn into(self) -> TimeSpec {
        self.0
    }
}

/// Helper struct wrapping [TimeSpec] that when parsed will contain a value defaulting in
/// [TimeSpec::Day].
struct DayDefault(TimeSpec);

impl Parseable for DayDefault {
    fn parse(input: &mut &str) -> PResult<DayDefault> {
        Ok(DayDefault(TimeSpec::parse(input, TimeSpec::Day)?))
    }
}

impl Into<TimeSpec> for DayDefault {
    fn into(self) -> TimeSpec {
        self.0
    }
}

impl Parseable for FileType {
    fn parse(input: &mut &str) -> PResult<FileType> {
        let invalid = cut_err(
            fail.context(StrContext::Expected(StrContextValue::Description(
                "invalid_type_specifier",
            ))),
        );

        alt((
            // If the type is more than one character long, error out
            take_while(2.., winnow::stream::AsChar::is_alpha).and_then(invalid),
            one_of(|c| "bcdpfls".contains(c)).map(|c| match c {
                'b' => FileType::Block,
                'c' => FileType::Character,
                'd' => FileType::Directory,
                'p' => FileType::Pipe,
                'f' => FileType::File,
                'l' => FileType::Link,
                's' => FileType::Socket,
                _ => unreachable!(),
            }),
            alpha1.and_then(cut_err(fail.context(StrContext::Expected(
                StrContextValue::Description("invalid_type_specifier"),
            )))),
        ))
        .parse_next(input)
    }
}

impl Parseable for Vec<FileType> {
    fn parse(input: &mut &str) -> PResult<Vec<FileType>> {
        separated(1.., FileType::parse, ",").parse_next(input)
    }
}

/// Element making up a symbolic permission definition.
///
/// The regex pattern is:
/// ```text
/// [ugoa]+[+=-][rwx]+
/// ```
///
/// The element in the center determines the type of the resulting object.
#[derive(PartialEq, Debug)]
enum PartialPermission {
    Set(Mode, Mode),
    Add(Mode),
    Del(Mode),
}

impl Permission {
    /// Returns a [crate::Mode] from a character.
    fn value(symbolic: char) -> Mode {
        match symbolic {
            'u' => Mode::S_IRWXU,
            'g' => Mode::S_IRWXG,
            'o' => Mode::S_IRWXO,
            'a' => Mode::S_IRWXU | Mode::S_IRWXG | Mode::S_IRWXO,
            'r' => Mode::S_IRUSR | Mode::S_IRGRP | Mode::S_IROTH,
            'w' => Mode::S_IWUSR | Mode::S_IWGRP | Mode::S_IWOTH,
            'x' => Mode::S_IXUSR | Mode::S_IXGRP | Mode::S_IXOTH,
            _ => unreachable!(),
        }
    }

    /// Parse a string type and return a Mode from all of the associated flags or'ed
    fn from_symbolic_str<S: AsRef<str>>(input: S) -> Option<Mode> {
        input
            .as_ref()
            .chars()
            .map(Permission::value)
            .reduce(|acc, e| acc | e)
    }
}

impl Parseable for PartialPermission {
    fn parse(input: &mut &str) -> PResult<PartialPermission> {
        let (target, operator, level) = (
            take_while(1.., |c| "ugoa".contains(c)),
            cut_err(one_of(|c| "+=-".contains(c)).context(StrContext::Expected(
                StrContextValue::Description("symbolic_permission_symbol"),
            ))),
            cut_err(
                take_while(1.., |c| "rwx".contains(c)).context(StrContext::Expected(
                    StrContextValue::Description("symbolic_permission_level"),
                )),
            ),
        )
            .parse_next(input)?;

        // We unwrap here, because the strings target and level have to contain the valid
        // characters at least one time
        let (target_mode, level_mode) = (
            Permission::from_symbolic_str(target).unwrap(),
            Permission::from_symbolic_str(level).unwrap(),
        );

        Ok(match operator {
            '=' => PartialPermission::Set(target_mode, level_mode),
            '+' => PartialPermission::Add(target_mode | level_mode),
            '-' => PartialPermission::Del(target_mode & !level_mode),
            _ => unreachable!(),
        })
    }
}

impl PartialPermission {
    fn update(&self, mode: Mode) -> Mode {
        match self {
            PartialPermission::Del(bits) => mode & bits.complement(),
            PartialPermission::Add(bits) => mode | *bits,
            PartialPermission::Set(target, level) => {
                let neg = mode & target.complement();
                neg | (*target & *level)
            }
        }
    }
}

impl Parseable for Permission {
    fn parse(input: &mut &str) -> PResult<Permission> {
        alt((
            separated(1.., PartialPermission::parse, ",")
                .map(|v: Vec<PartialPermission>| {
                    v.iter()
                        .fold(Mode::from_bits(0).unwrap(), |acc, e| e.update(acc))
                })
                .map(|m| Permission(m)),
            take_while(3.., |c| "01234567".contains(c))
                .map(|oct| u32::from_str_radix(oct, 8).unwrap())
                .map(|bits| Permission(Mode::from_bits(bits).unwrap())),
        ))
        .parse_next(input)
    }
}

impl Parseable for PermCheck {
    fn parse(input: &mut &str) -> PResult<PermCheck> {
        cut_err(alt((
            preceded("/", Permission::parse).map(PermCheck::Any),
            preceded("-", Permission::parse).map(PermCheck::AtLeast),
            cut_err(Permission::parse).map(PermCheck::Equal),
        )))
        .context(StrContext::Label("permission_comparison"))
        .parse_next(input)
    }
}

fn parse_comp_format<Typ, Def>(input: &mut &'_ str) -> PResult<Comparison<Typ>>
where
    Def: Parseable,
    Def: Into<Typ>,
{
    let outer = cut_err(alt((
        preceded("+", Def::parse).map(Comparison::GreaterThan),
        preceded("-", Def::parse).map(Comparison::LesserThan),
        cut_err(Def::parse).map(Comparison::Equal),
    )))
    .context(StrContext::Label("comparison"))
    .parse_next(input)?;

    match outer {
        Comparison::Equal(p) => Ok(Comparison::Equal(p.into())),
        Comparison::GreaterThan(p) => Ok(Comparison::GreaterThan(p.into())),
        Comparison::LesserThan(p) => Ok(Comparison::LesserThan(p.into())),
    }
}

fn parse_comp<P>(input: &mut &'_ str) -> PResult<Comparison<P>>
where
    P: Parseable,
{
    parse_comp_format::<P, P>(input)
}

pub fn parse_global_option(input: &mut &'_ str) -> PResult<GlobalOption> {
    alt((
        literal("-depth").value(GlobalOption::Depth),
        unary!("-maxdepth", GlobalOption::MaxDepth, u32::parse),
        unary!("-mindepth", GlobalOption::MinDepth, u32::parse),
        unary!("-threads", GlobalOption::Threads, u32::parse),
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
        unary!("-fls", Action::FileList, String::parse),
        unary!("-fprint0", Action::FilePrintNull, String::parse),
        //unary!("-fprintf", Action::FilePrintFormatted, String::parse),
        unary!("-fprint", Action::FilePrint, String::parse),
        terminated("-ls", multispace0).value(Action::List),
        unary!("-printf", Action::PrintFormatted, String::parse),
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
            unary!(
                "-amin",
                Test::AccessMin,
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
                Test::ChangeMin,
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
            unary!("-gid", Test::GroupId, parse_comp::<u32>),
            unary!("-group", Test::Group, String::parse),
            unary!("-ilname", Test::InsensitiveLinkName, String::parse),
            unary!("-iname", Test::InsensitiveName, String::parse),
            unary!("-inum", Test::InodeNumber, parse_comp::<u32>),
            unary!("-ipath", Test::InsensitivePath, String::parse),
            unary!("-iregex", Test::InsensitiveRegex, String::parse),
            unary!("-links", Test::Links, parse_comp::<u64>),
            unary!(
                "-mmin",
                Test::ModifyMin,
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
            unary!("-size", Test::Size, parse_comp::<Size>),
            literal("-true").value(Test::True),
            unary!("-type", Test::Type, Vec::<FileType>::parse),
            unary!("-uid", Test::UserId, parse_comp::<u32>),
            unary!("-user", Test::User, String::parse),
            literal("-writable").value(Test::Writable),
        )),
    ))
    .context(StrContext::Label("test"))
    .parse_next(input)
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
        parse_test.map(Token::Test),
        parse_action.map(Token::Action),
        parse_global_option.map(Token::Global),
        parse_positional.map(Token::Positional),
        preceded(
            any,
            fail.context(StrContext::Expected(StrContextValue::Description(
                "invalid_token",
            ))),
        ),
    ))
    .context(StrContext::Label("token"))
    .parse_next(input)
}

/// Collection of parsers that operate on [crate::find_parser::Token] slices to do precedence parsing.
///
/// This parsing method will keep track of the last encountered operator to ensure
/// the operator precedence is respected in the final expression.
mod precedence {
    #[cfg(test)]
    use crate::find_parser::{Comparison, Test};
    use crate::find_parser::{Exp, Ope, Token};
    use std::rc::Rc;
    use winnow::combinator::{alt, cut_err, delimited, eof, fail, preceded, repeat, repeat_till};
    use winnow::error::{StrContext, StrContextValue};
    use winnow::token::any;
    use winnow::{prelude::Parser, token::one_of, PResult};

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
            cut_err(one_of(Token::RParen)).context(StrContext::Expected(
                StrContextValue::Description("missing_closing_parenthesis"),
            )),
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
    let res = parse_comp::<u32>(&mut "44").unwrap();
    assert_eq!(Comparison::Equal(44), res);

    let res = parse_comp::<u32>(&mut "+44").unwrap();
    assert_eq!(Comparison::GreaterThan(44), res);

    let res = parse_comp::<u32>(&mut "-44").unwrap();
    assert_eq!(Comparison::LesserThan(44), res);

    Ok(())
}

#[test]
fn test_parse_comparison_uint_error() {
    let res = parse_comp::<u32>(&mut "not an int");
    assert!(res.is_err());

    let res = parse_comp::<u32>(&mut "@44");
    assert!(res.is_err());
}

#[test]
fn test_parse_string() -> Result<(), Box<dyn std::error::Error>> {
    let res = String::parse(&mut "a_long_string").unwrap();
    assert_eq!(String::from("a_long_string"), res);

    let res = String::parse(&mut "a_long_string\n").unwrap();
    assert_eq!(String::from("a_long_string"), res);

    let res = String::parse(&mut "a_long_string another").unwrap();
    assert_eq!(String::from("a_long_string"), res);

    let res = String::parse(&mut "'a_long_string another' again").unwrap();
    assert_eq!(String::from("a_long_string another"), res);

    Ok(())
}

#[test]
fn test_parse_size() {
    let res = Size::parse(&mut "20");
    assert_eq!(res, Ok(Size::Block(20)));

    let res = Size::parse(&mut "20b");
    assert_eq!(res, Ok(Size::Block(20)));

    let res = Size::parse(&mut "200c");
    assert_eq!(res, Ok(Size::Byte(200)));

    let res = Size::parse(&mut "200w");
    assert_eq!(res, Ok(Size::Word(200)));

    let res = Size::parse(&mut "200k");
    assert_eq!(res, Ok(Size::KiloByte(200)));

    let res = Size::parse(&mut "200M");
    assert_eq!(res, Ok(Size::MegaByte(200)));

    let res = Size::parse(&mut "200G");
    assert_eq!(res, Ok(Size::GigaByte(200)));

    let res = Size::parse(&mut "200T");
    assert_eq!(res, Ok(Size::TeraByte(200)));
}

#[test]
fn test_parse_time() {
    let res = TimeSpec::parse(&mut "20s", TimeSpec::Day);
    assert_eq!(res, Ok(TimeSpec::Second(20)));

    let res = TimeSpec::parse(&mut "20m", TimeSpec::Day);
    assert_eq!(res, Ok(TimeSpec::Minute(20)));

    let res = TimeSpec::parse(&mut "20h", TimeSpec::Day);
    assert_eq!(res, Ok(TimeSpec::Hour(20)));

    let res = TimeSpec::parse(&mut "20d", TimeSpec::Day);
    assert_eq!(res, Ok(TimeSpec::Day(20)));

    let res = TimeSpec::parse(&mut "20", TimeSpec::Day);
    assert_eq!(res, Ok(TimeSpec::Day(20)));

    let res = TimeSpec::parse(&mut "20", TimeSpec::Hour);
    assert_eq!(res, Ok(TimeSpec::Hour(20)));
}

#[test]
fn test_parse_filetype() {
    let res = FileType::parse(&mut "s");
    assert_eq!(res, Ok(FileType::Socket));

    let res = FileType::parse(&mut "b");
    assert_eq!(res, Ok(FileType::Block));

    let res = FileType::parse(&mut "c");
    assert_eq!(res, Ok(FileType::Character));

    let res = FileType::parse(&mut "d");
    assert_eq!(res, Ok(FileType::Directory));

    let res = FileType::parse(&mut "p");
    assert_eq!(res, Ok(FileType::Pipe));

    let res = FileType::parse(&mut "f");
    assert_eq!(res, Ok(FileType::File));

    let res = FileType::parse(&mut "l");
    assert_eq!(res, Ok(FileType::Link));
}

#[test]
fn test_parse_filetype_list() {
    let res = Vec::<FileType>::parse(&mut "s,b,c");
    assert_eq!(
        res,
        Ok(vec![FileType::Socket, FileType::Block, FileType::Character])
    );

    let res = Vec::<FileType>::parse(&mut "d,p,f");
    assert_eq!(
        res,
        Ok(vec![FileType::Directory, FileType::Pipe, FileType::File])
    );

    let res = Vec::<FileType>::parse(&mut "l");
    assert_eq!(res, Ok(vec![FileType::Link]));
}

#[test]
fn test_parse_filetype_list_error() {
    let res = Vec::<FileType>::parse(&mut "sb,c");
    assert!(res.is_err());

    let res = Vec::<FileType>::parse(&mut "d,k");
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
fn test_parse_partial_permission() {
    let res = PartialPermission::parse(&mut "u=r");
    assert_eq!(
        res,
        Ok(PartialPermission::Set(
            Mode::S_IRWXU,
            Mode::S_IRUSR | Mode::S_IRGRP | Mode::S_IROTH
        ))
    );

    let res = PartialPermission::parse(&mut "go+wx");
    assert_eq!(
        res,
        Ok(PartialPermission::Add(
            Mode::S_IRWXG
                | Mode::S_IRWXO
                | Mode::S_IWUSR
                | Mode::S_IWGRP
                | Mode::S_IWOTH
                | Mode::S_IXUSR
                | Mode::S_IXGRP
                | Mode::S_IXOTH
        ))
    );

    let res = PartialPermission::parse(&mut "u-r");
    assert_eq!(
        res,
        Ok(PartialPermission::Del(Mode::S_IWUSR | Mode::S_IXUSR))
    );

    let res = PartialPermission::parse(&mut "a-r");
    assert_eq!(
        res,
        Ok(PartialPermission::Del(
            Mode::S_IWUSR
                | Mode::S_IXUSR
                | Mode::S_IWGRP
                | Mode::S_IXGRP
                | Mode::S_IWOTH
                | Mode::S_IXOTH
        ))
    );
}

#[test]
fn test_parse_partial_permission_error() {
    let res = PartialPermission::parse(&mut "u@r");
    assert!(res.is_err());
}

#[test]
fn test_parse_permission() {
    let res = Permission::parse(&mut "u-r,g-r,o-r");
    let equivalent = Permission::parse(&mut "a-r");
    assert_eq!(res, equivalent);
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
