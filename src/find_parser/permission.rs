use crate::find_parser::prelude::*;
pub use crate::Mode;

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
            cut_err(one_of(|c| "+=-".contains(c)).context(expected("symbolic_permission_symbol"))),
            cut_err(
                take_while(1.., |c| "rwx".contains(c))
                    .context(expected("symbolic_permission_level")),
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
            '+' => PartialPermission::Add(target_mode & level_mode),
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
            take_while(3.., |c| "01234567".contains(c))
                .map(|oct| u32::from_str_radix(oct, 8).unwrap())
                .map(|bits| Permission(Mode::from_bits(bits).unwrap())),
            separated(1.., PartialPermission::parse, ",")
                .map(|v: Vec<PartialPermission>| {
                    v.iter()
                        .fold(Mode::from_bits(0).unwrap(), |acc, e| e.update(acc))
                })
                .map(|m| Permission(m)),
            fail.context(expected("invalid_permission_format")),
        ))
        .context(label("permission"))
        .parse_next(input)
    }
}

impl Parseable for PermCheck {
    fn parse(input: &mut &str) -> PResult<PermCheck> {
        alt((
            // We cut err in each as we have to get a permission at that point
            preceded("/", cut_err(Permission::parse)).map(PermCheck::Any),
            preceded("-", cut_err(Permission::parse)).map(PermCheck::AtLeast),
            cut_err(Permission::parse).map(PermCheck::Equal),
        ))
        .context(label("permission_comparison"))
        .parse_next(input)
    }
}

#[test]
fn test_parse_partial() {
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
            Mode::S_IWGRP | Mode::S_IWOTH | Mode::S_IXGRP | Mode::S_IXOTH
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
fn test_parse_partial_error() {
    let res = PartialPermission::parse(&mut "u@r");
    assert!(res.is_err());
}

#[test]
fn test_parse() {
    let res = Permission::parse(&mut "u-r,g-r,o-r");
    let equivalent = Permission::parse(&mut "a-r");
    assert_eq!(res, equivalent);
}

#[test]
fn test_parse_error() {
    let res = Permission::parse(&mut "7");
    assert!(res.is_err());

    let res = Permission::parse(&mut "u-r,u+e,u-x");
    assert!(res.is_err());
}
