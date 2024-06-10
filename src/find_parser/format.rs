#![allow(dead_code)]
use crate::find_parser::prelude::*;

impl Parseable for FormatSpecial {
    fn parse(input: &mut &str) -> PResult<FormatSpecial> {
        alt((
            // Check for all possible \X combinations
            preceded(
                "\\",
                alt((
                    take_while(3.., |c| "01234567".contains(c))
                        .map(|oct| u16::from_str_radix(oct, 8).unwrap())
                        .map(FormatSpecial::Ascii),
                    literal("0").value(FormatSpecial::Null),
                    literal("\\").value(FormatSpecial::Backslash),
                    literal("a").value(FormatSpecial::Alarm),
                    literal("b").value(FormatSpecial::Backspace),
                    literal("c").value(FormatSpecial::Clear),
                    literal("n").value(FormatSpecial::Newline),
                    literal("r").value(FormatSpecial::CarriageReturn),
                    literal("t").value(FormatSpecial::TabHorizontal),
                    literal("v").value(FormatSpecial::TabVertical),
                )),
            ),
            // No match, consume it as a regular backslash
            literal("\\").value(FormatSpecial::Backslash),
        ))
        .parse_next(input)
    }
}

impl Parseable for FormatField {
    fn parse(input: &mut &str) -> PResult<FormatField> {
        preceded(
            "%",
            alt((
                alt((
                    literal("%").value(FormatField::Percent),
                    literal("a").value(FormatField::Access),
                    literal("b").value(FormatField::DiskSizeBlocks),
                    literal("c").value(FormatField::Change),
                    literal("d").value(FormatField::Depth),
                    literal("D").value(FormatField::DeviceNumber),
                    literal("f").value(FormatField::Basename),
                    literal("F").value(FormatField::FsType),
                    literal("g").value(FormatField::Group),
                    literal("G").value(FormatField::GroupId),
                    literal("h").value(FormatField::Parents),
                    literal("H").value(FormatField::StartingPoint),
                    literal("i").value(FormatField::InodeDecimal),
                    literal("k").value(FormatField::DiskSizeKilos),
                    literal("l").value(FormatField::SymbolicTarget),
                    literal("m").value(FormatField::PermissionsOctal),
                    literal("M").value(FormatField::PermissionsSymbolic),
                    literal("n").value(FormatField::Hardlinks),
                    literal("p").value(FormatField::Name),
                    literal("P").value(FormatField::NameWithoutStartingPoint),
                    literal("s").value(FormatField::DiskSizeBytes),
                )),
                alt((
                    literal("S").value(FormatField::Sparseness),
                    literal("t").value(FormatField::Modify),
                    literal("u").value(FormatField::User),
                    literal("U").value(FormatField::UserId),
                    literal("y").value(FormatField::Type),
                    literal("Y").value(FormatField::TypeSymlink),
                    literal("Z").value(FormatField::SecurityContext),
                    literal("{fid}").value(FormatField::FileId),
                    literal("{projid}").value(FormatField::ProjectId),
                    literal("{mirror-count}").value(FormatField::MirrorCount),
                    literal("{stripe-count}").value(FormatField::StripeCount),
                    literal("{stripe-size}").value(FormatField::StripeSize),
                    preceded("A", any).map(FormatField::AccessFormatted),
                    preceded("C", any).map(FormatField::ChangeFormatted),
                    preceded("T", any).map(FormatField::ModifyFormatted),
                    delimited("{xattr:", alpha1, "}")
                        .map(|name: &str| FormatField::XAttr(String::from(name))),
                )),
                cut_err(fail.context(expected("invalid_format_specifier"))),
            )),
        )
        .parse_next(input)
    }
}

impl Parseable for Vec<FormatElement> {
    /// Parse a format string into a list of format string elements
    fn parse(input: &mut &str) -> PResult<Vec<FormatElement>> {
        // We can conceptualize a format string as a repetition of:
        // [literal]field|special, with a literal suffix. The below parser will repeat the first
        // matching as long as it can, then dump the rest in a literal.
        (
            // All of the literal + (field|special) pairs, folded neatly in a vec
            repeat(
                0..,
                // Match a string of length 0+ and a field or special
                repeat_till(
                    0..,
                    any,
                    alt((
                        FormatField::parse.map(FormatElement::Field),
                        FormatSpecial::parse.map(FormatElement::Special),
                    )),
                )
                // If the string was null, we ignore it
                .map(|(lit, el): (String, FormatElement)| match lit.len() {
                    0 => vec![el],
                    _ => vec![FormatElement::Literal(lit), el],
                }),
            )
            .fold(
                move || vec![],
                |mut acc, e| {
                    acc.extend(e);
                    acc
                },
            ),
            // The suffix
            repeat(0.., any),
        )
            .map(|(mut list, suffix): (Vec<FormatElement>, String)| {
                if !suffix.is_empty() {
                    list.push(FormatElement::Literal(suffix));
                }
                list
            })
            .context(expected("format_string"))
            .parse_next(input)
    }
}

#[test]
fn test_tokenize_format_string() {
    let res = Vec::<FormatElement>::parse(&mut "This is a %%Format%% string");
    assert_eq!(
        res,
        Ok(vec![
            FormatElement::Literal(String::from("This is a ")),
            FormatElement::Field(FormatField::Percent),
            FormatElement::Literal(String::from("Format")),
            FormatElement::Field(FormatField::Percent),
            FormatElement::Literal(String::from(" string")),
        ])
    );

    let res = Vec::<FormatElement>::parse(&mut "%%Format%%");
    assert_eq!(
        res,
        Ok(vec![
            FormatElement::Field(FormatField::Percent),
            FormatElement::Literal(String::from("Format")),
            FormatElement::Field(FormatField::Percent),
        ])
    );

    let res = Vec::<FormatElement>::parse(&mut "Special format: %%%%");
    assert_eq!(
        res,
        Ok(vec![
            FormatElement::Literal(String::from("Special format: ")),
            FormatElement::Field(FormatField::Percent),
            FormatElement::Field(FormatField::Percent),
        ])
    );

    let res = Vec::<FormatElement>::parse(&mut "No format.");
    assert_eq!(
        res,
        Ok(vec![FormatElement::Literal(String::from("No format.")),])
    )
}
