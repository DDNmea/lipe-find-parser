use crate::find_parser::prelude::*;

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
