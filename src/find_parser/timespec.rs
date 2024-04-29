use crate::find_parser::prelude::*;

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
pub struct MinDefault(TimeSpec);

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
pub struct DayDefault(TimeSpec);

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
