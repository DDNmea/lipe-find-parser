use crate::find_parser::prelude::*;

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
