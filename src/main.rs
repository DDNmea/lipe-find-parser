use find_parser::parse_expression;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: String = std::env::args()
        .into_iter()
        .skip(1)
        .map(|string| match string.contains(' ') {
            true => format!("'{}'", string),
            false => string,
        })
        .collect::<Vec<_>>()
        .join(" ");

    match parse_expression(&args) {
        Ok((_, exp)) => println!("{:?}", exp),
        Err(e) => eprintln!("{}", e.to_string()),
    }

    Ok(())
}
