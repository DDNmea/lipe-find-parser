use find_parser::{compile, parse};
use log::LevelFilter;
use simple_logger::SimpleLogger;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    SimpleLogger::new()
        .with_level(LevelFilter::Debug)
        .init()
        .unwrap();

    let args: String = std::env::args()
        .into_iter()
        .skip(1)
        .map(|string| match string.contains(' ') {
            true => format!("'{}'", string),
            false => string,
        })
        .collect::<Vec<_>>()
        .join(" ");
    log::debug!("Args: \"{}\"", args);

    let mut input = args.as_str();
    match parse(&mut input) {
        Ok((options, exp)) => println!(
            "Options: {:?}\nExpression: {:?}\nCode: {}",
            options,
            exp,
            compile(&exp, &options, "/mdt")
        ),
        Err(e) => eprintln!("{}", e),
    }

    Ok(())
}
