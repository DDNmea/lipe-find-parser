use lipe_find_parser::{compile, parse};
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

    let (options, exp) = parse(&mut input).map_err(|e| {
        eprintln!("{}", e.to_string());
        String::from("Failed to parse")
    })?;
    println!("Options: {:?}\nExpression: {:?}", options, exp);

    match compile(&exp, &options) {
        Ok(compiled) => {
            println!("Special IO: {:?}", compiled.io_map());
            println!("Scheme: {}", compiled.scheme("</path/to/device>"));
        }
        Err(e) => eprintln!("{e}"),
    }

    Ok(())
}
