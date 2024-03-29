use find_parser::parse;
use log::LevelFilter;
use simple_logger::SimpleLogger;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    //unsafe { backtrace_on_stack_overflow::enable() };

    SimpleLogger::new()
        .with_level(LevelFilter::Info)
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

    match parse(&args) {
        Ok(exp) => println!("{:?}", exp),
        Err(e) => eprintln!("{}", e.to_string()),
    }

    Ok(())
}
