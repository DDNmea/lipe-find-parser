use find_parser::parse_expression;
use find_parser::Span;
use log::LevelFilter;
use nom_recursive::RecursiveInfo;
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

    match parse_expression(Span::new_extra(&args, RecursiveInfo::new())) {
        Ok((rem, exp)) => println!("{:?} - Remainder: '{}'", exp, rem),
        Err(e) => eprintln!("{}", e.to_string()),
    }

    Ok(())
}
