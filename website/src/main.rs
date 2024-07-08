use lipe_find_parser::{compile, parse};
use std::panic;
use wasm_bindgen::prelude::*;

fn main() {
    wasm_logger::init(wasm_logger::Config::new(log::Level::Trace));

    panic::set_hook(Box::new(|panic_info| {
        println!("panic occurred: {panic_info}");
    }));

    let _ = setup();

    log::info!("Init done !");
}

pub fn get_window_document() -> Result<(web_sys::Window, web_sys::Document), JsValue> {
    let window = web_sys::window().ok_or("no global `window` exists")?;
    let document = window
        .document()
        .ok_or("should have a document on window")?;
    Ok((window, document))
}

pub fn get_document() -> Result<web_sys::Document, JsValue> {
    Ok(get_window_document()?.1)
}

pub fn setup() -> Result<(), JsValue> {
    let document = get_document()?;

    let expression = document
        .query_selector("input#expression")?
        .ok_or("No expression input")?;

    {
        let closure = Closure::<dyn FnMut(_)>::new(move |_event: web_sys::Event| {
            let _ = error_reset();

            if let Err(val) = update() {
                let _ = error(val).is_err_and(|e| {
                    log::error!("Failed to display error: {}", e.as_string().unwrap());
                    true
                });
            }
        });

        expression.add_event_listener_with_callback("input", closure.as_ref().unchecked_ref())?;
        closure.forget();
    }

    let manual = document
        .query_selector("div#manual")?
        .ok_or("No manual spot")?;

    manual.set_inner_html(include_str!(concat!(env!("OUT_DIR"), "/manual.html")));

    update()
}

fn error_reset() -> Result<(), JsValue> {
    let document = get_document()?;

    let error_display = document
        .query_selector("div#error")?
        .ok_or("No AST div !")?;

    error_display.set_class_name("");
    Ok(())
}

fn error(message: JsValue) -> Result<(), JsValue> {
    let document = get_document()?;

    let error_display = document
        .query_selector("div#error")?
        .ok_or("No error div !")?;

    error_display.set_class_name("active");
    let error_contents = error_display
        .query_selector("div#contents")?
        .ok_or("No contents div !")?;

    error_contents.set_inner_html(&html_escape::encode_text(&message.as_string().unwrap()));

    Ok(())
}

use lipe_find_parser::Target;
use std::collections::HashMap;
use std::fmt::Write;
fn generate_html_table(map: &Option<HashMap<u32, Target>>) -> String {
    let mut html = String::new();

    // Start the HTML table
    html.push_str("  <tr>\n");
    html.push_str("    <th>Index</th>\n");
    html.push_str("    <th>Filename</th>\n");
    html.push_str("    <th>Terminator</th>\n");
    html.push_str("  </tr>\n");

    match map {
        Some(map) => {
            // Iterate over the HashMap and add rows to the table
            for (index, target) in map {
                html.push_str("<tr>\n");
                write!(html, "<td>{}</td>\n", index).unwrap();

                match target {
                    Target::Stdout(term) => {
                        let terminator =
                            term.map_or(String::new(), |c| format!("0x{:02x}", c as u8));
                        write!(
                            html,
                            "<td>stdout</td><td>'{}'</td>\n",
                            html_escape::encode_text(&terminator)
                        )
                        .unwrap();
                    }
                    Target::File(filename, term) => {
                        let terminator =
                            term.map_or(String::new(), |c| format!("0x{:02x}", c as u8));
                        write!(
                            html,
                            "<td>{}</td><td>'{}'</td>\n",
                            html_escape::encode_text(&filename),
                            html_escape::encode_text(&terminator),
                        )
                        .unwrap();
                    }
                }

                html.push_str("  </tr>\n");
            }
        }
        None => html.push_str("<tr><td>*</td><td>stdout</td><td>'\\n'</td>\n"),
    }

    html
}

fn update() -> Result<(), JsValue> {
    let document = get_document()?;

    let expression = document
        .query_selector("input#expression")?
        .ok_or("No expression input")?
        .dyn_into::<web_sys::HtmlInputElement>()?
        .value();

    let _ = expression.trim();

    let ast = document.query_selector("pre#ast")?.ok_or("No AST div !")?;
    let io_map = document
        .query_selector("table#io-map")?
        .ok_or("No IO map table !")?;
    let options = document
        .query_selector("pre#options")?
        .ok_or("No option div !")?;
    let scheme = document
        .query_selector("pre#scheme")?
        .ok_or("No scheme div !")?;

    if expression.is_empty() {
        ast.set_inner_html("");
        options.set_inner_html("");
        scheme.set_inner_html("");
        return Ok(());
    }

    log::info!("Updating with: {expression}");

    /*let tokens = document
    .query_selector("div#tokens")?
    .ok_or("No token div !")?;*/

    let (opt, exp) = parse(expression).map_err(|err| err.to_string())?;
    ast.set_inner_html(&html_escape::encode_text(&format!("{:#?}", exp)));
    options.set_inner_html(&format!("{:#?}", opt));

    let code = compile(&exp, &opt).map_err(|err| err.to_string())?;
    io_map.set_inner_html(&generate_html_table(&code.io_map()));
    scheme.set_inner_html(&html_escape::encode_text(&format!(
        "{}",
        code.scheme("[DEVICE PATH]")
    )));

    //let subtitle = document.query_selector("div.subtitle")?.ok_or("No subtitle div !")?;

    Ok(())
}
