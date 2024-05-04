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
            if let Err(val) = update() {
                log::debug!("Error updating: {:#?}", val);
            }
        });

        expression.add_event_listener_with_callback("input", closure.as_ref().unchecked_ref())?;
        closure.forget();
    }

    update()
}

fn update() -> Result<(), JsValue> {
    let document = get_document()?;

    let expression = document
        .query_selector("input#expression")?
        .ok_or("No expression input")?
        .dyn_into::<web_sys::HtmlInputElement>()?
        .value();

    if expression.is_empty() {
        return Ok(());
    }

    log::info!("Updating with: {expression}");

    /*let tokens = document
    .query_selector("div#tokens")?
    .ok_or("No token div !")?;*/

    let ast = document.query_selector("pre#ast")?.ok_or("No AST div !")?;
    let options = document
        .query_selector("pre#options")?
        .ok_or("No option div !")?;

    let scheme = document
        .query_selector("pre#scheme")?
        .ok_or("No scheme div !")?;

    let (opt, exp) = parse(expression).map_err(|err| err.to_string())?;
    ast.set_inner_html(&html_escape::encode_text(&format!("{:#?}", exp)));
    options.set_inner_html(&format!("{:#?}", opt));

    let code = compile(&exp, &opt);
    scheme.set_inner_html(&html_escape::encode_text(&format!(
        "{}",
        code("[DEVICE PATH]")
    )));

    //let subtitle = document.query_selector("div.subtitle")?.ok_or("No subtitle div !")?;

    Ok(())
}
