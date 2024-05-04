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
