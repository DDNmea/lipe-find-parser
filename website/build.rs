use std::io::Write;

fn main() {
    // Tell Cargo that if the given file changes, to rerun this build script.
    println!("cargo::rerun-if-changed=../man.roff");

    let compiled = std::process::Command::new("groff")
        .args(["-Thtml", "-man", "-P", "-l", "../man.roff"])
        .output()
        .expect("Failed to compile ROFF to HTML")
        .stdout;

    let manual = scraper::Html::parse_document(&String::from_utf8(compiled).unwrap());

    let out_dir = std::env::var("OUT_DIR").unwrap_or(String::from("."));

    let body_select = scraper::Selector::parse("body").unwrap();
    let contents = manual.select(&body_select).next().unwrap();

    eprintln!("{}/manual.html", out_dir);
    let mut out = std::fs::OpenOptions::new()
        .read(false)
        .create(true)
        .write(true)
        .open(format!("{}/manual.html", out_dir))
        .unwrap();

    contents.child_elements().into_iter().skip(4).for_each(|c| {
        out.write(c.html().as_bytes()).unwrap();
    });
}
