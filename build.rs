include!("src/lib.rs");

fn main() {
    let out_dir = std::env::var("OUT_DIR").unwrap();
    let dest_path = std::path::Path::new(&out_dir).join("magic.rs");
    let mut f = std::fs::File::create(&dest_path).unwrap();
    std::io::Write::write_all(&mut f, litc("src/main.rs", &out_dir).unwrap().as_bytes()).unwrap();
}
