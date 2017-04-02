extern crate litc;

include!(concat!(env!("OUT_DIR"), "/magic.rs"));

const NUMBER : i32 = magic::NUMBER;

fn main() {
    println!("{}", NUMBER);
}
