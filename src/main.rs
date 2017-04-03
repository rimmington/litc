extern crate litc;

include!(concat!(env!("OUT_DIR"), "/magic.rs"));

const NUMBER : i32 = magic::linux::fs::FS_IOC_GETFLAGS;

// const WOO : i32 = magic::something::NUMBER;

fn main() {
    println!("{}", NUMBER);
}
