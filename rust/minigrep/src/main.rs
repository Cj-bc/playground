use std::env;
use std::io::prelude::*;
use std::fs::File;

fn main() {
    let args: Vec<String> =  env::args().collect();
    let query: &String = &args[1];
    let filename: &String = &args[2];

    let mut f = File::open(filename).expect("Couldn't open file");

    let mut content = String::new();
    f.read_to_string(&mut content).expect("Couldn't read from file");

    println!("{}: {}", filename, content);
}
