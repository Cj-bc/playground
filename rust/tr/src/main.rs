extern crate tr;
use std::env;
use std::io;

use tr::Config;

fn main() {
    let config = Config::new(env::args()).unwrap();
    tr::run(io::stdin(), io::stdout(), config)
}

