extern crate tr;
use std::env;

use tr::Config;

fn main() {
    let config = Config::new(env::args()).unwrap();
    tr::run(config)
}

