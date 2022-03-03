use std::env;
use std::io::prelude::*;
use std::fs::File;
use std::process;

fn main() {
    let args: Vec<String> =  env::args().collect();
    let cfg = Config::new(&args).unwrap_or_else(|err| {
	println!("Problem parsing arguments: {}", err);
	process::exit(1);
    });

    let mut f = File::open(&cfg.filename).expect("Couldn't open file");

    let mut content = String::new();
    f.read_to_string(&mut content).expect("Couldn't read from file");

    println!("{}: {}", cfg.filename, content);
}


struct Config {
    query: String,
    filename: String,
}

impl Config {
    pub fn new(args: &[String]) -> Result<Config, &'static str> {
	if args.len() < 3 {
	    return Err("Not enough arguments");
	}

	let query = args[1].clone();
	let filename = args[2].clone();
	Ok(Config { query, filename })
    }
}
