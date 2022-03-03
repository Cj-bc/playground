use std::io::prelude::*;
use std::fs::File;
use std::error::Error;

pub fn run(config: Config) -> Result<(), Box<Error>> {
    let mut f = File::open(&config.filename)?;

    let mut content = String::new();
    f.read_to_string(&mut content)?;

    println!("{}: {}", config.filename, content);

    Ok(())
}

pub struct Config {
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
