use std::env;
use std::io::prelude::*;
use std::fs::File;
use std::process;
use std::error::Error;

fn main() {
    let args: Vec<String> =  env::args().collect();
    let cfg = Config::new(&args).unwrap_or_else(|err| {
	println!("Problem parsing arguments: {}", err);
	process::exit(1);
    });

    if let Err(e) = run(cfg) {
	println!("Application error: {}", e);
	process::exit(1);
    };

}

fn run(config: Config) -> Result<(), Box<Error>> {
    let mut f = File::open(&config.filename)?;

    let mut content = String::new();
    f.read_to_string(&mut content)?;

    println!("{}: {}", config.filename, content);

    Ok(())
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
