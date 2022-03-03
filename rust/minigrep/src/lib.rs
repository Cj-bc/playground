use std::io::prelude::*;
use std::fs::File;
use std::error::Error;

pub fn run(config: Config) -> Result<(), Box<dyn Error>> {
    let mut f = File::open(&config.filename)?;

    let mut content = String::new();
    f.read_to_string(&mut content)?;

    println!("{}: {}", config.filename, content);

    Ok(())
}

#[derive(PartialEq,Debug)]
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

#[cfg(test)]
mod test {
    mod Config {
	use super::super::Config;
	#[test]
	fn new_success() {
	    assert_eq!(Ok(Config {query: String::from("a"), filename: String::from("b")})
		       , Config::new(&[String::from("binary name")
				       , String::from("a")
				       , String::from("b")]));
	}

	#[test]
	fn new_not_enough_arguments() {
	    assert_eq!(Err("Not enough arguments")
		       , Config::new(&[String::from("binary name")
				       , String::from("b")]));
	}

    }
}
