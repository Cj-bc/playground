use std::io::prelude::*;
use std::fs::File;
use std::error::Error;

pub fn run(config: Config) -> Result<(), Box<dyn Error>> {
    let mut f = File::open(&config.filename)?;

    let mut content = String::new();
    f.read_to_string(&mut content)?;

    for line in search(&config.query, &content) {
	println!("{}", line);
    }

    Ok(())
}

pub fn search<'a>(query: &str, contents: &'a str) -> Vec<&'a str> {
    let mut result = vec![];
    for l in contents.lines() {
	if l.contains(query) {
	    result.push(l);
	}
    }
    result


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
    #[cfg(test)]
    mod config {
	use super::super::*;
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

    #[cfg(test)]
    mod query {
	use super::super::*;
	#[test]
	fn one_result() {
	    let query = "duct";
	    let contents = "\
Rust:
safe, fast, productive.
Pick three.";

	    assert_eq!(
		vec!["safe, fast, productive."],
		search(query, contents)
	    );
	}

	#[test]
	fn no_result() {
	    let query = "DOESNOTEXIST";
	    let contents = "\
Rust:
safe, fast, productive.
Pick three.";

	    let expect: Vec<&str> = vec![];
	    assert_eq!(expect, search(query, contents));
	}
    }
}
