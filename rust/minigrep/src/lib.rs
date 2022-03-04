use std::env;
use std::io::prelude::*;
use std::fs::File;
use std::error::Error;

pub fn run(config: Config) -> Result<(), Box<dyn Error>> {
    let mut f = File::open(&config.filename)?;

    let mut content = String::new();
    f.read_to_string(&mut content)?;

    let result = if config.case_sensitive {
	search(&config.query, &content)
    } else {
	search_case_insensitive(&config.query, &content)
    };

    for line in result {
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

pub fn search_case_insensitive<'a>(query: &str, contents: &'a str) -> Vec<&'a str> {
    let mut result = vec![];
    for l in contents.lines() {
	if l.to_lowercase().contains(&query.to_lowercase()) {
	    result.push(l);
	}
    }
    result
}

#[derive(PartialEq,Debug)]
pub struct Config {
    query: String,
    filename: String,
    case_sensitive: bool,
}

impl Config {
    pub fn new(args: &[String]) -> Result<Config, &'static str> {
	if args.len() < 3 {
	    return Err("Not enough arguments");
	}

	let case_sensitive = env::var("CASE_INSENSITIVE").is_err();
	let query = args[1].clone();
	let filename = args[2].clone();
	Ok(Config { query, filename, case_sensitive })
    }
}

#[cfg(test)]
mod test {
    #[cfg(test)]
    mod config {
	use super::super::*;
	#[test]
	fn new_success() {
	    assert_eq!(Ok(Config {query: String::from("a"), filename: String::from("b")
				  , case_sensitive: true})
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
	fn case_sensitive() {
	    let query = "duct";
	    let contents = "\
Rust:
safe, fast, productive.
Pick three.
Duct tape.";

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

	#[test]
	fn case_insensitive() {
	    let query = "rUsT";
	    let contents = "\
Rust:
safe, fast, productive.
Pick three.
Trust me.";
	    assert_eq!(vec!["Rust:", "Trust me."]
		       , search_case_insensitive(query, contents)
	    );

	}
    }
}
