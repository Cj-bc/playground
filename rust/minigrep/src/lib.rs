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
    contents.lines().filter(|l| l.contains(query)).collect()
}

pub fn search_case_insensitive<'a>(query: &str, contents: &'a str) -> Vec<&'a str> {
    let query = query.to_lowercase();
    contents.lines().filter(|l| l.to_lowercase().contains(&query)).collect()
}

#[derive(PartialEq,Debug)]
pub struct Config {
    query: String,
    filename: String,
    case_sensitive: bool,
}

impl Config {
    pub fn new(mut args:impl Iterator<Item = String>) -> Result<Config, &'static str> {
	args.next();

	let case_sensitive = env::var("CASE_INSENSITIVE").is_err();
	let query = match args.next() {
	    Some(arg) => arg,
	    None => return Err("Didn't get a query string"),
	};
	let filename = match args.next() {
	    Some(arg) => arg,
	    None => return Err("Didn't get a filename"),
	};
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
		       , Config::new([String::from("binary name")
				       , String::from("a")
				       , String::from("b")].into_iter()));
	}

	#[test]
	fn new_no_query_string() {
	    assert_eq!(Err("Didn't get a query string")
		       , Config::new([String::from("binary name")].into_iter()));
	}
	#[test]
	fn new_no_filename() {
	    assert_eq!(Err("Didn't get a filename")
		       , Config::new([String::from("binary name")
				       , String::from("b")].into_iter()));
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
