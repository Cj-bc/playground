#![feature(iter_intersperse)]
use std::env;
use std::io::prelude::*;
use std::fs::File;
use std::error::Error;
use std::collections::HashMap;
use std::str::FromStr;

pub fn run(config: Config) -> Result<(), Box<dyn Error>> {
    let mut f = File::open(&config.filename)?;

    let mut content = String::new();
    f.read_to_string(&mut content)?;

    let result = if config.case_sensitive {
	search(&config.query, &content, &config)
    } else {
	search_case_insensitive(&config.query, &content, &config)
    };

    for line in result {
	println!("{}", line);
    }

    Ok(())
}

/// Internal function so that I don't have to write almost same code twice
/// in search and search_case_insensitive
fn _search<'a, F>(string_converter: F, query: &str, contents: &'a str, config: &Config) -> Vec<&'a str>
where
    F: Fn(&str) -> String 
{
    let query = string_converter(query);
    let lines: Vec<_> = contents.lines().collect();
    let found_match_idx: Vec<u32> = lines.iter().enumerate().fold(vec![], |mut store, (idx, line)| {
	if string_converter(line).contains(&query) {
	    store.push(idx as u32);
	};
	store
    });

    let result_lines = found_match_idx.into_iter()
	.map(|idx| (idx - config.before_context)..(idx + config.after_context + 1)) // before/after context ids
	.map(|range| range.filter_map(|idx| lines.get(idx as usize))
	     .into_iter()
	     .collect::<Vec<&&str>>());

    if config.before_context == 0 && config.after_context == 0 {
	result_lines
	    .flatten()
	    .map(|val| *val)
	    .collect()
    } else {
	// Append '--' between each context
	result_lines
	    .intersperse(vec![&&"--"])
	    .flatten()
	    .map(|val| *val)
	    .collect()
    }
}

pub fn search<'a>(query: &str, contents: &'a str, config: &Config) -> Vec<&'a str> {
    _search(|x| x.to_string(), query, contents, config)
}

pub fn search_case_insensitive<'a>(query: &str, contents: &'a str, config: &Config) -> Vec<&'a str> {
    _search(|x| x.to_lowercase(), query, contents, config)
}

#[derive(PartialEq,Debug)]
pub struct Config {
    query: String,
    filename: String,
    case_sensitive: bool,
    before_context: u32,
    after_context: u32,
}

impl Config {
    fn convert_option<F, T, E>(options: &HashMap<String, String>, reader: F, default: T, opt_name: &str) -> T
    where
	F: Fn(&str) -> Result<T, E>
    {
	match options.get(opt_name) {
	    Some(val) => reader(val).unwrap_or(default),
	    None => default,
	}
    }

    pub fn new(args:impl Iterator<Item = String>) -> Result<Config, &'static str> {
	let (options, mut args) = Config::separate_options_and_args(args);

	let before_context = Config::convert_option(&options, u32::from_str, 0, "--before");
	let after_context = Config::convert_option(&options, u32::from_str, 0, "--after");

	let case_sensitive = env::var("CASE_INSENSITIVE").is_err();
	let query = match args.next() {
	    Some(arg) => arg,
	    None => return Err("Didn't get a query string"),
	};
	let filename = match args.next() {
	    Some(arg) => arg,
	    None => return Err("Didn't get a filename"),
	};
	Ok(Config { query, filename, case_sensitive, before_context, after_context })
    }

    pub fn separate_options_and_args(args: impl Iterator<Item = String>)
					 -> (HashMap<String, String>, impl Iterator<Item = String>) {
	let mut searching_options = true;
	let mut options: HashMap<String, String> = HashMap::new();
	let mut arguments: Vec<String> = vec![];
	let mut next_option: Option<String> = None;
	for arg in args.skip(1) {
	    if let Some(opt) = next_option {
		options.insert(opt, arg.clone());
		next_option = None;
		continue;
	    }

	    if searching_options && &arg == "--" {
		searching_options = false;
		continue;
	    }

	    if arg.starts_with("--") && &arg != "--" {
		next_option = Some(arg);
	    } else {
		arguments.push(arg);
	    }
	};
	(options,arguments.into_iter())
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
				  , case_sensitive: true
				  , before_context: 0
				  , after_context: 0})
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

	#[test]
	fn new_flag_context() {
	    assert_eq!(Ok(Config {query: String::from("a")
				  , filename: String::from("b")
				  , case_sensitive: true
				  , before_context: 0
				  , after_context: 2
	    }), Config::new([ String::from("binary_name")
			    , String::from("--after")
			    , String::from("2")
			    , String::from("a")
			    , String::from("b")].into_iter()))
	}

	#[cfg(test)]
	mod convert_option {
	    use super::Config;
	    use std::collections::HashMap;
	    use std::str::FromStr;

	    #[test]
	    fn success() {
		assert_eq!(3
			   , Config::convert_option(&HashMap::from([("--after".to_string()
								     , "3".to_string())
								    , ("--before".to_string(), "9".to_string())])
						    , i32::from_str, 0, "--after"))
	    }
	}

	#[cfg(test)]
	mod separate_options_and_args {
	    use std::collections::HashMap;
	    use super::Config;

	    #[test]
	    fn with_dash() {
		let (opts, mut args) = Config::separate_options_and_args(vec!["binary_name".to_string()
									      , "--context".to_string()
									      , "2".to_string()
									      , "--".to_string()
									      , "target".to_string()
									      , "filename".to_string()].into_iter());
		assert_eq!(HashMap::from([("--context".to_string()
					   , "2".to_string())])
			   , opts);
		assert_eq!(Some("target".to_string()), args.next());
		assert_eq!(Some("filename".to_string()), args.next());
		assert_eq!(None, args.next());
	    }

	    #[test]
	    fn without_dash() {
		let (opts, mut args) = Config::separate_options_and_args(vec!["binary_name".to_string()
									      , "--context".to_string()
									      , "2".to_string()
									      , "target".to_string()
									      , "filename".to_string()].into_iter());
		assert_eq!(HashMap::from([("--context".to_string(), "2".to_string())])
			   , opts);
		assert_eq!(Some("target".to_string()), args.next());
		assert_eq!(Some("filename".to_string()), args.next());
		assert_eq!(None, args.next());
	    }

	    #[test]
	    fn without_options() {
		let (opts, mut args) = Config::separate_options_and_args(vec!["binary_name".to_string()
									      , "target".to_string()
									      , "filename".to_string()].into_iter());
		assert_eq!(HashMap::new(), opts);
		assert_eq!(Some("target".to_string()), args.next());
		assert_eq!(Some("filename".to_string()), args.next());
	    }

	    #[test]
	    fn without_arguments() {
		let (opts, mut args) = Config::separate_options_and_args(vec!["binary_name".to_string()
									      , "--context".to_string()
									      , "2".to_string()].into_iter());
		assert_eq!(HashMap::from([("--context".to_string(), "2".to_string())]), opts);
		assert_eq!(None, args.next());
	    }
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
		search(query, contents, &Config::new([String::from("binary name")
						      , String::from("regex")
						      , String::from("filename")
		].into_iter()).unwrap())
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
	    assert_eq!(expect, search(query, contents, &Config::new([String::from("binary name")
								     , String::from("--after")
								     , String::from("2")
								     , String::from("regex")
								     , String::from("filename")
	    ].into_iter()).unwrap()));
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
		       , search_case_insensitive(query, contents, &Config::new([String::from("binary name")
								     , String::from("regex")
								     , String::from("filename")
	    ].into_iter()).unwrap())
	    );

	}

	#[cfg(test)]
	mod options {
	    #[cfg(test)]
	    mod after_context {
		use crate::Config;
		use crate::search;

		#[test]
		fn no_overlap() {
		    let query = "one:";
		    let content = "\
one: level
foo
bar
baz
one: another
";
		    let args = [String::from("binary name")
				, String::from("--after")
				, String::from("3")
				, String::from("regex")
				, String::from("filename")
		    ];

		    assert_eq!(vec![String::from("one: level")
				    , String::from("foo")
				    , String::from("bar")
				    , String::from("baz")
				    , String::from("--")
				    , String::from("one: another")
				    ], search(&query, &content, &Config::new(args.into_iter()).unwrap()));
		}

		/// Contextの範囲が被った場合、そこの間には"--"を挟まない
		#[test]
		fn overlap() {
		    let query = "one:";
		    let content = "\
one: level
foo
bar
one: another
";
		    let args = [String::from("binary name")
				, String::from("--after")
				, String::from("3")
				, String::from("regex")
				, String::from("filename")
		    ];

		    assert_eq!(vec![String::from("one: level")
				    , String::from("foo")
				    , String::from("bar")
				    , String::from("one: another")
				    ], search(&query, &content, &Config::new(args.into_iter()).unwrap()));
		}


	    }
	}
    }

}
