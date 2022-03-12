use std::io;
use std::io::Write;
use std::io::Read;

#[derive(PartialEq, Debug)]
enum TrMode {
    Replace(String, String),
    Delete(String),
    SqueezeRepeats(String),
}

#[derive(PartialEq, Debug)]
pub struct Config {
    mode: TrMode,
}

impl Config {
    pub fn new(mut args: impl Iterator<Item = String>) -> Result<Config, String> {
	args.next(); // 引数0はバイナリ名

	// 本体がスコープを外れてしまうため、
	// Some(String) を Some(&str) に変換することはできない
	// なので一度変数に束縛する
	let fst = match args.next() {
	    Some(s) => s,
	    None => return Err("Arguments are not given".to_string())
	};

	match fst.as_str() {
	    "-s" => {
		args.next()
		    .map(|set1| Config {mode: TrMode::SqueezeRepeats(set1) })
		    .ok_or("SET1 isn't given".to_string())
	    }
	    "-d" => {
		let set1 = args.next().unwrap_or("SET1 isn't given".to_string());
		Ok(Config{mode: TrMode::Delete(set1)})
	    }
	    set1 => {
		let set2 = args.next().unwrap_or("SET2 isn't given".to_string());
		Ok(Config{mode: TrMode::Replace(set1.to_string().clone(), set2)})
	    }
	}
    }
}

pub fn run(config: Config) {
    let mut stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut buf = String::new();

    match config.mode {
	TrMode::Replace(s1, s2) => {
	    while let Ok(bytes) = stdin.read_line(&mut buf) {
		if bytes == 0 {
		    break;
		}
		stdout.write_all(buf.replace(&s1, s2.as_str()).as_bytes());
		buf.clear();
	    }
	}
	_ => {

	}
    }
}


#[cfg(test)]
mod test {
    #[cfg(test)]
    mod config {
	use crate::Config;
	use crate::TrMode;

	#[test]
	fn new_correct() {
	    let args1 = ["binary name", "-d", "a"].map(|s| s.to_string()).into_iter();
	    let result1 = Config { mode: TrMode::Delete("a".to_string()) };

	    assert_eq!(Ok(result1), Config::new(args1));

	    let args2 = ["binary name", "-s", "a"].map(|s| s.to_string()).into_iter();
	    let result2 = Config { mode: TrMode::SqueezeRepeats("a".to_string()) };
	    assert_eq!(Ok(result2), Config::new(args2));

	    let args3 = ["binary name", "a", "b"].map(|s| s.to_string()).into_iter();
	    let result3 = Config { mode: TrMode::Replace("a".to_string(), "b".to_string()) };
	    assert_eq!(Ok(result3), Config::new(args3));
	    
	}
    }
}
