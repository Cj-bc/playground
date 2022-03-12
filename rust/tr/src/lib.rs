use std::env;
use std::io;
use std::io::Write;
use std::io::Read;

enum TrMode {
    Replace(String, String),
    Delete(String),
    SqueezeRepeats(String),
}

pub struct Config {
    mode: TrMode,
}

impl Config {
    pub fn new(mut args: env::Args) -> Result<Config, String> {
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
	    while let Ok(bytes) = stdin.read_to_string(&mut buf) {
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
