#![feature(string_remove_matches)]
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

/// stdinからEOFまで各行を行毎に読み込み、指定された関数を実行していく関数
fn run_with_loadedVal(mut stdin: io::Stdin, mut f: impl FnMut(&String)) {
    let mut buf = String::new();

    while let Ok(bytes) = stdin.read_line(&mut buf) {
	if bytes == 0 {
	    break;
	}
	f(&buf);
	buf.clear();
    }
}

pub fn run(mut stdin: io::Stdin, mut writer: impl Write,config: Config) {
    let mut buf = String::new();

    match config.mode {
	TrMode::Replace(s1, s2) => {
	    run_with_loadedVal(stdin, |s| {
		writer.write_all(s.replace(&s1, s2.as_str()).as_bytes());
	    })
	},
	TrMode::Delete(target) => {
	    run_with_loadedVal(stdin, |s| {
		let mut buf = s.clone();
		buf.remove_matches(&target);
		writer.write_all(buf.as_bytes());
	    })
	},
	TrMode::SqueezeRepeats(s1) => {
	    run_with_loadedVal(stdin, |s| {
		let mut output_buffer = String::new();
		let mut charsIter = buf.chars();
		let mut previous_c = match charsIter.next() {
		    Some(c) => c,
		    None => {
			// no input
			return;
		    }
		};

		// previous_cはバッファに追加されていることを想定しているので、
		// 最初の文字も入れてあげる
		output_buffer.push(previous_c);
		for c in charsIter {
		    if previous_c != c || !s1.contains(c) {
			output_buffer.push(c);
			previous_c = c;
		    }
		}

		writer.write_all(output_buffer.as_bytes());
		output_buffer.clear();
	    })
	},
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

	#[test]
	fn new_noargs() {
	    assert!(Config::new(["binary name".to_string()].into_iter()).is_err());
	}
    }

}
