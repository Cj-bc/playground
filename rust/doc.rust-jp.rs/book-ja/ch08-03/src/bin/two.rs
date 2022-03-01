fn main() {
    let input = String::from("first");
    println!("original is '{}', pig_latin version is '{}'", input, pig_latin(&input))
}


fn pig_latin(original: &str) -> String {
    if original.starts_with(&['a', 'e', 'i', 'o', 'u']) {
	format!("{}-hay", original)
    } else {
	let first_letter = original.chars().nth(0);
	let mut o = String::from(original);
	o.remove(0);
	format!("{}{}ay", o, first_letter.unwrap())
    }
}
