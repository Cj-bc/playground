use std::collections::HashMap;

fn main() {
    let input = "Add Sally to Engineering";
    let mut db: HashMap<String, Vec<String>> = HashMap::new();

    let inputs: Vec<_> = input.split(' ').collect();
    match inputs.get(0) {
	None => (),
	Some(&"Add") => add_entry(&mut db, inputs),
	Some(&"List") => list_entry(&mut db, inputs),
	_ => (),
    }
}

fn add_entry(db: &mut HashMap<String, Vec<String>>, i: Vec<&str>) {
}

fn list_entry(db: &mut HashMap<String, Vec<String>>, i: Vec<&str>) {
}
