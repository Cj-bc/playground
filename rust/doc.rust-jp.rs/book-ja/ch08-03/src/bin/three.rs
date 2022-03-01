use std::collections::HashMap;

fn main() {
    let input = "Add Sally to Engineering";
    let mut db: HashMap<String, Vec<String>> = HashMap::new();

    let inputs: Vec<_> = i.split(' ').collect();
    match inputs[..] {
	["Add", name, "to", office] => {
	    add_entry(&mut db, &name, &office);
	},
	["List", office] => {
	    list_entry(&mut db, &office);
	}
	_ => (),
    }
}

fn add_entry(db: &mut HashMap<String, Vec<String>>, name: &str, office: &str) {
}

fn list_entry(db: &mut HashMap<String, Vec<String>>, office: &str) {
}
