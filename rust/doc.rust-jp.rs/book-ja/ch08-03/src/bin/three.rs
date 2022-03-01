use std::collections::HashMap;

fn main() {
    let mut db: HashMap<String, Vec<String>> = HashMap::new();
    interpreter(&mut db, String::from("Add Sally to Engineering"));
    interpreter(&mut db, String::from("List Engineering"));
}

fn interpreter(db: &mut HashMap<String, Vec<String>>, i: String) {
    let inputs: Vec<_> = i.split(' ').collect();
    match inputs[..] {
	["Add", name, "to", office] => {
	    add_entry(db, &name, &office);
	},
	["List", office] => {
	    list_entry(db, &office);
	}
	_ => (),
    }

}

fn add_entry(db: &mut HashMap<String, Vec<String>>, name: &str, office: &str) {
    let entry: &mut Vec<String> = db.entry(office.to_string()).or_default();
    entry.push(String::from(name));
    println!("{} was added to the office {}", name, office);
}

fn list_entry(db: &mut HashMap<String, Vec<String>>, office: &str) {
    if let Some(names) = db.get(&String::from(office)) {
	println!("list of names in office '{}'", &office);
	for name in names.iter() {
	    println!("- {}", name);
	}
    }
}
