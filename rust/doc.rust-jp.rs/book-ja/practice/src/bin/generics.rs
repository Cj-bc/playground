fn main() {
    let input = ['a', '#', '0', 'Z'];
    println!("largest value is {}", largest_char(&input));
    let input = [1, 4, 10, 79, 1, 3, 5];
    println!("largest value is {}", largest_i32(&input));
}

fn largest_char(list: &[char]) -> &char {
    let mut largest = &list[0];

    for v in list.iter() {
	if *largest < *v {
	    largest = v;
	}
    }

    largest
}


fn largest_i32(list: &[i32]) -> &i32 {
    let mut largest = &list[0];

    for v in list.iter() {
	if *largest < *v {
	    largest = v;
	}
    }

    largest
}
