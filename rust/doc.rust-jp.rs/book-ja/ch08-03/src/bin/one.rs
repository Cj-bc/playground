use std::collections::HashMap;

fn main() {
    let input = vec![5, 9, 3, 7, 4, 6, 1, 2, 8];
    println!("input is {:?}", input);
    println!("mean: {}", mean(&input));
    println!("median: {}", median(&input));
    println!("mode: {:?}", mode(&input).0);
}

fn mean(l: &Vec<usize>) -> usize {
    let mut sum = 0;
    for v in l.iter() {
	sum += v;
    }

    sum / l.len()
}

fn median(l: &Vec<usize>) -> usize {
    let mut l_ = l.clone();
    l_.sort();

    let middle_idx = l_.len()/2;

    if (l_.len() % 2) == 0 {
	l_[middle_idx]
    } else {
	(l_[middle_idx] + l_[middle_idx + 1])/2
    }
}

fn mode(l: &Vec<usize>) -> (usize, usize) {
    let mut map: HashMap<usize, usize> = HashMap::new();

    for v in l.iter() {
	let entry = map.entry(v.clone()).or_insert(0);
	*entry += 1;
    }

    map.into_iter().fold((0,0), |(max_idx, max_count), (current_idx, current_count)| {
	if max_count < current_count {
	    (current_idx, current_count)
	} else {
	    (max_idx, max_count)
	}
    })
}
