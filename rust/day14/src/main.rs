use std::collections::{HashMap, HashSet};
use std::fs;

type InsertionMap<'a> = HashMap<String, char>;

fn decode_polymer_template(steps: usize, template: &str, insertion_map: &InsertionMap) -> String {
    let mut template = String::from(template);
    for _ in 0..steps {
        let mut new_template = String::with_capacity(template.len() * 2);
        for i in 0..(template.len() - 1) {
            let pair = String::from_iter(template[i..(i + 2)].chars());
            let insertion_char = insertion_map[pair.as_str()];
            new_template.push(pair.chars().next().unwrap());
            new_template.push(insertion_char);
        }
        new_template.push(template.chars().next_back().unwrap());
        template = new_template;
    }
    template
}


fn solve(template: &str) -> isize {
    let unique_chars: HashSet<char> = HashSet::from_iter(template.chars());
    
    let unique_counts = unique_chars.iter().map(|c| (c, template.chars().filter(|c1| c1 == c).count()));
    let max = unique_counts.clone().max_by_key(|(_, count)| *count).unwrap().1;
    let min = unique_counts.clone().min_by_key(|(_, count)| *count).unwrap().1;
    max as isize - min as isize
}


fn parse_input_file(path: &str) -> (String, InsertionMap) {
    let content = fs::read_to_string(path).unwrap();
    let mut lines = content.lines();
    let template = lines.next().unwrap().to_string();
    let _empty_line = lines.next().unwrap();
    let insertion_map = InsertionMap::from_iter(
        lines
            .map(|line| line.split(' '))
            .map(|mut split| (split.next().unwrap(), split.next_back().unwrap()))
            .map(|(pair, insertion_char)| {
                (String::from(pair), insertion_char.chars().next().unwrap())
            }),
    );
    (template, insertion_map)
}

fn main() {
    let (template, insertion_map) = parse_input_file("input.txt");
    println!("Answer 1: {}", solve(&decode_polymer_template(10, &template, &insertion_map)));
    //println!("Answer 2: {}", solve(&decode_polymer_template(40, &template, &insertion_map)));
    // slow as fu**
}

mod tests {
    use super::InsertionMap;

    #[test]
    fn decoding_polymer_template() {
        let template = "NNCB";
        let insertion_map = InsertionMap::from_iter([
            (String::from("NN"), 'C'),
            (String::from("NC"), 'B'),
            (String::from("CB"), 'H'),
        ]);

        let decoded_template = super::decode_polymer_template(1, template, &insertion_map);
        assert_eq!(decoded_template, String::from("NCNBCHB"));
    }
}
