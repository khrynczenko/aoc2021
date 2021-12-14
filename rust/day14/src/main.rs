use std::collections::{HashMap, HashSet};
use std::collections::hash_map::Entry;
use std::fs;

type Pair = (char, char);
type InsertionMap<'a> = HashMap<Pair, char>;

struct DecodingState {
    template: HashMap<Pair, usize>,
    character_count: HashMap<char, usize>,
}

fn add_new_pair(template: &mut HashMap<Pair, usize>, pair: Pair, times: usize) {
    if let Entry::Vacant(e) = template.entry(pair) {
        e.insert(times);
        return;
    }

    template.insert(pair, template[&pair] + times);
}

fn decode_new_pairs(pair: &Pair, insertion_map: &InsertionMap) -> (Pair, Pair) {
    let new_char = insertion_map[pair];
    ((pair.0, new_char), (new_char, pair.1))
}

fn perform_decoding_step(
    insertion_map: &InsertionMap,
    initial_state: DecodingState,
) -> DecodingState {
    let mut new_template = HashMap::new();
    let mut new_character_count = initial_state.character_count;
    for (key_pair, times) in initial_state.template.clone().into_iter() {
        let (pair1, pair2) = decode_new_pairs(&key_pair, insertion_map);
        add_new_pair(&mut new_template, pair1, times);
        add_new_pair(&mut new_template, pair2, times);
        match new_character_count.entry(pair1.1) {
            Entry::Vacant(e) => {
                e.insert(initial_state.template[&key_pair]);
            },
            Entry::Occupied(mut e) => {
                e.insert(e.get() + initial_state.template[&key_pair]);
            }
        }
    }

    DecodingState {
        template: new_template,
        character_count: new_character_count,
    }
}

fn decode_polymer_template(
    steps: usize,
    template: &str,
    insertion_map: &InsertionMap,
) -> DecodingState {
    let mut template_from_next = template.chars().clone();
    template_from_next.next();
    let pairs: Vec<Pair> = template.chars().zip(template_from_next).collect();
    let unique_pairs: HashSet<Pair> = HashSet::from_iter(pairs.iter().copied());
    let unique_chars: HashSet<char> = HashSet::from_iter(template.chars());

    let character_count: HashMap<char, usize> = unique_chars
        .into_iter()
        .map(|c| (c, template.chars().filter(|c2| c == *c2).count()))
        .collect();

    let template: HashMap<Pair, usize> = unique_pairs
        .into_iter()
        .map(|pair| (pair, pairs.iter().filter(|&&p| p == pair).count()))
        .collect();

    let mut decoding_state = DecodingState {
        template,
        character_count,
    };

    for _ in 0..steps {
        decoding_state = perform_decoding_step(insertion_map, decoding_state);
    }

    decoding_state
}

fn solve(steps: usize, insertion_map: &InsertionMap, template: &str) -> isize {
    let decoding_state = decode_polymer_template(steps, template, insertion_map);
    let max = *decoding_state
        .character_count
        .iter()
        .max_by_key(|(_, count)| *count)
        .unwrap()
        .1;
    let min = *decoding_state
        .character_count
        .iter()
        .min_by_key(|(_, count)| *count)
        .unwrap()
        .1;
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
                (
                    (
                        pair.chars().next().unwrap(),
                        pair.chars().next_back().unwrap(),
                    ),
                    insertion_char.chars().next().unwrap(),
                )
            }),
    );
    (template, insertion_map)
}

fn main() {
    let (template, insertion_map) = parse_input_file("input.txt");
    println!("Answer 1: {}", solve(10, &insertion_map, &template));

    println!("Answer 2: {}", solve(40, &insertion_map, &template));
}
