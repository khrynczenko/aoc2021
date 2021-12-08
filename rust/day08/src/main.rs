use std::collections::HashSet;
use std::fs;

#[derive(Debug)]
struct Entry {
    pattern: Vec<String>,
    digits: Vec<String>,
}

fn parse_input_file(path: &str) -> Vec<Entry> {
    let content = fs::read_to_string(path).unwrap();
    content
        .lines()
        .map(|line| line.split('|'))
        .map(|mut split| (split.next().unwrap(), split.next().unwrap()))
        .map(|(signal, output)| {
            (
                signal.split(' ').take_while(|x| *x != ""),
                output.split(' ').skip(1),
            )
        })
        .map(|(signal, output)| (signal.map(|s| s.to_string()), output.map(|d| d.to_string())))
        .map(|(pattern, digits)| Entry {
            pattern: pattern.collect(),
            digits: digits.collect(),
        })
        .collect()
}

fn count_special_digits(entries: &[Entry]) -> usize {
    entries
        .iter()
        .map(|entry| &entry.digits)
        .map(|segments| segments.iter().filter(|segment| [2, 3, 4, 7].contains(&segment.len())))
        .map(|segments| segments.count())
        .sum()
}

fn main() {
    let entries = parse_input_file("input.txt");
    println!("Answer 1: {}", count_special_digits(&entries));
}
