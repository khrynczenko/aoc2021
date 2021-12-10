use std::collections::{HashMap, HashSet};
use std::fs;
use std::hash::Hash;

const INPUT_SIGNALS_COUNT: usize = 10;

type Segment = char;
type Signal = HashSet<Segment>;

#[derive(Debug)]
struct Entry {
    input_signals: Vec<String>,
    output_signals: Vec<String>,
}

#[derive(Debug)]
struct EntrySets {
    input_signals: Vec<Signal>,
    output_signals: Vec<Signal>,
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
        .map(|(input, output)| (input.map(|s| s.to_string()), output.map(|d| d.to_string())))
        .map(|(input_signals, output_signals)| Entry {
            input_signals: input_signals.collect(),
            output_signals: output_signals.collect(),
        })
        .collect()
}

fn count_uniqely_segmented_signals(entries: &[Entry]) -> usize {
    entries
        .iter()
        .map(|entry| &entry.output_signals)
        .map(|segments| {
            segments
                .iter()
                .filter(|segment| [2, 3, 4, 7].contains(&segment.len()))
        })
        .map(|segments| segments.count())
        .sum()
}

fn obtain_n_segment_signal(n: usize, signals: &[Signal]) -> &Signal {
    signals
        .into_iter()
        .filter(|signal| signal.len() == n)
        .next()
        .unwrap()
}

fn decode_signal_mapping(input_signals: &[Signal]) -> HashMap<Segment, Segment> {
    assert!(input_signals.len() == INPUT_SIGNALS_COUNT);
    let mut map = HashMap::new();
    let one = obtain_n_segment_signal(2, input_signals);
    let seven = obtain_n_segment_signal(3, input_signals);
    let four = obtain_n_segment_signal(4, input_signals);
    let eight = obtain_n_segment_signal(7, input_signals);
    let five_segment_signals: Vec<Signal> = input_signals
        .into_iter()
        .filter(|signal| signal.len() == 5)
        .cloned()
        .collect();
    let six_segment_signals: Vec<Signal> = input_signals
        .into_iter()
        .filter(|signal| signal.len() == 6)
        .cloned()
        .collect();

    let a: Signal = seven.difference(&one).copied().collect();
    map.insert('a', *a.iter().next().unwrap());

    let d = intersect_many(five_segment_signals.iter().chain([four.clone()].iter()));
    map.insert('d', *d.iter().next().unwrap());

    let f = intersect_many(six_segment_signals.iter().chain([one.clone()].iter()));
    map.insert('f', *f.iter().next().unwrap());

    let c: Signal = one.difference(&f).copied().collect();
    map.insert('c', *c.iter().next().unwrap());

    let four_diff_one = HashSet::from_iter(four.clone().difference(&one).copied());
    let b: Signal = four_diff_one.difference(&d).copied().collect();
    map.insert('b', *b.iter().next().unwrap());

    let five_segment_signals_diff_a: Signal = intersect_many(five_segment_signals.iter())
        .difference(&a)
        .copied()
        .collect();
    let g: Signal = five_segment_signals_diff_a
        .difference(&d)
        .copied()
        .collect();
    map.insert('g', *g.iter().next().unwrap());

    let e: Signal = eight
        .difference(&union_many(&[a, b, c, d, f, g.clone()]))
        .copied()
        .collect();
    map.insert('e', *e.iter().next().unwrap());

    assert!(map.len() == 7);
    map.into_iter().map(|(key, value)| (value, key)).collect()
}

fn intersect_many<'a, I, T>(sets: I) -> HashSet<T>
where
    I: Iterator<Item = &'a HashSet<T>> + Clone,
    T: Eq + Hash + Clone + 'a,
{
    let mut sets = sets.peekable();
    let first = sets.next();
    if first.is_none() {
        return HashSet::new();
    }

    if sets.peek().is_none() {
        return first.unwrap().clone();
    }

    let acc = first.unwrap().clone();

    sets.fold(acc, |acc, set| {
        HashSet::from_iter(acc.intersection(&set).cloned())
    })
}

fn union_many<T>(sets: &[HashSet<T>]) -> HashSet<T>
where
    T: Eq + Hash + Clone,
{
    if sets.is_empty() {
        return HashSet::new();
    }

    if sets.len() == 1 {
        return sets.first().unwrap().clone();
    }

    let acc = sets.first().unwrap().clone();

    sets.iter()
        .fold(acc, |acc, set| HashSet::from_iter(acc.union(set).cloned()))
}

fn decode_signal(signal: Signal, map: HashMap<char, char>) -> char {
    let decoded_signal: Signal = signal
        .iter()
        .map(|s| map.get(s).unwrap())
        .copied()
        .collect();
    convert_signal_to_digit(decoded_signal)
}

fn decode_output(signals: &[Signal], map: HashMap<char, char>) -> usize {
    let digits: String = signals
        .iter()
        .map(|s| decode_signal(s.clone(), map.clone()))
        .collect();
    digits.parse().unwrap()
}

fn convert_signal_to_digit(signal: Signal) -> char {
    match signal {
        _ if signal == Signal::from_iter("abegfc".chars()) => '0',
        _ if signal == Signal::from_iter("cf".chars()) => '1',
        _ if signal == Signal::from_iter("acdeg".chars()) => '2',
        _ if signal == Signal::from_iter("acdfg".chars()) => '3',
        _ if signal == Signal::from_iter("bdcf".chars()) => '4',
        _ if signal == Signal::from_iter("abdfg".chars()) => '5',
        _ if signal == Signal::from_iter("abdefg".chars()) => '6',
        _ if signal == Signal::from_iter("acf".chars()) => '7',
        _ if signal == Signal::from_iter("abcdefg".chars()) => '8',
        _ if signal == Signal::from_iter("abcdfg".chars()) => '9',
        _ => unreachable!(),
    }
}

fn sum_digits(entries: &[Entry]) -> usize {
    let entry_sets: Vec<EntrySets> = entries
        .iter()
        .map(|entry| EntrySets {
            input_signals: entry
                .input_signals
                .iter()
                .map(|signal| HashSet::from_iter(signal.chars()))
                .collect(),
            output_signals: entry
                .output_signals
                .iter()
                .map(|signal| HashSet::from_iter(signal.chars()))
                .collect(),
        })
        .collect();

    let mut sum = 0;
    for entry_set in entry_sets {
        let map = decode_signal_mapping(&entry_set.input_signals);
        let output = decode_output(&entry_set.output_signals, map);
        sum += output;
    }

    sum
}

fn main() {
    let entries = parse_input_file("input.txt");
    println!("Answer 1: {}", count_uniqely_segmented_signals(&entries));
    println!("Answer 2: {}", sum_digits(&entries));
}
