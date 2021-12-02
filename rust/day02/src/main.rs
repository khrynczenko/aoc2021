use std::fs;
use std::path::Path;
use std::str::FromStr;

#[derive(Debug)]
enum Move {
    Forward(i64),
    Down(i64),
    Up(i64),
}

type MoveConstructor = fn(i64) -> Move;

fn make_direction_from_string(s: &str) -> Option<MoveConstructor> 
{
    match s {
        "forward" => Some(Move::Forward),
        "down" => Some(Move::Down),
        "up" => Some(Move::Up),
        _ => None,
    }
}

fn parse_input_file<T: AsRef<Path>>(path: T) -> Vec<Move> {
    let content = fs::read_to_string(path).unwrap();
    content
        .lines()
        .map(|line| line.split(' '))
        .map(|mut substrings| (substrings.next().unwrap(), substrings.next().unwrap()))
        .map(|(move_str, count)| {
            make_direction_from_string(move_str).unwrap()(i64::from_str(count).unwrap())
        })
        .collect()
}

fn calculate_horizontal_position(moves: &[Move]) -> i64 {
    moves
        .iter()
        .map(|m| match m {
            Move::Forward(x) => *x,
            _ => 0,
        })
        .sum()
}

fn calculate_depth_position(moves: &[Move]) -> i64 {
    moves
        .iter()
        .map(|m| match m {
            Move::Up(x) => -x,
            Move::Down(x) => *x,
            _ => 0,
        })
        .sum()
}

fn calculate_with_aim(moves: &[Move]) -> i64 {
    let mut horizontal: i64 = 0;
    let mut depth: i64 = 0;
    let mut aim: i64 = 0;
    
    for m in moves {
        match m {
            Move::Forward(x) => { horizontal += x; depth += aim * x },
            Move::Down(x) => aim += x,
            Move::Up(x) => aim -= x,
        }
    }
    horizontal * depth
}

fn main() {
    let moves = parse_input_file("input.txt");
    println!(
        "Answer 1: {}",
        calculate_horizontal_position(&moves) * calculate_depth_position(&moves)
    );
    println!(
        "Answer 2: {}",
        calculate_with_aim(&moves),
    );
}
