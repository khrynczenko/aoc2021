use std::collections::{HashMap, HashSet};
use std::fs;

type Cost = isize;
type Position = isize;
type Positions = Vec<isize>;

fn parse_input_file(path: &str) -> Positions {
    let content = fs::read_to_string(path).unwrap();
    content
        .as_str()
        .lines()
        .next()
        .unwrap()
        .split(',')
        .map(|x| x.parse().unwrap())
        .collect()
}

fn initialize_hash_map(positions: &[Position]) -> HashMap<Position, Cost> {
    let unique_positions = HashSet::<isize>::from_iter(positions.iter().copied());
    let costs = HashMap::from_iter(unique_positions.iter().map(|position| (*position, 0)));
    costs
}

type CostFunction = fn(Position, Position) -> Cost;

fn cost_function1(pos1: Position, pos2: Position) -> Cost {
    (pos1 - pos2).abs()
}

fn cost_function2(pos1: Position, pos2: Position) -> Cost {
    (1..=(pos1 - pos2).abs()).sum()
}

fn find_costs(positions: &[Position], f: CostFunction) -> HashMap<Position, Cost> {
    let mut costs = initialize_hash_map(positions);
    for (position, _) in costs.clone() {
        let total: isize = positions.iter().map(|pos| f(position, *pos)).sum();
        costs.insert(position, total);
    }
    costs
}

fn calculate_optimal_position_total_cost(
    positions: &[Position],
    cost_function: CostFunction,
) -> isize {
    let costs = find_costs(positions, cost_function);
    *costs.iter().min_by_key(|(_, cost)| *cost).unwrap().1
}

fn main() {
    let positions = parse_input_file("input.txt");
    println!(
        "Answer 1: {}",
        calculate_optimal_position_total_cost(&positions, cost_function1)
    );
    println!(
        "Answer 1: {}",
        calculate_optimal_position_total_cost(&positions, cost_function2)
    );
}
