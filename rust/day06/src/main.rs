use std::collections::HashMap;
use std::fs;

#[derive(Debug, Clone)]
struct Lanternfish {
    pub left_days: usize,
}

impl Lanternfish {
    pub fn new(left_days: usize) -> Self {
        Lanternfish { left_days }
    }

    pub fn update(&self) -> (Self, Option<Lanternfish>) {
        match self.left_days {
            0 => (Lanternfish::new(6), Some(Lanternfish::new(8))),
            x => (Lanternfish::new(x - 1), None),
        }
    }
}

#[derive(Debug, Clone)]
struct SimulationState {
    pub fishes: Vec<Lanternfish>,
}

fn update_simulation_state(simulation: SimulationState) -> SimulationState {
    let new_fishes: Vec<Lanternfish> = simulation
        .fishes
        .iter()
        .map(|fish| fish.update())
        .map(
            |(updated_fish, possibly_spawned_fish)| match possibly_spawned_fish {
                Some(spawned_fish) => vec![updated_fish, spawned_fish],
                None => vec![updated_fish],
            },
        )
        .flatten()
        .collect();
    SimulationState { fishes: new_fishes }
}

fn run_n_days_simulation(simulation: SimulationState, days: usize) -> SimulationState {
    let mut simulation = simulation;
    for _ in 0..days {
        simulation = update_simulation_state(simulation);
    }
    simulation
}

type FastSimulationState = HashMap<usize, usize>;

fn make_fast_simulation(slow_simulation: &SimulationState) -> FastSimulationState {
    let mut fast_simulation = FastSimulationState::from_iter([
        (0usize, 0),
        (0usize, 0),
        (1usize, 0),
        (2usize, 0),
        (3usize, 0),
        (4usize, 0),
        (5usize, 0),
        (6usize, 0),
        (7usize, 0),
        (8usize, 0),
    ]);
    for fish in slow_simulation.fishes.iter() {
        fast_simulation.insert(
            fish.left_days,
            fast_simulation.get(&fish.left_days).unwrap() + 1,
        );
    }
    fast_simulation
}

fn run_fast_n_days_simulation(simulation: FastSimulationState, days: usize) -> FastSimulationState {
    let mut simulation = simulation;
    for _ in 0..days {
        let new_zeros = *simulation.get(&1).unwrap();
        let new_ones = *simulation.get(&2).unwrap();
        let new_twos = *simulation.get(&3).unwrap();
        let new_threes = *simulation.get(&4).unwrap();
        let new_fours = *simulation.get(&5).unwrap();
        let new_fives = *simulation.get(&6).unwrap();
        let new_sixes = *simulation.get(&7).unwrap() + *simulation.get(&0).unwrap();
        let new_sevens = *simulation.get(&8).unwrap();
        let new_eights = *simulation.get(&0).unwrap();
        simulation = FastSimulationState::from_iter([
            (0usize, new_zeros),
            (0usize, new_zeros),
            (1usize, new_ones),
            (2usize, new_twos),
            (3usize, new_threes),
            (4usize, new_fours),
            (5usize, new_fives),
            (6usize, new_sixes),
            (7usize, new_sevens),
            (8usize, new_eights),
        ]);
    }
    simulation
}

fn parse_input(path: &str) -> Option<Vec<Lanternfish>> {
    let content = fs::read_to_string(path).ok()?;
    let parse_digits = content.lines().next()?.split(',').map(|d| d.parse());
    if parse_digits.clone().any(|d| d.is_err()) {
        return None;
    }

    Some(
        parse_digits
            .map(|d| d.unwrap())
            .map(|d| Lanternfish { left_days: d })
            .collect(),
    )
}


fn main() {
    let fishes = parse_input("input.txt").expect("Could not parse the provided input!");
    let simulation = SimulationState {
        fishes: fishes.clone(),
    };

    println!(
        "Answer 1: {}",
        run_n_days_simulation(simulation.clone(), 80).fishes.len()
    );

    let fast_simulation = make_fast_simulation(&simulation);
    let fast_result: usize = run_fast_n_days_simulation(fast_simulation, 256).iter()
        .fold(0, |acc, (_, value)| { acc + value });

    println!("Answer 2: {}", fast_result);
}
