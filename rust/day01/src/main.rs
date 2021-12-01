use std::fs;
use std::str::FromStr;

type DepthMeasurment = usize;

fn parse_input_file(path: &str) -> Vec<DepthMeasurment> {
    let contents = fs::read_to_string(path).unwrap();
    let measurement = contents.lines().map(|line| usize::from_str(line).unwrap());
    measurement.collect()
}

fn count_increases(measurements: &[DepthMeasurment]) -> usize {
    let mut increases = 0;
    for i in 1..measurements.len() {
        if measurements[i] > measurements[i - 1] {
            increases += 1;
        }
    }
    increases
}

fn count_triple_increases(measurements: &[DepthMeasurment]) -> usize {
    let mut increases = 0;
    for i in 3..measurements.len() {
        let window1 = measurements[i - 1] + measurements[i - 2] + measurements[i - 3];
        let window2 = measurements[i] + measurements[i - 1] + measurements[i - 2];
        if window2 > window1 {
            increases += 1;
        }
    }
    increases
}

fn main() {
    let measurements = parse_input_file("input.txt");
    let depth_increases = count_increases(&measurements);
    println!("Answer 1: {}", depth_increases);
    let triple_depth_increases = count_triple_increases(&measurements);
    println!("Answer 2: {}", triple_depth_increases);
}
