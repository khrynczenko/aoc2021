use std::fs;
use std::str::FromStr;

type DepthMeasurment = usize;

fn parse_input_file(path: &str) -> Vec<DepthMeasurment> {
    let contents = fs::read_to_string(path).unwrap();
    let measurement = contents.lines().map(|line| usize::from_str(line).unwrap());
    measurement.collect()
}

fn count_increases(measurements: &[DepthMeasurment]) -> usize {
    measurements.windows(2).filter(|pair| pair[1] > pair[0]).count()
}

fn count_triple_increases(measurements: &[DepthMeasurment]) -> usize {
    measurements.windows(4).filter(|values| values[3] > values[0]).count()
}

fn main() {
    let measurements = parse_input_file("input.txt");
    let depth_increases = count_increases(&measurements);
    println!("Answer 1: {}", depth_increases);
    let triple_depth_increases = count_triple_increases(&measurements);
    println!("Answer 2: {}", triple_depth_increases);
}
