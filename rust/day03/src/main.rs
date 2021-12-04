use std::fs;
use std::path::Path;

const BIT_COUNT: u8 = 12;

fn parse_input_file<T: AsRef<Path>>(path: T) -> Vec<u16> {
    let content = fs::read_to_string(path.as_ref()).unwrap();
    content
        .lines()
        .map(|line| u16::from_str_radix(line, 2).unwrap())
        .collect()
}

fn calculate_power_consumption(binary_numbers: &[u16]) -> usize {
    let numbers_count = binary_numbers.len();
    let mut gamma_rate: u16 = 0;
    let mut epsilon_rate: u16 = 0;
    for i in 0..BIT_COUNT {
        let ones: usize = binary_numbers
            .iter()
            .map(|x| x & (1 << i))
            .filter(|&x| x > 0)
            .count();
        let zeros = numbers_count - ones;
        if ones > zeros {
            gamma_rate = gamma_rate | (1 << i);
        } else {
            epsilon_rate = epsilon_rate | (1 << i);
        }
    }

    gamma_rate as usize * epsilon_rate as usize
}

fn calculate_life_support_rating(binary_numbers: &[u16]) -> usize {
    let mut binary_numbersv: Vec<u16> = binary_numbers.iter().map(|&x| x).collect();

    for i in 1..(BIT_COUNT + 1) {
        if binary_numbersv.len() == 1 {
            break;
        }
        let ones: usize = binary_numbersv
            .iter()
            .map(|x| x & (1 << BIT_COUNT - i))
            .filter(|&x| x > 0)
            .count();
        let zeros = binary_numbersv.len() - ones;
        if ones >= zeros {
            binary_numbersv = binary_numbersv
            .iter()
            .filter(|&&x| (x & (1 << BIT_COUNT - i)) > 0)
            .map(|x| *x)
            .collect()
        } else {
            binary_numbersv = binary_numbersv
            .iter()
            .filter(|&&x| (x & (1 << BIT_COUNT - i)) == 0)
            .map(|x| *x)
            .collect()
        }
    }

    let oxygen_generator_rating = *binary_numbersv.get(0).unwrap();

    let mut binary_numbersv: Vec<u16> = binary_numbers.iter().map(|&x| x).collect();
    for i in 1..(BIT_COUNT + 1) {
        if binary_numbersv.len() == 1 {
            break;
        }
        let ones: usize = binary_numbersv
            .iter()
            .map(|x| x & (1 << (BIT_COUNT - i)))
            .filter(|&x| x > 0)
            .count();
        let zeros = binary_numbersv.len() - ones;
        if ones < zeros {
            binary_numbersv = binary_numbersv
            .iter()
            .filter(|&&x| (x & (1 << (BIT_COUNT - i))) > 0)
            .map(|x| *x)
            .collect()
        } else {
            binary_numbersv = binary_numbersv
            .iter()
            .filter(|&&x| (x & (1 << (BIT_COUNT - i))) == 0)
            .map(|x| *x)
            .collect()
        }
    }
    let co2_scrubber_rating = *binary_numbersv.get(0).unwrap();

    oxygen_generator_rating as usize * co2_scrubber_rating as usize
}

fn main() {
    let binary_numbers = parse_input_file("input.txt");
    println!("Answer 1: {}", calculate_power_consumption(&binary_numbers));
    println!("Answer 2: {}", calculate_life_support_rating(&binary_numbers));
}
