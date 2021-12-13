use std::ops::{Index, IndexMut};

type Energy = usize;
type Indices = (usize, usize);

#[derive(Debug, Clone)]
struct Grid<T> {
    pub width: usize,
    pub height: usize,
    values: Vec<T>,
}

#[derive(Debug, Clone)]
struct Octopus {
    energy: Energy,
    flashed: bool,
}

fn normalize_indices(grid_height: usize, indices: (usize, usize)) -> usize {
    indices.0 + grid_height * indices.1
}

fn get_adjacents_indices_to(
    width: usize,
    height: usize,
    indices: (usize, usize),
) -> impl Iterator<Item = Indices> {
    match indices {
        // CORNERS
        // left upper corner
        (0, 0) => vec![(1, 0), (0, 1), (1, 1)].into_iter(),
        // right upper corner
        (x, 0) if x == width - 1 => vec![(x - 1, 0), (x - 1, 1), (x, 1)].into_iter(),
        // left lower corner
        (0, y) if y == height - 1 => vec![(0, y - 1), (1, y - 1), (1, y)].into_iter(),
        // right lower corner
        (x, y) if x == width - 1 && y == height - 1 => {
            vec![(x - 1, y - 1), (x, y - 1), (x - 1, y)].into_iter()
        }
        // left border
        (0, y) => vec![(0, y - 1), (1, y - 1), (1, y), (0, y + 1), (1, y + 1)].into_iter(),
        // right border
        (x, y) if x == width - 1 => vec![
            (x - 1, y - 1),
            (x, y - 1),
            (x - 1, y),
            (x - 1, y + 1),
            (x, y + 1),
        ]
        .into_iter(),
        // upper border
        (x, 0) => vec![(x - 1, 0), (x + 1, 0), (x - 1, 1), (x, 1), (x + 1, 1)].into_iter(),
        // lower border
        (x, y) if y == height - 1 => vec![
            (x - 1, y - 1),
            (x, y - 1),
            (x + 1, y - 1),
            (x - 1, y),
            (x + 1, y),
        ]
        .into_iter(),
        // middle
        (x, y) => vec![
            (x - 1, y - 1),
            (x, y - 1),
            (x + 1, y - 1),
            (x - 1, y),
            (x + 1, y),
            (x - 1, y + 1),
            (x, y + 1),
            (x + 1, y + 1),
        ]
        .into_iter(),
    }
}

impl<T> Grid<T> {
    pub fn new<I>(width: usize, height: usize, values: I) -> Option<Grid<T>>
    where
        I: Iterator<Item = T>,
    {
        let values: Vec<T> = values.collect();
        if values.len() != width * height {
            return None;
        }

        Some(Grid {
            width,
            height,
            values,
        })
    }

    pub fn iter(&self) -> std::slice::Iter<T> {
        self.values.iter()
    }

    pub fn get_adjacents_mut<'a>(
        &'a mut self,
        x: usize,
        y: usize,
    ) -> impl Iterator<Item = &'a mut T> {
        let height = self.height;
        let width = self.width;
        self.values
            .as_mut_slice()
            .into_iter()
            .enumerate()
            .filter(move |(i, _)| {
                match (x, y) {
                    (0, 0) =>
                    // CORNERS
                    {
                        *i == normalize_indices(height, (x + 1, y))
                            || *i == normalize_indices(height, (x, y + 1))
                            || *i == normalize_indices(height, (x + 1, y + 1))
                    }
                    (x, 0) if x == width - 1 => {
                        *i == normalize_indices(height, (x - 1, y))
                            || *i == normalize_indices(height, (x, y + 1))
                            || *i == normalize_indices(height, (x - 1, y + 1))
                    }
                    (0, y) if y == height - 1 => {
                        *i == normalize_indices(height, (x, y - 1))
                            || *i == normalize_indices(height, (x + 1, y))
                            || *i == normalize_indices(height, (x + 1, y - 1))
                    }
                    (x, y) if x == width - 1 && y == height - 1 => {
                        *i == normalize_indices(height, (x - 1, y))
                            || *i == normalize_indices(height, (x, y - 1))
                            || *i == normalize_indices(height, (x - 1, y - 1))
                    }
                    // BORDERS
                    // left border
                    (0, y) => {
                        *i == normalize_indices(height, (x, y - 1))
                            || *i == normalize_indices(height, (x, y + 1))
                            || *i == normalize_indices(height, (x + 1, y - 1))
                            || *i == normalize_indices(height, (x + 1, y))
                            || *i == normalize_indices(height, (x + 1, y + 1))
                    }
                    // right border
                    (x, y) if x == width - 1 =>
                    // ri
                    {
                        *i == normalize_indices(height, (x, y - 1))
                            || *i == normalize_indices(height, (x, y + 1))
                            || *i == normalize_indices(height, (x - 1, y - 1))
                            || *i == normalize_indices(height, (x - 1, y))
                            || *i == normalize_indices(height, (x - 1, y + 1))
                    }
                    // upper border
                    (x, 0) => {
                        *i == normalize_indices(height, (x - 1, y))
                            || *i == normalize_indices(height, (x + 1, y))
                            || *i == normalize_indices(height, (x - 1, y + 1))
                            || *i == normalize_indices(height, (x, y + 1))
                            || *i == normalize_indices(height, (x + 1, y + 1))
                    }
                    // lower border
                    (x, y) if y == height - 1 => {
                        *i == normalize_indices(height, (x - 1, y))
                            || *i == normalize_indices(height, (x + 1, y))
                            || *i == normalize_indices(height, (x - 1, y - 1))
                            || *i == normalize_indices(height, (x, y - 1))
                            || *i == normalize_indices(height, (x + 1, y - 1))
                    }
                    // middle
                    (x, y) => {
                        *i == normalize_indices(height, (x - 1, y))
                            || *i == normalize_indices(height, (x + 1, y))
                            || *i == normalize_indices(height, (x - 1, y - 1))
                            || *i == normalize_indices(height, (x, y - 1))
                            || *i == normalize_indices(height, (x + 1, y - 1))
                            || *i == normalize_indices(height, (x - 1, y + 1))
                            || *i == normalize_indices(height, (x, y + 1))
                            || *i == normalize_indices(height, (x + 1, y + 1))
                    }
                }
            })
            .map(|(_, v)| v)
    }
}

impl<T> IntoIterator for Grid<T> {
    type IntoIter = std::vec::IntoIter<T>;
    type Item = T;
    fn into_iter(self) -> <Self as IntoIterator>::IntoIter {
        self.values.into_iter()
    }
}

impl<T> Index<(usize, usize)> for Grid<T> {
    type Output = T;

    fn index(&self, indices: (usize, usize)) -> &Self::Output {
        let normalized_idx = indices.0 + self.height * indices.1;
        &self.values[normalized_idx]
    }
}

impl<T> IndexMut<(usize, usize)> for Grid<T> {
    fn index_mut(&mut self, indices: (usize, usize)) -> &mut Self::Output {
        let normalized_idx = indices.0 + self.height * indices.1;
        &mut self.values[normalized_idx]
    }
}

fn flash(indices: (usize, usize), grid: &mut Grid<Octopus>) {
    let octopus = &mut grid[indices];
    if octopus.energy > 9 && !octopus.flashed {
        octopus.flashed = true;
        octopus.energy = 0;
        for adj_indices in get_adjacents_indices_to(grid.width, grid.height, indices) {
            if !grid[adj_indices].flashed {
                grid[adj_indices].energy += 1;
            }
            flash(adj_indices, grid);
        }
    }
}

fn step(grid: &mut Grid<Octopus>) {
    for x in 0..grid.width {
        for y in 0..grid.height {
            let octopus = &mut grid[(x, y)];
            octopus.flashed = false;
            octopus.energy += 1;
        }
    }
    for x in 0..grid.width {
        for y in 0..grid.height {
            flash((x, y), grid);
        }
    }
}

fn solve(steps: usize, mut grid: Grid<Octopus>) -> (usize, usize) {
    let mut total_flashes: usize = 0;
    let mut step_when_all_flashed = 0;
    for i in 0..steps {
        step(&mut grid);
        let flashes = grid.iter().filter(|octopus| octopus.flashed).count();
        if grid.iter().all(|octopus| octopus.flashed) && step_when_all_flashed == 0 {
            step_when_all_flashed = i;
        }
        total_flashes += flashes;
    }
    (total_flashes, step_when_all_flashed)
}

fn parse_input_file(path: &str) -> Grid<Octopus> {
    let content = std::fs::read_to_string(path).unwrap();
    let lines = content.lines();
    let grid_width = lines.clone().count();
    let grid_height = lines.clone().next().map(|line| line.len()).unwrap();
    let digits: Vec<usize> = content
        .lines()
        .collect::<String>()
        .as_str()
        .chars()
        .map(|x| x.to_string().parse().unwrap())
        .collect();
    let octopuses: Vec<Octopus> = digits
        .into_iter()
        .map(|d| Octopus {
            energy: d,
            flashed: false,
        })
        .collect();
    Grid::new(grid_width, grid_height, octopuses.into_iter()).unwrap()
}

fn main() {
    let grid = parse_input_file("input.txt");
    let (flashes, _) = solve(100, grid.clone());
    println!("Answer 1: {}", flashes);
    let (_, step_when_all_flashed) = solve(9999, grid);
    println!("Answer 2: {}", step_when_all_flashed + 1);
}

mod tests {
    use crate::{Grid, Indices};
    

    #[test]
    fn making_grid_using_new() {
        let grid = Grid::new(5, 5, [1].repeat(25).into_iter());
        assert!(grid.is_some());
        let grid = Grid::new(5, 5, [1].repeat(24).into_iter());
        assert!(grid.is_none());
    }

    #[test]
    fn indexing_grid() {
        let grid = Grid::new(3, 3, [0, 1, 2, 3, 4, 5, 6, 7, 8].into_iter()).unwrap();
        assert_eq!(grid[(0, 0)], 0);
        assert_eq!(grid[(1, 0)], 1);
        assert_eq!(grid[(2, 0)], 2);
        assert_eq!(grid[(0, 1)], 3);
        assert_eq!(grid[(1, 1)], 4);
        assert_eq!(grid[(2, 1)], 5);
        assert_eq!(grid[(0, 2)], 6);
        assert_eq!(grid[(1, 2)], 7);
        assert_eq!(grid[(2, 2)], 8);
    }

    #[test]
    fn getting_adjacent_indices() {
        let width = 3;
        let height = 3;

        let left_corner: Vec<Indices> = super::get_adjacents_indices_to(width, height, (0, 0)).collect();
        assert_eq!(left_corner, vec![(1, 0), (0, 1), (1, 1)]);
        let right_corner: Vec<Indices> = super::get_adjacents_indices_to(width, height, (2, 0)).collect();
        assert_eq!(right_corner, vec![(1, 0), (1, 1), (2, 1)]);
        let left_lower_corner: Vec<Indices> =
            super::get_adjacents_indices_to(width, height, (0, 2)).collect();
        assert_eq!(left_lower_corner, vec![(0, 1), (1, 1), (1, 2)]);
        let right_lower_corner: Vec<Indices> =
            super::get_adjacents_indices_to(width, height, (2, 2)).collect();
        assert_eq!(right_lower_corner, vec![(1, 1), (2, 1), (1, 2)]);

        let left_border: Vec<Indices> = super::get_adjacents_indices_to(width, height, (0, 1)).collect();
        assert_eq!(left_border, vec![(0, 0), (1, 0), (1, 1), (0, 2), (1, 2)]);

        let right_border: Vec<Indices> = super::get_adjacents_indices_to(width, height, (2, 1)).collect();
        assert_eq!(right_border, vec![(1, 0), (2, 0), (1, 1), (1, 2), (2, 2)]);

        let upper_border: Vec<Indices> = super::get_adjacents_indices_to(width, height, (1, 0)).collect();
        assert_eq!(upper_border, vec![(0, 0), (2, 0), (0, 1), (1, 1), (2, 1)]);

        let lower_border: Vec<Indices> = super::get_adjacents_indices_to(width, height, (1, 2)).collect();
        assert_eq!(lower_border, vec![(0, 1), (1, 1), (2, 1), (0, 2), (2, 2)]);

        let middle: Vec<Indices> = super::get_adjacents_indices_to(width, height, (1, 1)).collect();
        assert_eq!(
            middle,
            vec![
                (0, 0),
                (1, 0),
                (2, 0),
                (0, 1),
                (2, 1),
                (0, 2),
                (1, 2),
                (2, 2)
            ]
        );
    }
}
