use std::collections::HashSet;
use advent_of_code::Solution;

pub fn part_one(input: &str) -> Solution {
    let mut location: (i32, i32) = (0, 0);

    let mut visited: HashSet<(i32, i32)> = HashSet::new();
    visited.insert(location);

    for c in input.chars() {
        location = next_location(c, &location);

        visited.insert(location);
    }

    Solution::USize(visited.len())
}

pub fn part_two(input: &str) -> Solution {
    let mut santa_loc: (i32, i32) = (0, 0);
    let mut robot_loc: (i32, i32) = (0, 0);
    let mut santa_turn = true;

    let mut visited: HashSet<(i32, i32)> = HashSet::new();
    visited.insert(santa_loc);

    for c in input.chars() {
        if santa_turn {
            santa_loc = next_location(c, &santa_loc);
            visited.insert(santa_loc);
        } else {
            robot_loc = next_location(c, &robot_loc);
            visited.insert(robot_loc);
        }

        santa_turn = !santa_turn;
    }

    Solution::USize(visited.len())
}

fn next_location(direction: char, current_loc: &(i32, i32)) -> (i32, i32) {
    match direction {
        '^' => (current_loc.0, current_loc.1 + 1),
        'v' => (current_loc.0, current_loc.1 - 1),
        '>' => (current_loc.0 + 1, current_loc.1),
        '<' => (current_loc.0 - 1, current_loc.1),
        _ => unreachable!()
    }
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 3);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test() {
        let input = advent_of_code::read_file("inputs", 3);
        assert_eq!(part_one(&input), Solution::USize(2592));
        assert_eq!(part_two(&input), Solution::USize(2360));
    }
}
