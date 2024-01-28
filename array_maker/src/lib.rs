use rand::prelude::*;
use std::cmp::Ordering;

pub fn rand_by_key<T>(
    length: usize,
    min: isize,
    max: isize,
    key: fn(isize) -> T,
    sort_key: Option<fn(&T, &T) -> Ordering>,
) -> Vec<T> {
    let mut rng = thread_rng();
    let mut values: Vec<T> = vec![];

    for _ in 0..length {
        values.push(key(rng.gen_range(min..=max)))
    }

    if let Some(f) = sort_key {
        values.sort_unstable_by(f)
    }

    values
}

pub fn seq_by_key<T>(start: isize, end: isize, skip: usize, key: fn(isize) -> T) -> Vec<T> {
    if start < end {
        Vec::from_iter((start..=end).step_by(skip).map(|x| key(x)))
    } else {
        Vec::from_iter((end..=start).rev().step_by(skip).map(|x| key(x)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test1() {
        let current = rand_by_key(5, 0, 10, |x| x as f64, None);
        println!("{:?}", current);
        let current = rand_by_key(5, 0, 10, |x| x as f64, Some(|a, b| a.total_cmp(b)));
        println!("{:?}", current);
        let current = rand_by_key(5, 0, 10, |x| x as f64, Some(|a, b| b.total_cmp(a)));
        println!("{:?}", current);
    }

    #[test]
    fn test2() {
        let current = seq_by_key(0, 10, 2, |x| x as f64);
        println!("{:?}", current);
        let current = seq_by_key(10, 0, 2, |x| x as f64);
        println!("{:?}", current);
    }
}
