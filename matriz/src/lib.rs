use std::fmt::Display;
use std::fmt;

pub struct Matriz {
    rules: fn (usize, usize) -> usize,
    size_x: usize,
    size_y: usize,
    matriz: Vec<Vec<usize>>,
    bigger_num_len: usize
}

impl Matriz {
    fn get_num_of_digits(num: usize) -> usize {
        let mut result = num;
        let mut num_of_digits = 0;

        if result == 0 {
            num_of_digits += 1
        }

        loop {
            if result != 0 {
                num_of_digits += 1;
                result = result / 10
            }
            else { break }
        }

        num_of_digits
    }

    fn apply_rules(size: (usize, usize), rule: fn (usize, usize) -> usize) -> (Vec<Vec<usize>>, usize) {
        let mut m: Vec<Vec<usize>> = vec![];
        let mut bnl = 0;

        for y in 1..=size.1 {
            m.push(vec![]);
            for x in 1..=size.0 {
                let result = rule(x, y);
                let num_of_digits = Matriz::get_num_of_digits(result);

                if num_of_digits > bnl {
                    bnl = num_of_digits
                }
                m[y-1].push(result)
            }
        }
        (m, bnl)
    }

    pub fn create(size: (usize, usize), rule: fn (usize, usize) -> usize) -> Matriz {
        let mounted_matriz = Matriz::apply_rules(size, rule);
        Matriz { size_x: size.0, size_y: size.1, rules: rule, matriz: mounted_matriz.0, bigger_num_len: mounted_matriz.1}
    }
}

impl Display for Matriz {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut text = String::new();

        for y in 0..self.size_y {
            text.push('\n');
            for x in 0..self.size_x {
                let value = self.matriz[y][x];

                for _ in 0..(self.bigger_num_len - Matriz::get_num_of_digits(value)) {
                    text.push(' ')
                }

                text += &(value.to_string() + " ");
            }
        }

        write!(f, "{}", text.trim_matches('\n'))
    }
}