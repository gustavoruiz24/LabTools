#![allow(dead_code)]

use crate::CompResult::*;
use colored::Colorize;
use dimension::*;
use plotters::prelude::*;
use rand::prelude::*;
use std::fmt::{Display, Formatter};
use std::ops::Add;
use dimension::traits::{DimenBasics, DimenSetAndGet};

pub trait CompareBasics {
    fn as_f64(&self) -> f64;

    fn zero() -> Self;

    fn div_f64(self, other: f64) -> Self;
}

impl CompareBasics for f64 {
    fn as_f64(&self) -> f64 {
        *self
    }

    fn zero() -> Self {
        0.0
    }

    fn div_f64(self, other: f64) -> Self {
        self / other
    }
}

impl CompareBasics for i32 {
    fn as_f64(&self) -> f64 {
        *self as f64
    }

    fn zero() -> Self {
        0
    }

    fn div_f64(self, other: f64) -> Self {
        (self as f64 / other) as i32
    }
}

impl CompareBasics for u32 {
    fn as_f64(&self) -> f64 {
        *self as f64
    }

    fn zero() -> Self {
        0
    }

    fn div_f64(self, other: f64) -> Self {
        (self as f64 / other) as u32
    }
}

impl CompareBasics for GeneDimen {
    fn as_f64(&self) -> f64 {
        self.get_value()
    }

    fn zero() -> Self {
        SimpDimen::init_nd(0.0).to_generic()
    }

    fn div_f64(self, other: f64) -> Self {
        self / other
    }
}

#[derive(Copy, Clone)]
enum CompResult<T> {
    Correct(T),
    Wrong(T),
}

impl<T> CompResult<T> {
    fn unwrap(&self) -> &T {
        match self {
            Correct(num) => num,
            Wrong(num) => num,
        }
    }
}

impl<T: Display> Display for CompResult<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.unwrap())
    }
}

type Results<T> = Vec<Vec<CompResult<T>>>;
type ErrorRates<T> = Vec<Vec<Option<T>>>;
pub struct Comparison<T> {
    values: Vec<T>,
    min_max_val: (f64, f64),
    results: Results<T>,
    min_max_res: (f64, f64),
    error_rates: ErrorRates<T>,
}

impl<T> Comparison<T>
    where
        T: Display + Add<Output = T> + PartialEq + Clone + CompareBasics,
{
    fn take_max<X: Display>(x: &[X]) -> usize {
        x.iter().map(|x| x.to_string().len()).max().unwrap()
    }

    fn take_max_sizes(
        results: &[CompResult<T>],
        error_rates: &[Option<T>],
    ) -> (usize, Option<usize>) {
        let r_max_size = Self::take_max(results);

        if !error_rates.iter().any(|x| x.is_some()) {
            return (r_max_size, None);
        }

        let error_rates = error_rates.iter().map(|x| x.clone().unwrap_or(T::zero()));

        let er_max_size = Self::take_max(&error_rates.collect::<Vec<T>>());

        (r_max_size, Some(er_max_size))
    }

    fn complete(x: &mut String, expected_len: usize) {
        for _ in 0..(expected_len - x.len()) {
            x.push(' ')
        }
    }

    fn print_complete(x: &mut String, expected_len: usize) {
        Self::complete(x, expected_len);
        print!("{} | ", x);
    }

    fn head_line(&self, wrong_in: bool) -> (usize, Vec<usize>, Vec<usize>) {
        let v_max = Self::take_max(&self.values);
        let mut r_maximums = vec![];
        let mut er_maximums = vec![];
        let mut total_size = 0;

        let mut headline = String::from("Values");
        Self::print_complete(&mut headline, 7 + v_max);
        total_size += 10 + v_max;

        for model in 0..self.results.len() {
            let (r_max, er_max) =
                Self::take_max_sizes(&self.results[model], &self.error_rates[model]);
            r_maximums.push(r_max);
            er_maximums.push(er_max.unwrap_or(0));

            let mut hl_model = format!("Model {}", model);
            let size_max = 8 + r_max;
            if er_max.is_none() || !wrong_in {
                Self::print_complete(&mut hl_model, size_max);
                total_size += size_max + 3;
            } else {
                Self::print_complete(&mut hl_model, size_max + 13 + er_max.unwrap());
                total_size += size_max + 16 + er_max.unwrap();
            }
        }

        let mut sep = String::new();
        for _ in 0..(total_size - 1) {
            sep.push('-')
        }
        println!("\n{}", sep);

        (v_max, r_maximums, er_maximums)
    }

    pub fn new<X>(
        values: Vec<X>,
        fns: Vec<fn(X) -> X>,
        err_rate_fn: fn(&X, &X) -> X,
    ) -> Comparison<X>
        where
            X: Display + Add<Output = T> + PartialEq + Clone + CompareBasics,
    {
        let min_max = |old: (f64, f64), new: f64| -> (f64, f64) {
            if new < old.0 {
                (new, old.1)
            } else if new > old.1 {
                (old.0, new)
            } else {
                (old.0, old.1)
            }
        };

        let mut results: Results<X> = vec![];
        let mut error_rates: ErrorRates<X> = vec![];
        let mut min_max_val: (f64, f64) = (values[0].as_f64(), values[0].as_f64());
        let mut min_max_res: (f64, f64) = (
            fns[0](values[0].clone()).as_f64(),
            fns[0](values[0].clone()).as_f64(),
        );

        for f in fns {
            let mut results_piece = vec![];
            let mut error_rates_piece = vec![];

            for (i, val) in values.iter().enumerate() {
                let result = f(val.clone());

                min_max_val = min_max(min_max_val, val.as_f64());
                min_max_res = min_max(min_max_res, result.as_f64());

                let error_rate;
                let res;

                let correct = if !results.is_empty() {
                    results[0][i].unwrap()
                } else {
                    &result
                };

                if &result == correct {
                    error_rate = None;
                    res = Correct(result);
                } else {
                    error_rate = Some(err_rate_fn(correct, &result));
                    res = Wrong(result);
                }
                results_piece.push(res);
                error_rates_piece.push(error_rate)
            }

            results.push(results_piece);
            error_rates.push(error_rates_piece)
        }

        Comparison {
            values,
            min_max_val,
            results,
            min_max_res,
            error_rates,
        }
    }

    pub fn compare(&self) {
        let (v_max, r_maximums, er_maximums) = self.head_line(true);

        for i in 0..self.results[0].len() {
            let mut message = format!("Value: {}", self.values[i]);
            Self::print_complete(&mut message, 7 + v_max);

            for model in 0..self.results.len() {
                let mut r_er = String::new();

                let mut result = format!("Result: {}", self.results[model][i]);
                Self::complete(&mut result, 8 + r_maximums[model]);
                r_er += &result;

                if let Some(er) = &self.error_rates[model][i] {
                    let mut err_rate = format!(" Error Rate: {}", er);
                    Self::complete(&mut err_rate, 13 + er_maximums[model]);
                    r_er += &err_rate;
                } else if er_maximums[model] != 0 {
                    let r_er_len = r_er.len();
                    Self::complete(&mut r_er, r_er_len + 13 + er_maximums[model]);
                }

                match self.results[model][i] {
                    Correct(_) => print!("{} | ", &r_er.green()),
                    Wrong(_) => print!("{} | ", &r_er.red()),
                }
            }
            println!()
        }
    }

    pub fn show(&self) {
        let (v_max, r_maximums, _) = self.head_line(false);

        for i in 0..self.results[0].len() {
            let mut message = format!("Value: {}", self.values[i]);
            Self::print_complete(&mut message, 7 + v_max);

            for (model, _) in r_maximums.iter().enumerate().take(self.results.len()) {
                let mut result = format!("Result: {}", self.results[model][i]);
                Self::print_complete(&mut result, 8 + r_maximums[model]);
            }
            println!()
        }
    }

    pub fn statistics(&self, wrong_in: bool, advanced: bool) {
        let push_in_recurrent = |vals: &mut Vec<(T, usize)>, new: T, quantity: usize| {
            if let Some(pos) = vals.iter().position(|x| x.0 == new) {
                vals[pos].1 += quantity
            } else {
                vals.push((new, quantity))
            };
        };

        let print_recurrent = |name: &str, x: &mut Vec<(T, usize)>| {
            x.retain(|x| x.1 > 1);
            if !x.is_empty() {
                print!("{: <12}{}: ", ' ', name);
                x.sort_unstable_by(|a, b| b.1.cmp(&a.1));
                for (val, times) in x {
                    print!("{}: {}x; ", val, times)
                }
                println!()
            }
        };

        println!("Statistics:");
        for model in 0..self.results.len() {
            println!("{: <4}Model {}:", ' ', model);
            let mut recurrent_c_res: Vec<(T, usize)> = vec![];
            let mut recurrent_w_res: Vec<(T, usize)> = vec![];
            let mut recurrent_err_rates: Vec<(T, usize)> = vec![];

            let (mut cr_sum, mut cr_count) = (T::zero(), 0.0);
            let (mut wr_sum, mut wr_count) = (T::zero(), 0.0);
            let (mut er_sum, mut er_count) = (T::zero(), 0.0);

            for piece in 0..self.results[model].len() {
                match &self.results[model][piece] {
                    Correct(num) => {
                        (cr_sum, cr_count) = (cr_sum + num.clone(), cr_count + 1.0);
                        push_in_recurrent(&mut recurrent_c_res, num.clone(), 1)
                    }
                    Wrong(num) => {
                        let er = self.error_rates[model][piece].clone().unwrap();

                        (wr_sum, wr_count) = (wr_sum + num.clone(), wr_count + 1.0);
                        push_in_recurrent(&mut recurrent_w_res, num.clone(), 1);

                        (er_sum, er_count) = (er_sum + er.clone(), er_count + 1.0);
                        push_in_recurrent(&mut recurrent_err_rates, er, 1);
                    }
                }
            }

            if wrong_in {
                if advanced {
                    println!(
                        "{: <8}Success rate: {}%",
                        ' ',
                        cr_count / (cr_count + wr_count) * 100.0
                    );
                    if !recurrent_c_res.is_empty() || !recurrent_w_res.is_empty() {
                        println!("{: <8}Recurrent:", ' ');
                    }
                    print_recurrent("Correct results", &mut recurrent_c_res);
                    print_recurrent("Wrong results", &mut recurrent_w_res);
                    print_recurrent("Error rates", &mut recurrent_err_rates);
                }
                println!("{: <8}Means:", ' ');
                if cr_count != 0.0 {
                    println!("{: <12}Correct results: {}", ' ', cr_sum.div_f64(cr_count));
                }
                if wr_count != 0.0 {
                    println!("{: <12}Wrong results: {}", ' ', wr_sum.div_f64(wr_count));
                    println!("{: <12}Error rates: {}", ' ', er_sum.div_f64(er_count))
                }
            } else {
                if advanced {
                    println!("{: <8}Recurrent:", ' ');
                    let mut recurrent_res = recurrent_c_res;
                    for (val, quantity) in recurrent_w_res {
                        push_in_recurrent(&mut recurrent_res, val, quantity)
                    }
                    print_recurrent("Results", &mut recurrent_res)
                }
                println!("{: <8}Means:", ' ');
                println!(
                    "{: <12}Results: {}",
                    ' ',
                    (cr_sum + wr_sum).div_f64(cr_count + wr_count)
                );
            }
        }
    }

    pub fn plot(&self) {
        let mut rng = thread_rng();

        let root_drawing_area =
            BitMapBackend::new("images/Graph.png", (600, 400)).into_drawing_area();

        root_drawing_area.fill(&WHITE).unwrap();

        let mut chart = ChartBuilder::on(&root_drawing_area)
            .set_label_area_size(LabelAreaPosition::Left, 40)
            .set_label_area_size(LabelAreaPosition::Bottom, 40)
            .build_cartesian_2d(
                self.min_max_val.0..self.min_max_val.1,
                self.min_max_res.0..self.min_max_res.1,
            )
            .unwrap();
        chart.configure_mesh().draw().unwrap();

        for results_piece in &self.results {
            let r = rng.gen_range(0..255);
            let g = rng.gen_range(0..255);
            let b_limit = 710 - (r as usize + g as usize);
            let b_limit = if b_limit > 255 { 255 } else { b_limit as u8 };
            let b = rng.gen_range(0..b_limit);

            chart
                .draw_series(LineSeries::new(
                    self.values
                        .iter()
                        .zip(results_piece)
                        .map(|x| (x.0.as_f64(), x.1.unwrap().as_f64())),
                    &RGBColor(r, g, b),
                ))
                .unwrap();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let cp = Comparison::new(
            vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
            vec![|x| x + 2, |x| x * 2, |x| x - 2],
            |x, y| x / y,
        );
        cp.compare()
    }
}
