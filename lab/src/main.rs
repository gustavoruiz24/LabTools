#![allow(unused_imports)]
#![allow(dead_code)]

use array_maker::*;
use compare::*;
use dimension::SimpDimenBase::*;
use dimension::*;
use std::f64::consts::PI;
use value::Value::{self, Number, Unknown};

fn main() {
    let vals = seq_by_key(-250, 250, 1, |x| {
        // SimpDimen::init(x as f64, "m", Length).unwrap().to_generic()
        x as f64
    });

    let cp = Comparison::new(
        vals,
        // vec![|x| x.pow(2) * 4.0 * PI, |x| x.pow(3) * 4.0 * PI / 3.0],
        vec![|x| 1.0 / x],
        |x, y| y.clone() / x.clone(),
    );

    cp.plot();
}