use std::f64::consts::PI;

use expression::expr_structs::ExprTree;

pub type ConvFromSi = fn(ExprTree) -> ExprTree;
pub type ConvToSi = fn(ExprTree) -> ExprTree;
pub type ProportionFromSi = fn(ExprTree) -> ExprTree;
pub type ProportionToSi = fn(ExprTree) -> ExprTree;

pub const PREFIXES: [(&str, f64); 21] = [
    ("Y", 1e24),
    ("Z", 1e21),
    ("E", 1e18),
    ("P", 1e15),
    ("T", 1e12),
    ("G", 1e9),
    ("M", 1e6),
    ("k", 1e3),
    ("h", 1e2),
    ("da", 1e1),
    ("", 1.0),
    ("d", 1e-1),
    ("c", 1e-2),
    ("m", 1e-3),
    ("u", 1e-6),
    ("n", 1e-9),
    ("p", 1e-12),
    ("f", 1e-15),
    ("a", 1e-18),
    ("z", 1e-21),
    ("y", 1e-24),
];

pub const ND_PREFIXES: [(&str, f64); 1] = [("", 1.0)];

pub const NO_CHANGE: (ConvFromSi, ConvToSi) = (|x| x, |x| x);

const LENGTH_CONV_RATES: [(ConvFromSi, ConvToSi); 5] = [
    (|x| x / 3.0856775813e16, |x| x * 3.0856775813e16),
    (|x| x / 9.461e15, |x| x * 9.461e15),
    (|x| x / 1.49597870690e11, |x| x * 1.49597870690e11),
    NO_CHANGE,
    (|x| x * 1e10, |x| x / 1e10),
];

const LENGTH_PROPORTIONS: [(ProportionFromSi, ProportionToSi); 5] = LENGTH_CONV_RATES;

pub const LENGTH_INFO: ([&str; 5], [(ConvFromSi, ConvToSi); 5], [(ProportionFromSi, ProportionToSi); 5]) = (
    ["pc", "ly", "au", "m", "ang"], LENGTH_CONV_RATES, LENGTH_PROPORTIONS
);

const TIME_CONV_RATES: [(ConvFromSi, ConvToSi); 7] = [
    (|x| x / 3.15576e7, |x| x * 3.15576e7),
    (|x| x / 2.592e6, |x| x * 2.592e6),
    (|x| x / 6.048e5, |x| x * 6.048e5),
    (|x| x / 8.64e4, |x| x * 8.64e4),
    (|x| x / 3.6e3, |x| x * 3.6e3),
    (|x| x / 60.0, |x| x * 60.0),
    NO_CHANGE,
];

const TIME_PROPORTIONS: [(ProportionFromSi, ProportionToSi); 7] = TIME_CONV_RATES;

pub const TIME_INFO: ([&str; 7], [(ConvFromSi, ConvToSi); 7], [(ProportionFromSi, ProportionToSi); 7]) = (
    ["y", "mo", "wk", "day", "h", "min", "s"], TIME_CONV_RATES, TIME_PROPORTIONS
);

const MASS_CONV_RATES: [(ConvFromSi, ConvToSi); 3] = [
    (|x| x / 1e3, |x| x * 1e3),
    NO_CHANGE,
    (|x| x * 1e3, |x| x / 1e3),
];

const MASS_PROPORTIONS: [(ProportionFromSi, ProportionToSi); 3] = MASS_CONV_RATES;

pub const MASS_INFO: ([&str; 3], [(ConvFromSi, ConvToSi); 3], [(ProportionFromSi, ProportionToSi); 3]) = (
    ["t", "kg", "g"], MASS_CONV_RATES, MASS_PROPORTIONS
);

pub const EL_CURRENT_INFO: ([&str; 1], [(ConvFromSi, ConvToSi); 1], [(ProportionFromSi, ProportionToSi); 1]) = (
    ["A"], [NO_CHANGE], [NO_CHANGE]
);

const TEMPERATURE_CONV_RATES: [(ConvFromSi, ConvToSi); 3] = [
    (|x| x - 273.15, |x| x + 273.15),
    (
        |x| (x - 273.15) * 9.0 / 5.0 + 32.0,
        |x| (x - 32.0) * 5.0 / 9.0 + 273.15,
    ),
    NO_CHANGE,
];

const TEMPERATURE_PROPORTIONS: [(ProportionFromSi, ProportionToSi); 3] = [
    NO_CHANGE,
    (|x| x * 9.0 / 5.0, |x| x * 5.0 / 9.0),
    NO_CHANGE,
];

pub const TEMPERATURE_INFO: ([&str; 3], [(ConvFromSi, ConvToSi); 3], [(ProportionFromSi, ProportionToSi); 3]) = (
    ["C", "F", "K"], TEMPERATURE_CONV_RATES, TEMPERATURE_PROPORTIONS
);

pub const AM_OF_SUBSTANCE_INFO: ([&str; 1], [(ConvFromSi, ConvToSi); 1], [(ProportionFromSi, ProportionToSi); 1]) = (
    ["mole"], [NO_CHANGE], [NO_CHANGE]
);

pub const LUMINOUS_IN_INFO: ([&str; 1], [(ConvFromSi, ConvToSi); 1], [(ProportionFromSi, ProportionToSi); 1]) = (
    ["cd"], [NO_CHANGE], [NO_CHANGE]
);

const ANGLE_CONV_RATES: [(ConvFromSi, ConvToSi); 5] = [
    (|x| x * PI / 180.0, |x| x * 180.0 / PI),
    NO_CHANGE,
    (|x| x * 15.0, |x| x / 15.0),
    (|x| x * 60.0, |x| x / 60.0),
    (|x| x * 3.6e3, |x| x / 3.6e3),
];

const ANGLE_PROPORTIONS: [(ProportionFromSi, ProportionToSi); 5] = ANGLE_CONV_RATES;

pub const ANGLE_INFO: ([&str; 5], [(ConvFromSi, ConvToSi); 5], [(ProportionFromSi, ProportionToSi); 5]) = (
    ["rad", "°", "h", "'", r#"""#], ANGLE_CONV_RATES, ANGLE_PROPORTIONS
);

pub const ND_INFO: ([&str; 1], [(ConvFromSi, ConvToSi); 1], [(ProportionFromSi, ProportionToSi); 1]) = (
    [""], [NO_CHANGE], [NO_CHANGE]
);
