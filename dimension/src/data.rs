use std::f64::consts::PI;

pub const PREFIXES: [(&str, f64); 14] = [
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
];
pub const ND_PREFIXES: [(&str, f64); 1] = [("", 1.0)];
pub const LENGTH_INFO: ([&str; 5], [(ConvFromSi, ConvToSi); 5]) = (
    ["pc", "ly", "au", "m", "ang"],
    [
        (|x| x / 3.0856775813e16, |x| x * 3.0856775813e16),
        (|x| x / 9.461e15, |x| x * 9.461e15),
        (|x| x / 1.49597870690e11, |x| x * 1.49597870690e11),
        (|x| x, |x| x),
        (|x| x * 1e10, |x| x / 1e10),
    ],
);
pub const TIME_INFO: ([&str; 7], [(ConvFromSi, ConvToSi); 7]) = (
    ["y", "mo", "wk", "day", "h", "min", "s"],
    [
        (|x| x / 3.15576e7, |x| x * 3.15576e7),
        (|x| x / 2.592e6, |x| x * 2.592e6),
        (|x| x / 6.048e5, |x| x * 6.048e5),
        (|x| x / 8.64e4, |x| x * 8.64e4),
        (|x| x / 3.6e3, |x| x * 3.6e3),
        (|x| x / 60.0, |x| x * 60.0),
        (|x| x, |x| x),
    ],
);
pub const MASS_INFO: ([&str; 3], [(ConvFromSi, ConvToSi); 3]) = (
    ["t", "kg", "g"],
    [
        (|x| x / 1e3, |x| x * 1e3),
        (|x| x, |x| x),
        (|x| x * 1e3, |x| x / 1e3),
    ],
);
pub const EL_CURRENT_INFO: ([&str; 1], [(ConvFromSi, ConvToSi); 1]) = (["A"], [(|x| x, |x| x)]);
pub const TEMPERATURE_INFO: ([&str; 3], [(ConvFromSi, ConvToSi); 3]) = (
    ["C", "F", "K"],
    [
        (|x| x - 273.15, |x| x + 273.15),
        (
            |x| (x - 273.15) * 9.0 / 5.0 + 32.0,
            |x| (x - 32.0) * 5.0 / 9.0 + 273.15,
        ),
        (|x| x, |x| x),
    ],
);
pub const AM_OF_SUBSTANCE_INFO: ([&str; 1], [(ConvFromSi, ConvToSi); 1]) =
    (["mole"], [(|x| x, |x| x)]);
pub const LUMINOUS_IN_INFO: ([&str; 1], [(ConvFromSi, ConvToSi); 1]) = (["cd"], [(|x| x, |x| x)]);
pub const ANGLE_INFO: ([&str; 5], [(ConvFromSi, ConvToSi); 5]) = (
    ["rad", "°", "h", "'", r#"""#],
    [
        (|x| x * PI / 180.0, |x| x * 180.0 / PI),
        (|x| x, |x| x),
        (|x| x * 15.0, |x| x / 15.0),
        (|x| x * 60.0, |x| x / 60.0),
        (|x| x * 3.6e3, |x| x / 3.6e3),
    ],
);
pub const ND_INFO: ([&str; 1], [(ConvFromSi, ConvToSi); 1]) = ([""], [(|x| x, |x| x)]);

pub type ConvFromSi = fn(f64) -> f64;
pub type ConvToSi = fn(f64) -> f64;
