use std::f64::consts::PI;
use std::ops::*;
use std::fmt::{self, Display, Formatter, LowerExp, UpperExp};
use std::sync::Arc;
use crate::DimensionBase::*;

#[derive(Copy, Clone)]
pub enum DimensionBase {
    Length,
    Time,
    Mass,
    ElCurrent,
    Temperature,
    AmOfSubstance,
    LuminousIn,
    Angle,
    ND
}

impl Display for DimensionBase {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Length => { write!(f, "{}", "Length") }
            Time => { write!(f, "{}", "Time") }
            Mass => { write!(f, "{}", "Mass") }
            ElCurrent => { write!(f, "{}", "ElCurrent") }
            Temperature => { write!(f, "{}", "Temperature") }
            AmOfSubstance => { write!(f, "{}", "AmOfSubstance") }
            LuminousIn => { write!(f, "{}", "LuminousIn") }
            Angle => { write!(f, "{}", "Angle") }
            ND => { write!(f, "{}", "ND") }
        }
    }
}

#[derive(Clone)]
pub struct Dimension {
    value: f64,
    prefix: usize,
    unit: usize,
    base_name: DimensionBase,
    si_unit: &'static str,
    prefixes: Arc<[(&'static str, f64)]>,
    units: Arc<[&'static str]>,
    conv_rates: Arc<[(fn(f64) -> f64, fn(f64) -> f64)]>
}

impl Dimension {
    fn get_unit(d: &Dimension, unit: &str) -> usize {
        d.units.iter().position(|x| x == &unit).unwrap()
    }

    pub fn get_unit_owner(unit: &str) -> Result<DimensionBase, String> {
        let bases = [Length, Time, Mass, ElCurrent, Temperature, AmOfSubstance, LuminousIn, Angle, ND];

        for base in bases {
            let d = Dimension::get_base(base);
            let unit_pos = Dimension::separate_unit(&d, unit);
            if d.units.contains(&unit) || unit_pos.is_some() {
                return Ok(base)
            }
        }

        Err(format!("`{}` unit was not found in any `Dimension`", unit))
    }

    fn get_base(base: DimensionBase) -> Dimension {
        match base {
            Length => {
                Dimension {
                    value: 0.0,
                    prefix: 6,
                    unit: 3,
                    base_name: Length,
                    si_unit: "m",
                    prefixes: Arc::new([("T", 1e12), ("G", 1e9), ("M", 1e6), ("k", 1e3), ("h", 1e2), ("da", 1e1), ("", 1.0), ("d", 1e-1), ("c", 1e-2), ("m", 1e-3), ("u", 1e-6), ("n", 1e-9), ("p", 1e-12), ("f", 1e-15)]),
                    units: Arc::new(["pc", "ly", "au", "m", "ang"]),
                    conv_rates: Arc::new([(|x| x / 3.0856775813e16, |x| x * 3.0856775813e16), (|x| x / 9.461e15, |x| x * 9.461e15), (|x| x / 1.49597870690e11, |x| x * 1.49597870690e11), (|x| x, |x| x), (|x| x * 1e10, |x| x / 1e10)])
                }
            },
            Time => {
                Dimension {
                    value: 0.0,
                    prefix: 6,
                    unit: 6,
                    base_name: Time,
                    si_unit: "s",
                    prefixes: Arc::new([("T", 1e12), ("G", 1e9), ("M", 1e6), ("k", 1e3), ("h", 1e2), ("da", 1e1), ("", 1.0), ("d", 1e-1), ("c", 1e-2), ("m", 1e-3), ("u", 1e-6), ("n", 1e-9), ("p", 1e-12), ("f", 1e-15)]),
                    units: Arc::new(["y", "mo", "wk", "day", "hr", "min", "s"]),
                    conv_rates: Arc::new([(|x| x / 3.1556952e7, |x| x * 3.1556952e7), (|x| x / 2.592e6, |x| x * 2.592e6), (|x| x / 6.048e5, |x| x * 6.048e5), (|x| x / 8.64e4, |x| x * 8.64e4), (|x| x / 3.6e3, |x| x * 3.6e3), (|x| x / 60.0, |x| x * 60.0), (|x| x, |x| x)])
                }
            },
            Mass => {
                Dimension {
                    value: 0.0,
                    prefix: 5,
                    unit: 1,
                    base_name: Mass,
                    si_unit: "kg",
                    prefixes: Arc::new([("T", 1e12), ("G", 1e9), ("M", 1e6), ("h", 1e2), ("da", 1e1), ("", 1.0), ("d", 1e-1), ("c", 1e-2), ("m", 1e-3), ("u", 1e-6), ("n", 1e-9), ("p", 1e-12), ("f", 1e-15)]),
                    units: Arc::new(["t", "kg", "g"]),
                    conv_rates: Arc::new([(|x| x / 1e3, |x| x * 1e3), (|x| x, |x| x), (|x| x * 1e3, |x| x / 1e3)])
                }
            },
            ElCurrent => {
                Dimension {
                    value: 0.0,
                    prefix: 6,
                    unit: 0,
                    base_name: ElCurrent,
                    si_unit: "A",
                    prefixes: Arc::new([("T", 1e12), ("G", 1e9), ("M", 1e6), ("k", 1e3), ("h", 1e2), ("da", 1e1), ("", 1.0), ("d", 1e-1), ("c", 1e-2), ("m", 1e-3), ("u", 1e-6), ("n", 1e-9), ("p", 1e-12), ("f", 1e-15)]),
                    units: Arc::new(["A"]),
                    conv_rates: Arc::new([])
                }
            },
            Temperature => {
                Dimension {
                    value: 0.0,
                    prefix: 6,
                    unit: 2,
                    base_name: Temperature,
                    si_unit: "K",
                    prefixes: Arc::new([("T", 1e12), ("G", 1e9), ("M", 1e6), ("k", 1e3), ("h", 1e2), ("da", 1e1), ("", 1.0), ("d", 1e-1), ("c", 1e-2), ("m", 1e-3), ("u", 1e-6), ("n", 1e-9), ("p", 1e-12), ("f", 1e-15)]),
                    units: Arc::new(["C", "F", "K"]),
                    conv_rates: Arc::new([(|x| x - 273.15, |x| x + 273.15), (|x| { (x - 273.15) * 9.0/5.0 + 32.0 }, |x| { (x - 32.0) * 5.0/9.0 + 273.15 }), (|x| x, |x| x)])
                }
            },
            AmOfSubstance => {
                Dimension {
                    value: 0.0,
                    prefix: 6,
                    unit: 0,
                    base_name: AmOfSubstance,
                    si_unit: "mole",
                    prefixes: Arc::new([("T", 1e12), ("G", 1e9), ("M", 1e6), ("k", 1e3), ("h", 1e2), ("da", 1e1), ("", 1.0), ("d", 1e-1), ("c", 1e-2), ("m", 1e-3), ("u", 1e-6), ("n", 1e-9), ("p", 1e-12), ("f", 1e-15)]),
                    units: Arc::new(["mole"]),
                    conv_rates: Arc::new([])
                }
            },
            LuminousIn => {
                Dimension {
                    value: 0.0,
                    prefix: 6,
                    unit: 0,
                    base_name: LuminousIn,
                    si_unit: "cd",
                    prefixes: Arc::new([("T", 1e12), ("G", 1e9), ("M", 1e6), ("k", 1e3), ("h", 1e2), ("da", 1e1), ("", 1.0), ("d", 1e-1), ("c", 1e-2), ("m", 1e-3), ("u", 1e-6), ("n", 1e-9), ("p", 1e-12), ("f", 1e-15)]),
                    units: Arc::new(["cd"]),
                    conv_rates: Arc::new([])
                }
            },
            Angle => {
                Dimension {
                    value: 0.0,
                    prefix: 6,
                    unit: 1,
                    base_name: Angle,
                    si_unit: "°",
                    prefixes: Arc::new([("T", 1e12), ("G", 1e9), ("M", 1e6), ("k", 1e3), ("h", 1e2), ("da", 1e1), ("", 1.0), ("d", 1e-1), ("c", 1e-2), ("m", 1e-3), ("u", 1e-6), ("n", 1e-9), ("p", 1e-12), ("f", 1e-15)]),
                    units: Arc::new(["rad", "°", "h", "'", r#"""#]),
                    conv_rates: Arc::new([(|x| x * PI / 180.0, |x| x * 180.0 / PI), (|x| x, |x| x), (|x| x * 15.0, |x| x / 15.0), (|x| x * 60.0, |x| x / 60.0), (|x| x * 3.6e3, |x| x / 3.6e3)])
                }
            },
            ND => {
                Dimension {
                    value: 0.0,
                    prefix: 6,
                    unit: 0,
                    base_name: ND,
                    si_unit: "ND",
                    prefixes: Arc::new([("T", 1e12), ("G", 1e9), ("M", 1e6), ("k", 1e3), ("h", 1e2), ("da", 1e1), ("", 1.0), ("d", 1e-1), ("c", 1e-2), ("m", 1e-3), ("u", 1e-6), ("n", 1e-9), ("p", 1e-12), ("f", 1e-15)]),
                    units: Arc::new(["ND"]),
                    conv_rates: Arc::new([])
                }
            }
        }
    }

    fn separate_unit(base: &Dimension, unit: &str) -> Option<(usize, usize)> {
        let mut chars = unit.chars();
        let (prefix, unit) = (chars.next().unwrap(), String::from_iter(chars));
        let prefix_pos = base.prefixes.iter().position(|x| &x.0.to_string() == &prefix.to_string());
        let unit_pos = base.units.iter().position(|x| x == &unit);
        if unit_pos.is_some() && prefix_pos.is_some() {
            Some((prefix_pos.unwrap(), unit_pos.unwrap()))
        }
        else {
            None
        }
    }

    pub fn remove_prefix(&mut self) {
        self.value = self.value * self.prefixes[self.prefix].1;
        self.prefix = self.prefixes.iter().position(|x| x.0 == "").unwrap();
    }

    fn verify_unit(d: &Dimension, unit: &str) -> Result<(), String> {
        if d.units.contains(&unit) {
            Ok(())
        } else { Err(format!("unitError: Dimension `{}` does not contain the `{}` unit.", d.base_name, unit)) }
    }

    pub fn new(base: DimensionBase) -> Dimension {
        Dimension::get_base(base)
    }

    pub fn init(value: f64, unit: &str, base_name: DimensionBase) -> Result<Dimension, String> {
        let base = Dimension::get_base(base_name);
        let separated_unit = Dimension::separate_unit(&base, unit);
        let (prefix, unit_pos): (usize, usize);
        if separated_unit.is_none() {
            Dimension::verify_unit(&base, unit)?;
            prefix = base.prefixes.iter().position(|x| x.0 == "").unwrap();
            unit_pos = Dimension::get_unit(&base, unit);
        }
        else {
            let separated_unit = separated_unit.unwrap();
            prefix = separated_unit.0;
            unit_pos = separated_unit.1;
        }
        Ok(Dimension {
            prefix,
            unit: unit_pos,
            value,
            ..base
        })
    }

    pub fn init_nd(value: f64) -> Dimension {
        Dimension { value, ..Dimension::get_base(ND) }
    }

    pub fn as_si(&self) -> Dimension {
        let mut d = self.clone();
        d.to_si();
        d
    }

    pub fn to_si(&mut self) {
        self.remove_prefix();
        let unit = Dimension::get_unit(self, self.si_unit);
        self.value = self.conv_rates[self.unit].1(self.value);
        self.unit = unit;
    }

    pub fn as_unit(&self, unit: &str) -> Result<Dimension, String> {
        let mut d = self.clone();
        d.to_unit(unit)?;
        Ok(d)
    }

    pub fn to_unit(&mut self, unit: &str) -> Result<(), String> {
        let separated_unit = Dimension::separate_unit(&self, unit);
        let (prefix, unit_pos): (usize, usize);
        if separated_unit.is_none() {
            Dimension::verify_unit(&self, unit)?;
            prefix = self.prefixes.iter().position(|x| x.0 == "").unwrap();
            unit_pos = Dimension::get_unit(&self, unit);
        }
        else {
            let separated_unit = separated_unit.unwrap();
            prefix = separated_unit.0;
            unit_pos = separated_unit.1;
        }
        let unit = unit_pos;
        if unit != self.unit || prefix != self.prefix {
            self.to_si();
            self.value = self.conv_rates[unit].0(self.value) * self.prefixes[prefix].1.powi(-1);
            self.prefix = prefix;
            self.unit = unit;
        }
        Ok(())
    }

    pub fn verified_add(&self, other: Dimension) -> Result<Dimension, String> {
        if self.unit == other.unit {
            Dimension::init(self.value + other.value, self.units[self.unit], self.base_name)
        } else { Err(format!("unitError: {} can't add {}", self.units[self.unit], self.units[other.unit])) }
    }

    pub fn pow(&self, exp: f64) -> Dimension {
        Dimension::init(self.value.powf(exp), self.units[self.unit], self.base_name).unwrap()
    }

    pub fn get_value(&self) -> f64 {
        self.value
    }

    pub fn units_vs_si(base: DimensionBase) {
        let d = Dimension::get_base(base) + 1;
        for unit in d.units.iter() {
            println!("{:4} {}", unit.to_string() + ":", Dimension::init(1.0, unit, base).unwrap().as_unit(d.si_unit).unwrap())
        }
    }

    pub fn units_vs_unit(base: DimensionBase, unit: &str) {
        let d = Dimension::get_base(base) + 1;
        for c_unit in d.units.iter() {
            println!("{:4} {}", c_unit.to_string() + ":", Dimension::init(1.0, c_unit, base).unwrap().as_unit(unit).unwrap())
        }
    }
}

impl Display for Dimension {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}{}{}", self.value, self.prefixes[self.prefix].0, self.units[self.unit])
    }
}

impl UpperExp for Dimension {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:E}{}{}", self.value, self.prefixes[self.prefix].0, self.units[self.unit])
    }
}

impl LowerExp for Dimension {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{:e}{}{}", self.value, self.prefixes[self.prefix].0, self.units[self.unit])
    }
}

impl Add<Dimension> for Dimension {
    type Output = Dimension;

    fn add(self, other: Dimension) -> Dimension {
        let complete_unit = self.prefixes[self.prefix].0.to_string() + self.units[self.unit];
        Dimension::init(self.value + other.as_unit(&complete_unit).unwrap().value, &complete_unit, self.base_name).unwrap()
    }
}

impl Add<f64> for Dimension {
    type Output = Dimension;

    fn add(self, other: f64) -> Dimension {
        Dimension::init(self.value + other, self.units[self.unit], self.base_name).unwrap()
    }
}

impl Add<i32> for Dimension {
    type Output = Dimension;

    fn add(self, other: i32) -> Dimension {
        let other = other as f64;
        Dimension::init(self.value + other, self.units[self.unit], self.base_name).unwrap()
    }
}

impl Sub<Dimension> for Dimension {
    type Output = Dimension;

    fn sub(self, other: Dimension) -> Dimension {
        let complete_unit = self.prefixes[self.prefix].0.to_string() + self.units[self.unit];
        Dimension::init(self.value - other.as_unit(&complete_unit).unwrap().value, &complete_unit, self.base_name).unwrap()
    }
}

impl Sub<f64> for Dimension {
    type Output = Dimension;

    fn sub(self, other: f64) -> Dimension {
        Dimension::init(self.value - other, self.units[self.unit], self.base_name).unwrap()
    }
}

impl Sub<i32> for Dimension {
    type Output = Dimension;

    fn sub(self, other: i32) -> Dimension {
        let other = other as f64;
        Dimension::init(self.value - other, self.units[self.unit], self.base_name).unwrap()
    }
}

impl Mul<Dimension> for Dimension {
    type Output = Dimension;

    fn mul(self, other: Dimension) -> Dimension {
        let complete_unit = self.prefixes[self.prefix].0.to_string() + self.units[self.unit];
        Dimension::init(self.value * other.as_unit(&complete_unit).unwrap().value, &complete_unit, self.base_name).unwrap()
    }
}

impl Mul<f64> for Dimension {
    type Output = Dimension;

    fn mul(self, other: f64) -> Dimension {
        Dimension::init(self.value * other, self.units[self.unit], self.base_name).unwrap()
    }
}

impl Mul<i32> for Dimension {
    type Output = Dimension;

    fn mul(self, other: i32) -> Dimension {
        let other = other as f64;
        Dimension::init(self.value * other, self.units[self.unit], self.base_name).unwrap()
    }
}

impl Div<Dimension> for Dimension {
    type Output = Dimension;

    fn div(self, other: Dimension) -> Dimension {
        let complete_unit = self.prefixes[self.prefix].0.to_string() + self.units[self.unit];
        Dimension::init(self.value / other.as_unit(&complete_unit).unwrap().value, &complete_unit, self.base_name).unwrap()
    }
}

impl Div<f64> for Dimension {
    type Output = Dimension;

    fn div(self, other: f64) -> Dimension {
        Dimension::init(self.value / other, self.units[self.unit], self.base_name).unwrap()
    }
}

impl Div<i32> for Dimension {
    type Output = Dimension;

    fn div(self, other: i32) -> Dimension {
        let other = other as f64;
        Dimension::init(self.value / other, self.units[self.unit], self.base_name).unwrap()
    }
}

impl Rem<Dimension> for Dimension {
    type Output = Dimension;

    fn rem(self, other: Dimension) -> Dimension {
        let complete_unit = self.prefixes[self.prefix].0.to_string() + self.units[self.unit];
        Dimension::init(self.value % other.as_unit(&complete_unit).unwrap().value, &complete_unit, self.base_name).unwrap()
    }
}

impl Rem<f64> for Dimension {
    type Output = Dimension;

    fn rem(self, other: f64) -> Dimension {
        Dimension::init(self.value % other, self.units[self.unit], self.base_name).unwrap()
    }
}

impl Rem<i32> for Dimension {
    type Output = Dimension;

    fn rem(self, other: i32) -> Dimension {
        let other = other as f64;
        Dimension::init(self.value % other, self.units[self.unit], self.base_name).unwrap()
    }
}

impl AddAssign<Dimension> for Dimension {
    fn add_assign(&mut self, other: Dimension) {
        let complete_unit = self.prefixes[self.prefix].0.to_string() + self.units[self.unit];
        self.value += other.as_unit(&complete_unit).unwrap().value
    }
}

impl AddAssign<f64> for Dimension {
    fn add_assign(&mut self, other: f64) {
        self.value += other
    }
}

impl AddAssign<i32> for Dimension {
    fn add_assign(&mut self, other: i32) {
        let other = other as f64;
        self.value += other
    }
}

impl SubAssign<Dimension> for Dimension {
    fn sub_assign(&mut self, other: Dimension) {
        let complete_unit = self.prefixes[self.prefix].0.to_string() + self.units[self.unit];
        self.value -= other.as_unit(&complete_unit).unwrap().value
    }
}

impl SubAssign<f64> for Dimension {
    fn sub_assign(&mut self, other: f64) {
        self.value -= other
    }
}

impl SubAssign<i32> for Dimension {
    fn sub_assign(&mut self, other: i32) {
        let other = other as f64;
        self.value -= other
    }
}

impl MulAssign<Dimension> for Dimension {
    fn mul_assign(&mut self, other: Dimension) {
        let complete_unit = self.prefixes[self.prefix].0.to_string() + self.units[self.unit];
        self.value *= other.as_unit(&complete_unit).unwrap().value
    }
}

impl MulAssign<f64> for Dimension {
    fn mul_assign(&mut self, other: f64) {
        self.value *= other
    }
}

impl MulAssign<i32> for Dimension {
    fn mul_assign(&mut self, other: i32) {
        let other = other as f64;
        self.value *= other
    }
}

impl DivAssign<Dimension> for Dimension {
    fn div_assign(&mut self, other: Dimension) {
        let complete_unit = self.prefixes[self.prefix].0.to_string() + self.units[self.unit];
        self.value /= other.as_unit(self.units[self.unit]).unwrap().value
    }
}

impl DivAssign<f64> for Dimension {
    fn div_assign(&mut self, other: f64) {
        self.value /= other
    }
}

impl DivAssign<i32> for Dimension {
    fn div_assign(&mut self, other: i32) {
        let other = other as f64;
        self.value /= other
    }
}

impl RemAssign<Dimension> for Dimension {
    fn rem_assign(&mut self, other: Dimension) {
        let complete_unit = self.prefixes[self.prefix].0.to_string() + self.units[self.unit];
        self.value %= other.as_unit(&complete_unit).unwrap().value
    }
}

impl RemAssign<f64> for Dimension {
    fn rem_assign(&mut self, other: f64) {
        self.value %= other
    }
}

impl RemAssign<i32> for Dimension {
    fn rem_assign(&mut self, other: i32) {
        let other = other as f64;
        self.value %= other
    }
}

impl Neg for Dimension {
    type Output = Dimension;

    fn neg(self) -> Dimension {
        let complete_unit = self.prefixes[self.prefix].0.to_string() + self.units[self.unit];
        Dimension::init(-self.value, &complete_unit, self.base_name).unwrap()
    }
}