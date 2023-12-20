extern crate log;

use crate::DimenError::*;
use crate::GeneDimen::*;
use crate::SimpDimenBase::*;
use env_logger::{self, Env};
use log::warn;
use std::f64::consts::PI;
use std::fmt::{self, Debug, Display, Formatter, LowerExp, UpperExp};
use std::ops::*;
use std::sync::Arc;

fn take_num_unit<T: AsRef<str>>(
    owner: &'static str,
    value: T,
) -> Result<(f64, String), DimenError> {
    let value = value.as_ref();
    let number_str = value
        .split_once(|x: char| x.is_alphabetic())
        .ok_or(InvalidValError(owner))?;

    let unit_str = value
        .rsplit_once(|x: char| x.is_numeric())
        .ok_or(InvalidValError(owner))?;

    Ok((
        number_str.0.parse::<f64>().expect(
            format!(
                "Number received a non-numeric value in `{}::soft_from`.",
                owner
            )
            .as_ref(),
        ),
        unit_str.1.to_string(),
    ))
}

pub type OwnerName = &'static str;
pub type OwnerBase = Option<String>;
pub type Unit = String;
pub type Operator = Option<String>;
pub type Operation = &'static str;
pub type Attribute = &'static str;

#[derive(Clone)]
pub enum DimenError {
    UnitError(OwnerName, OwnerBase, Unit),
    DiffLenError(OwnerName, Attribute, Attribute),
    InvalidValError(OwnerName),
    OperatorError(Operator),
    VerifiedOpError(OwnerName, Unit, Operation, OwnerName, Unit),
}

impl Display for DimenError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            UnitError(owner_name, owner_base, unit) => {
                if let Some(owner_base) = owner_base {
                    write!(
                        f,
                        "UnitError: `{}` with base `{}` does not contain the `{}` unit.",
                        owner_name, owner_base, unit
                    )
                } else {
                    write!(
                        f,
                        "UnitError: Any `{}` contains the `{}` unit.",
                        owner_name, unit
                    )
                }
            }
            DiffLenError(owner_name, attribute1, attribute2) => {
                write!(
                    f,
                    "DiffLenError: The `{}` attributes `{}` and `{}` must have the same length.",
                    owner_name, attribute1, attribute2
                )
            }
            InvalidValError(owner_name) => {
                write!(
                    f,
                    "InvalidValError: The provided value to `{}::from` must include \
                both a numerical value and a unit in order.",
                    owner_name
                )
            }
            OperatorError(op) => {
                if let Some(op) = op {
                    write!(
                        f,
                        "OperatorError: No implementations to `{}` operator for `CompDimen`. \
                    Try one of: (+, -, *, /, ^)",
                        op
                    )
                } else {
                    write!(f, "OperatorError: `CompDimen` can not be built without an operator. Try `SimpDimen`.")
                }
            }
            VerifiedOpError(owner_name1, unit1, operation, owner_name2, unit2) => {
                write!(
                    f,
                    "VerifiedOpError: `{}` with unit `{}` expected a `{}` with unit `{}` in the `verified_{}` operation \
                    but received `{}` with unit `{}`.",
                    owner_name1, unit1, owner_name1, unit1, operation, owner_name2, unit2
                )
            }
        }
    }
}

impl Debug for DimenError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

pub type NewDimen<Dimen> = Result<Dimen, DimenError>;
pub type ModDimen<Dimen> = Result<Dimen, DimenError>;

pub trait Pow<Rhs = Self> {
    type Output;
    fn powf(self, rhs: Rhs) -> Self::Output;
}

pub trait DimenBasics {
    fn to_si(&self) -> Self
    where
        Self: Sized + Clone,
    {
        let mut d = self.clone();
        d.bcm_si();
        d
    }

    fn to_unit<T: AsRef<str>>(&self, unit: T) -> ModDimen<Self>
    where
        Self: Sized + Clone,
    {
        let mut d = self.clone();
        d.bcm_unit(unit)?;
        Ok(d)
    }

    fn bcm_si(&mut self);

    fn bcm_unit<T: AsRef<str>>(&mut self, unit: T) -> Result<(), DimenError>;

    fn to_generic(self) -> GeneDimen;

    fn verified_add(&self, other: &Self) -> ModDimen<Self>
    where
        Self: Sized;

    fn verified_sub(&self, other: &Self) -> ModDimen<Self>
    where
        Self: Sized;

    fn verified_mul(&self, other: &Self) -> ModDimen<Self>
    where
        Self: Sized;

    fn verified_div(&self, other: &Self) -> ModDimen<Self>
    where
        Self: Sized;

    fn verified_powf(&self, other: &Self) -> ModDimen<Self>
    where
        Self: Sized;
}

pub trait DimenBaseDependent<Base> {
    fn init(value: f64, unit: &str, base: Base) -> NewDimen<Self>
    where
        Self: Sized;

    fn get_base(&self) -> Base;
}

pub trait DimenSetAndGet {
    fn set_value(&mut self, other: f64);

    fn get_value(&self) -> f64;

    fn get_unit(&self) -> String;

    fn get_base_to_display(&self) -> String;

    fn get_name() -> &'static str;
}

macro_rules! impl_ops_d_for_d {
    ($($d1:ident, $d2:ident);+) => {
        $(
            impl Add<$d2> for $d1 {
                type Output = GeneDimen;

                fn add(mut self, other: $d2) -> GeneDimen {
                    if other.get_unit().is_empty() {
                        self.set_value(self.get_value() + other.get_value());
                        self.to_generic()
                    }
                    else {
                        let other_result = other.to_unit(self.get_unit());
                        if let Ok(other) = other_result {
                            self.set_value(self.get_value() + other.get_value());
                            self.to_generic()
                        }
                        else {
                            GeneDimen::from(
                                CompDimen::from(
                                    (
                                        Box::new(self.to_generic()),
                                        '+',
                                        Box::new(other.to_generic())
                                    )
                                )
                            )
                        }
                    }
                }
            }

            impl Sub<$d2> for $d1 {
                type Output = GeneDimen;

                fn sub(mut self, other: $d2) -> GeneDimen {
                    if other.get_unit().is_empty() {
                        self.set_value(self.get_value() - other.get_value());
                        self.to_generic()
                    }
                    else {
                        let other_result = other.to_unit(self.get_unit());
                        if let Ok(other) = other_result {
                            self.set_value(self.get_value() - other.get_value());
                            self.to_generic()
                        }
                        else {
                            GeneDimen::from(
                                CompDimen::from(
                                    (
                                        Box::new(self.to_generic()),
                                        '-',
                                        Box::new(other.to_generic())
                                    )
                                )
                            )
                        }
                    }
                }
            }

            impl Mul<$d2> for $d1 {
                type Output = GeneDimen;

                fn mul(mut self, other: $d2) -> GeneDimen {
                    if other.get_unit().is_empty() {
                        self.set_value(self.get_value() * other.get_value());
                        self.to_generic()
                    }
                    else {
                        let mut other = other;
                        let other_result = other.to_unit(self.get_unit());
                        if let Ok(new_other) = other_result {
                            other = new_other;
                        };
                        GeneDimen::from(
                            CompDimen::from(
                                (
                                    Box::new(self.to_generic()),
                                    '*',
                                    Box::new(other.to_generic())
                                )
                            )
                        )
                    }
                }
            }

            impl Div<$d2> for $d1 {
                type Output = GeneDimen;

                fn div(mut self, other: $d2) -> GeneDimen {
                    if other.get_unit().is_empty() {
                        self.set_value(self.get_value() / other.get_value());
                        self.to_generic()
                    }
                    else {
                        let other_result = other.to_unit(self.get_unit());
                        if let Ok(other) = other_result {
                            GeneDimen::from(
                                SimpDimen::init_nd( self.get_value() / other.get_value() )
                            )
                        }
                        else {
                            GeneDimen::from(
                                CompDimen::from(
                                    (
                                        Box::new(self.to_generic()),
                                        '/',
                                        Box::new(other.to_generic())
                                    )
                                )
                            )
                        }
                    }
                }
            }

            impl Pow<$d2> for $d1 {
                type Output = GeneDimen;

                fn powf(self, other: $d2) -> GeneDimen {
                    let other_result = other.to_unit(self.get_unit());
                    let mut other = other;
                    if let Ok(new_other) = other_result {
                        other = new_other;
                    }
                    GeneDimen::from(
                        CompDimen::from(
                            (
                                Box::new(self.to_generic()),
                                '^',
                                Box::new(other.to_generic())
                            )
                        )
                    )
                }
            }
        )*
    }
}

macro_rules! impl_ops_f64_for_d {
    ($($t:ident, $tfn:ident, $d: ident);+) => {
        $(impl $t<f64> for $d {
            type Output = $d;

            fn $tfn(mut self, other: f64) -> $d {
                self.set_value(self.get_value().$tfn(other));
                self
            }
        })*
    }
}

macro_rules! impl_neg_for_d {
    ($($d: ident),+) => {
        $(impl Neg for $d {
            type Output = $d;

            fn neg(mut self) -> $d {
                self.set_value(-self.get_value());
                self
            }
        })*
    }
}

macro_rules! impl_ops_asn_f64_for_d {
    ($($t:ident, $tfn:ident, $op:tt, $d:ident);+) => {
        $(impl $t<f64> for $d {
            fn $tfn(&mut self, other: f64) {
                self.set_value(self.get_value() $op other);
            }
        })*
    }
}

macro_rules! pass_ds_to_impl_ops {
    ($($d:ident),+) => {
        $(
            impl_ops_d_for_d!($d, SimpDimen; $d, CompDimen; $d, GeneDimen);
            impl_ops_f64_for_d!(Add, add, $d; Sub, sub, $d; Mul, mul, $d; Div, div, $d; Pow, powf, $d);
            impl_neg_for_d!($d);
            impl_ops_asn_f64_for_d!(AddAssign, add_assign, +, $d; SubAssign, sub_assign, -, $d; MulAssign, mul_assign, *, $d; DivAssign, div_assign, /, $d);
        )*
    };
}

macro_rules! impl_ops_num_for_d {
    ($($t:ident, $tfn:ident, $ty:ty, $d: ident);+) => {
        $(
            impl $t<$ty> for $d {
                type Output = $d;

                fn $tfn(mut self, other: $ty) -> $d {
                    let other = other as f64;
                    self.set_value(self.get_value().$tfn(other));
                    self
                }
            }
        )*
    };
}

macro_rules! impl_ops_asn_num_for_d {
    ($($t:ident, $tfn:ident, $op:tt, $ty:ty, $d: ident);+) => {
        $(
            impl $t<$ty> for $d {
                fn $tfn(&mut self, other: $ty) {
                    let other = other as f64;
                    self.set_value(self.get_value() $op other);
                }
            }
        )*
    };
}

macro_rules! pass_nums_to_impl_ops {
    ($($ty:ty),+) => {
        $(
            impl_ops_num_for_d!(Add, add, $ty, SimpDimen; Sub, sub, $ty, SimpDimen; Mul, mul, $ty, SimpDimen; Div, div, $ty, SimpDimen; Pow, powf, $ty, SimpDimen);
            impl_ops_num_for_d!(Add, add, $ty, CompDimen; Sub, sub, $ty, CompDimen; Mul, mul, $ty, CompDimen; Div, div, $ty, CompDimen; Pow, powf, $ty, CompDimen);
            impl_ops_num_for_d!(Add, add, $ty, GeneDimen; Sub, sub, $ty, GeneDimen; Mul, mul, $ty, GeneDimen; Div, div, $ty, GeneDimen; Pow, powf, $ty, GeneDimen);

            impl_ops_asn_num_for_d!(AddAssign, add_assign, +, $ty, SimpDimen; SubAssign, sub_assign, -, $ty, SimpDimen; MulAssign, mul_assign, *, $ty, SimpDimen; DivAssign, div_assign, /, $ty, SimpDimen);
            impl_ops_asn_num_for_d!(AddAssign, add_assign, +, $ty, CompDimen; SubAssign, sub_assign, -, $ty, CompDimen; MulAssign, mul_assign, *, $ty, CompDimen; DivAssign, div_assign, /, $ty, CompDimen);
            impl_ops_asn_num_for_d!(AddAssign, add_assign, +, $ty, GeneDimen; SubAssign, sub_assign, -, $ty, GeneDimen; MulAssign, mul_assign, *, $ty, GeneDimen; DivAssign, div_assign, /, $ty, GeneDimen);
        )*
    };
}

macro_rules! impl_ops_asn_gd_for_gd {
    ($($t:ident, $tfn:ident, $op:tt);+) => {
        $(impl $t<GeneDimen> for GeneDimen {
            fn $tfn(&mut self, other: GeneDimen) {
                *self = self.clone() $op other;
            }
        })*
    }
}

macro_rules! impl_exp_for_ds {
    ($($d:ident),+) => {
        $(
        impl UpperExp for $d {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                write!(f, "{:E}{}", self.get_value(), self.get_unit())
            }
        }

        impl LowerExp for $d {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                write!(f, "{:e}{}", self.get_value(), self.get_unit())
            }
        }
        )+
    };
}

macro_rules! impl_from_str_string_for_ds {
    ($($d: ident),+) => {
        $(impl From<&str> for $d {
            fn from(value: &str) -> Self {
                Self::soft_from(value).unwrap_or_else(|err| panic!("{}", err))
            }
        }

        impl From<String> for $d {
            fn from(value: String) -> Self {
                Self::soft_from(value).unwrap_or_else(|err| panic!("{}", err))
            }
        })*
    };
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum SimpDimenBase {
    Length,
    Time,
    Mass,
    ElCurrent,
    Temperature,
    AmOfSubstance,
    LuminousIn,
    Angle,
    ND,
    Custom(&'static str),
}

impl Display for SimpDimenBase {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

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

type ConvFromSi = fn(f64) -> f64;
type ConvToSi = fn(f64) -> f64;

#[derive(Clone, PartialEq)]
pub struct SimpDimen {
    value: f64,
    prefix: usize,
    unit: usize,
    base: SimpDimenBase,
    si_unit: usize,
    prefixes: Arc<[(&'static str, f64)]>,
    units: Arc<[&'static str]>,
    conv_rates: Arc<[(ConvFromSi, ConvToSi)]>,
}

impl SimpDimen {
    fn get_unit_pos(d: &SimpDimen, unit: &str) -> usize {
        d.units.iter().position(|x| x == &unit).unwrap()
    }

    fn get_base_info(base: SimpDimenBase) -> SimpDimen {
        match base {
            Length => SimpDimen {
                value: 0.0,
                prefix: 6,
                unit: 3,
                base: Length,
                si_unit: 3,
                prefixes: Arc::new(PREFIXES),
                units: Arc::new(["pc", "ly", "au", "m", "ang"]),
                conv_rates: Arc::new([
                    (|x| x / 3.0856775813e16, |x| x * 3.0856775813e16),
                    (|x| x / 9.461e15, |x| x * 9.461e15),
                    (|x| x / 1.49597870690e11, |x| x * 1.49597870690e11),
                    (|x| x, |x| x),
                    (|x| x * 1e10, |x| x / 1e10),
                ]),
            },
            Time => SimpDimen {
                value: 0.0,
                prefix: 6,
                unit: 6,
                base: Time,
                si_unit: 6,
                prefixes: Arc::new(PREFIXES),
                units: Arc::new(["y", "mo", "wk", "day", "h", "min", "s"]),
                conv_rates: Arc::new([
                    (|x| x / 3.15576e7, |x| x * 3.15576e7),
                    (|x| x / 2.592e6, |x| x * 2.592e6),
                    (|x| x / 6.048e5, |x| x * 6.048e5),
                    (|x| x / 8.64e4, |x| x * 8.64e4),
                    (|x| x / 3.6e3, |x| x * 3.6e3),
                    (|x| x / 60.0, |x| x * 60.0),
                    (|x| x, |x| x),
                ]),
            },
            Mass => SimpDimen {
                value: 0.0,
                prefix: 6,
                unit: 1,
                base: Mass,
                si_unit: 1,
                prefixes: Arc::new(PREFIXES),
                units: Arc::new(["t", "kg", "g"]),
                conv_rates: Arc::new([
                    (|x| x / 1e3, |x| x * 1e3),
                    (|x| x, |x| x),
                    (|x| x * 1e3, |x| x / 1e3),
                ]),
            },
            ElCurrent => SimpDimen {
                value: 0.0,
                prefix: 6,
                unit: 0,
                base: ElCurrent,
                si_unit: 0,
                prefixes: Arc::new(PREFIXES),
                units: Arc::new(["A"]),
                conv_rates: Arc::new([(|x| x, |x| x)]),
            },
            Temperature => SimpDimen {
                value: 0.0,
                prefix: 6,
                unit: 2,
                base: Temperature,
                si_unit: 2,
                prefixes: Arc::new(PREFIXES),
                units: Arc::new(["C", "F", "K"]),
                conv_rates: Arc::new([
                    (|x| x - 273.15, |x| x + 273.15),
                    (
                        |x| (x - 273.15) * 9.0 / 5.0 + 32.0,
                        |x| (x - 32.0) * 5.0 / 9.0 + 273.15,
                    ),
                    (|x| x, |x| x),
                ]),
            },
            AmOfSubstance => SimpDimen {
                value: 0.0,
                prefix: 6,
                unit: 0,
                base: AmOfSubstance,
                si_unit: 0,
                prefixes: Arc::new(PREFIXES),
                units: Arc::new(["mole"]),
                conv_rates: Arc::new([(|x| x, |x| x)]),
            },
            LuminousIn => SimpDimen {
                value: 0.0,
                prefix: 6,
                unit: 0,
                base: LuminousIn,
                si_unit: 0,
                prefixes: Arc::new(PREFIXES),
                units: Arc::new(["cd"]),
                conv_rates: Arc::new([(|x| x, |x| x)]),
            },
            Angle => SimpDimen {
                value: 0.0,
                prefix: 6,
                unit: 1,
                base: Angle,
                si_unit: 1,
                prefixes: Arc::new(PREFIXES),
                units: Arc::new(["rad", "°", "h", "'", r#"""#]),
                conv_rates: Arc::new([
                    (|x| x * PI / 180.0, |x| x * 180.0 / PI),
                    (|x| x, |x| x),
                    (|x| x * 15.0, |x| x / 15.0),
                    (|x| x * 60.0, |x| x / 60.0),
                    (|x| x * 3.6e3, |x| x / 3.6e3),
                ]),
            },
            ND => SimpDimen {
                value: 0.0,
                prefix: 0,
                unit: 0,
                base: ND,
                si_unit: 0,
                prefixes: Arc::new([("", 1.0)]),
                units: Arc::new([""]),
                conv_rates: Arc::new([(|x| x, |x| x)]),
            },
            Custom(name) => {
                env_logger::init_from_env(
                    Env::default()
                        .filter_or("MY_LOG_LEVEL", "trace")
                        .write_style_or("MY_LOG_STYLE", "always"),
                );
                let warning_message = "Creating a custom EDimension using `EDimension::init`, try `EDimension::create_custom` instead";
                warn!("Warning!: {}!", warning_message);
                SimpDimen {
                    value: 0.0,
                    prefix: 6,
                    unit: 0,
                    base: Custom(name),
                    si_unit: 0,
                    prefixes: Arc::new(PREFIXES),
                    units: Arc::new([""]),
                    conv_rates: Arc::new([(|x| x, |x| x)]),
                }
            }
        }
    }

    fn separate_unit(base: &SimpDimen, unit: &str) -> Option<(usize, usize)> {
        if base.get_unit().is_empty() || unit.is_empty() {
            return None;
        }

        let mut chars = unit.chars();
        let (prefix, unit) = (chars.next().unwrap(), String::from_iter(chars));

        if let (Some(prefix_pos), Some(unit_pos)) = (
            base.prefixes
                .iter()
                .position(|(x, _)| x == &prefix.to_string()),
            base.units.iter().position(|x| x == &unit),
        ) {
            Some((prefix_pos, unit_pos))
        } else {
            None
        }
    }

    fn verify_unit(d: &SimpDimen, unit: &str) -> Result<(usize, usize), DimenError> {
        if d.units.contains(&unit) {
            let prefix = if d.base != ND { 6 } else { 0 };
            Ok((prefix, Self::get_unit_pos(d, unit)))
        } else {
            Self::separate_unit(d, unit).ok_or(UnitError(
                Self::get_name(),
                Some(d.get_base_to_display()),
                unit.to_string(),
            ))
        }
    }

    pub fn val_to_display(value: f64, unit: String, show_unit: bool) -> String {
        let val_to_display = if show_unit {
            format!("{}{}", value, unit)
        } else {
            value.to_string()
        };
        val_to_display
    }

    pub fn remove_prefix(&mut self) {
        self.value *= self.prefixes[self.get_prefix()].1;
        self.prefix = if self.base != ND { 6 } else { 0 };
    }

    pub fn bcm_unit_unchecked(&mut self, prefix: usize, unit: usize) {
        if unit != self.unit || prefix != self.prefix {
            self.bcm_si();
            self.value = self.conv_rates[unit].0(self.value) * self.prefixes[prefix].1.powi(-1);
            self.prefix = prefix;
            self.unit = unit;
        }
    }

    pub fn new(base: SimpDimenBase) -> SimpDimen {
        Self::get_base_info(base)
    }

    pub fn create_custom(
        name: &'static str,
        si: &str,
        units: Arc<[&'static str]>,
        conv_rates: Vec<(ConvFromSi, ConvToSi)>,
    ) -> NewDimen<SimpDimen> {
        if units.len() != conv_rates.len() {
            return Err(DiffLenError(Self::get_name(), "units", "conv_rates"));
        }
        let mut conv_rates = conv_rates;
        conv_rates.sort_unstable_by(|a, b| a.0(1.0).partial_cmp(&b.0(1.0)).unwrap());

        let unit_err = UnitError(
            Self::get_name(),
            Some(Custom(name).to_string()),
            si.to_string(),
        );
        let unit = units.iter().position(|x| x == &si).ok_or(unit_err)?;

        Ok(SimpDimen {
            value: 0.0,
            prefix: 6,
            unit,
            base: Custom(name),
            si_unit: unit,
            prefixes: Arc::new(PREFIXES),
            units,
            conv_rates: Arc::from(conv_rates),
        })
    }

    pub fn init_nd(value: f64) -> SimpDimen {
        SimpDimen {
            value,
            ..Self::get_base_info(ND)
        }
    }

    pub fn init_custom(value: f64, unit: &str, base: SimpDimen) -> NewDimen<SimpDimen> {
        let (prefix, unit) = Self::verify_unit(&base, unit)?;
        Ok(SimpDimen {
            value,
            prefix,
            unit,
            ..base
        })
    }

    pub fn soft_from<T: AsRef<str>>(value: T) -> NewDimen<SimpDimen> {
        let (number, unit) = take_num_unit(Self::get_name(), value)?;
        Ok(Self::init(number, &unit, Self::get_unit_owner(&unit)?)?)
    }

    pub fn set_prefix(&mut self, prefix: usize) {
        self.bcm_unit_unchecked(prefix, self.unit)
    }

    pub fn set_prefix_kv(&mut self, prefix: usize) {
        self.prefix = prefix;
    }

    pub fn get_prefix(&self) -> usize {
        self.prefix
    }

    pub fn get_prefixes(&self) -> Arc<[(&'static str, f64)]> {
        self.prefixes.clone()
    }

    pub fn get_units(&self) -> Arc<[&'static str]> {
        self.units.clone()
    }

    pub fn get_unit_owner(unit: &str) -> Result<SimpDimenBase, DimenError> {
        let bases = [
            Length,
            Time,
            Mass,
            ElCurrent,
            Temperature,
            AmOfSubstance,
            LuminousIn,
            Angle,
            ND,
        ];

        for base in bases {
            let d = Self::get_base_info(base);
            let unit_pos = Self::separate_unit(&d, unit);
            if d.units.contains(&unit) || unit_pos.is_some() {
                return Ok(base);
            }
        }

        Err(UnitError(Self::get_name(), None, unit.to_string()))
    }

    pub fn get_unit_pos_c(&self) -> usize {
        self.unit
    }
}

impl DimenBasics for SimpDimen {
    fn bcm_si(&mut self) {
        if (self.unit != self.si_unit || self.prefix != 6) && self.base != ND {
            self.remove_prefix();
            let unit = self.si_unit;
            self.value = self.conv_rates[self.unit].1(self.value);
            self.unit = unit;
        }
    }

    fn bcm_unit<T: AsRef<str>>(&mut self, unit: T) -> Result<(), DimenError> {
        let (prefix, unit) = Self::verify_unit(self, unit.as_ref())?;
        self.bcm_unit_unchecked(prefix, unit);
        Ok(())
    }

    fn to_generic(self) -> GeneDimen {
        GeneDimen::from(self)
    }

    fn verified_add(&self, other: &SimpDimen) -> ModDimen<SimpDimen> {
        let unit = self.get_unit();
        let other_unit = other.get_unit();
        if unit == other_unit {
            Ok(SimpDimen {
                value: self.value + other.get_value(),
                ..self.clone()
            })
        } else {
            Err(VerifiedOpError(
                Self::get_name(),
                unit,
                "add",
                Self::get_name(),
                other_unit,
            ))
        }
    }

    fn verified_sub(&self, other: &SimpDimen) -> ModDimen<SimpDimen> {
        let unit = self.get_unit();
        let other_unit = other.get_unit();
        if unit == other_unit {
            Ok(SimpDimen {
                value: self.value - other.get_value(),
                ..self.clone()
            })
        } else {
            Err(VerifiedOpError(
                Self::get_name(),
                unit,
                "sub",
                Self::get_name(),
                other_unit,
            ))
        }
    }

    fn verified_mul(&self, other: &SimpDimen) -> ModDimen<SimpDimen> {
        let unit = self.get_unit();
        let other_unit = other.get_unit();
        if unit == other_unit {
            Ok(SimpDimen {
                value: self.value * other.get_value(),
                ..self.clone()
            })
        } else {
            Err(VerifiedOpError(
                Self::get_name(),
                unit,
                "mul",
                Self::get_name(),
                other_unit,
            ))
        }
    }

    fn verified_div(&self, other: &SimpDimen) -> ModDimen<SimpDimen> {
        let unit = self.get_unit();
        let other_unit = other.get_unit();
        if unit == other_unit {
            Ok(SimpDimen {
                value: self.value / other.get_value(),
                ..self.clone()
            })
        } else {
            Err(VerifiedOpError(
                Self::get_name(),
                unit,
                "div",
                Self::get_name(),
                other_unit,
            ))
        }
    }

    fn verified_powf(&self, other: &SimpDimen) -> ModDimen<SimpDimen> {
        let unit = self.get_unit();
        let other_unit = other.get_unit();
        if unit == other_unit {
            Ok(SimpDimen {
                value: self.value.powf(other.get_value()),
                ..self.clone()
            })
        } else {
            Err(VerifiedOpError(
                Self::get_name(),
                unit,
                "powf",
                Self::get_name(),
                other_unit,
            ))
        }
    }
}

impl DimenBaseDependent<SimpDimenBase> for SimpDimen {
    fn init(value: f64, unit: &str, base: SimpDimenBase) -> NewDimen<SimpDimen> {
        let base = Self::get_base_info(base);
        let (prefix, unit) = Self::verify_unit(&base, unit)?;
        Ok(SimpDimen {
            value,
            prefix,
            unit,
            ..base
        })
    }

    fn get_base(&self) -> SimpDimenBase {
        self.base
    }
}

impl DimenSetAndGet for SimpDimen {
    fn set_value(&mut self, other: f64) {
        self.value = other;
    }

    fn get_value(&self) -> f64 {
        self.value
    }

    fn get_unit(&self) -> String {
        self.prefixes[self.prefix].0.to_string() + self.units[self.unit]
    }

    fn get_base_to_display(&self) -> String {
        self.get_base().to_string()
    }

    fn get_name() -> &'static str {
        "SimpDimen"
    }
}

impl Display for SimpDimen {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            Self::val_to_display(self.value, self.get_unit(), true)
        )
    }
}

impl Debug for SimpDimen {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.get_unit() != "" {
            write!(f, "{}{}", self.value, self.get_unit())
        } else {
            write!(f, "{}ND", self.value)
        }
    }
}

impl From<f64> for SimpDimen {
    fn from(value: f64) -> Self {
        Self::init_nd(value)
    }
}

type CompDimenBase = (Box<GeneDimen>, char, Box<GeneDimen>);

#[derive(Clone, PartialEq, Debug)]
pub struct CompDimen {
    value: f64,
    unit: String,
    base: (Box<GeneDimen>, char, Box<GeneDimen>),
}

impl CompDimen {
    fn get_op(&self) -> char {
        self.base.1
    }

    fn get_base_values(&self) -> (f64, f64) {
        (self.base.0.get_value(), self.base.2.get_value())
    }

    fn updated_unit(base: &CompDimenBase) -> String {
        let mut unit = base.0.get_unit();
        unit.push(base.1);
        let base2_unit = base.2.get_unit();
        if !base2_unit.is_empty() {
            unit += &base.2.get_unit();
        } else {
            unit += &base.2.get_value().to_string();
        }
        unit
    }

    fn update_value(&mut self) {
        let (value1, value2) = self.get_base_values();
        self.value = match self.get_op() {
            '+' => value1 + value2,
            '-' => value1 - value2,
            '*' => value1 * value2,
            '/' => value1 / value2,
            '^' => value1.powf(value2),
            _ => {
                panic!(
                    "OperatorError: `CompDimen` does not support the `{}` operator.",
                    self.get_op()
                )
            }
        };
    }

    fn verify_op(op: &char) -> Result<(), DimenError> {
        if ['+', '-', '*', '/', '^'].contains(op) {
            Ok(())
        } else {
            Err(OperatorError(Some(op.to_string())))
        }
    }

    pub fn new(base: CompDimenBase) -> NewDimen<CompDimen> {
        Self::verify_op(&base.1)?;
        let mut base = base;
        base.0.set_value(0.0);
        base.2.set_value(0.0);
        let unit = Self::updated_unit(&base);
        Ok(CompDimen {
            value: 0.0,
            unit,
            base,
        })
    }

    pub fn soft_from<T: AsRef<str>>(value: T) -> NewDimen<CompDimen> {
        let (number, unit) = take_num_unit(Self::get_name(), value)?;
        let op = unit
            .chars()
            .rfind(|x| Self::verify_op(x).is_ok())
            .ok_or(OperatorError(None))?;

        let (unit1, unit2) = unit.rsplit_once(|x| x == op).unwrap();
        let base = match (GeneDimen::soft_from(unit1), GeneDimen::soft_from(unit2)) {
            (Ok(base1), Ok(base2)) => (Box::new(base1), op, Box::new(base2)),
            _ => return Err(UnitError(Self::get_name(), None, unit)),
        };

        Ok(Self::init(number, &unit, base).unwrap())
    }

    pub fn make_base(base1: SimpDimenBase, op: char, base2: SimpDimenBase) -> CompDimenBase {
        (
            Box::new(GeneDimen::from(SimpDimen::new(base1))),
            op,
            Box::new(GeneDimen::from(SimpDimen::new(base2))),
        )
    }
}

impl DimenBasics for CompDimen {
    fn bcm_si(&mut self) {
        self.base.0.bcm_si();
        self.base.2.bcm_si();
        self.update_value();
        self.unit = Self::updated_unit(&self.base);
    }

    fn bcm_unit<T: AsRef<str>>(&mut self, unit: T) -> Result<(), DimenError> {
        if self.unit != unit.as_ref() {
            let dimen_error = UnitError(
                Self::get_name(),
                Some(self.get_base_to_display()),
                unit.as_ref().to_string(),
            );

            let (unit1, unit2) = unit
                .as_ref()
                .rsplit_once(self.get_op())
                .ok_or(dimen_error.clone())?;

            let base1 = self.base.0.bcm_unit(unit1);
            let base2 = if !self.base.2.get_unit().is_empty() {
                self.base.2.bcm_unit(unit2)
            } else {
                Ok(())
            };
            Err(dimen_error).or(base1.and(base2))?;

            self.update_value();
            self.unit = Self::updated_unit(&self.base);
        }
        Ok(())
    }

    fn to_generic(self) -> GeneDimen {
        GeneDimen::from(self)
    }

    fn verified_add(&self, other: &CompDimen) -> ModDimen<CompDimen> {
        let unit = self.get_unit();
        let other_unit = other.get_unit();
        if unit == other_unit {
            Ok(CompDimen {
                value: self.value + other.get_value(),
                ..self.clone()
            })
        } else {
            Err(VerifiedOpError(
                Self::get_name(),
                unit,
                "add",
                Self::get_name(),
                other_unit,
            ))
        }
    }

    fn verified_sub(&self, other: &CompDimen) -> ModDimen<CompDimen> {
        let unit = self.get_unit();
        let other_unit = other.get_unit();
        if unit == other_unit {
            Ok(CompDimen {
                value: self.value - other.get_value(),
                ..self.clone()
            })
        } else {
            Err(VerifiedOpError(
                Self::get_name(),
                unit,
                "sub",
                Self::get_name(),
                other_unit,
            ))
        }
    }

    fn verified_mul(&self, other: &CompDimen) -> ModDimen<CompDimen> {
        let unit = self.get_unit();
        let other_unit = other.get_unit();
        if unit == other_unit {
            Ok(CompDimen {
                value: self.value * other.get_value(),
                ..self.clone()
            })
        } else {
            Err(VerifiedOpError(
                Self::get_name(),
                unit,
                "mul",
                Self::get_name(),
                other_unit,
            ))
        }
    }

    fn verified_div(&self, other: &CompDimen) -> ModDimen<CompDimen> {
        let unit = self.get_unit();
        let other_unit = other.get_unit();
        if unit == other_unit {
            Ok(CompDimen {
                value: self.value / other.get_value(),
                ..self.clone()
            })
        } else {
            Err(VerifiedOpError(
                Self::get_name(),
                unit,
                "div",
                Self::get_name(),
                other_unit,
            ))
        }
    }

    fn verified_powf(&self, other: &CompDimen) -> ModDimen<CompDimen> {
        let unit = self.get_unit();
        let other_unit = other.get_unit();
        if unit == other_unit {
            Ok(CompDimen {
                value: self.value.powf(other.get_value()),
                ..self.clone()
            })
        } else {
            Err(VerifiedOpError(
                Self::get_name(),
                unit,
                "powf",
                Self::get_name(),
                other_unit,
            ))
        }
    }
}

impl DimenBaseDependent<CompDimenBase> for CompDimen {
    fn init(value: f64, unit: &str, base: CompDimenBase) -> NewDimen<CompDimen> {
        let mut cd = Self::new(base)?;
        cd.bcm_unit(unit)?;
        cd.value = value;
        Ok(cd)
    }

    fn get_base(&self) -> CompDimenBase {
        self.base.clone()
    }
}

impl DimenSetAndGet for CompDimen {
    fn set_value(&mut self, other: f64) {
        self.value = other;
    }

    fn get_value(&self) -> f64 {
        self.value
    }

    fn get_unit(&self) -> String {
        self.unit.clone()
    }

    fn get_base_to_display(&self) -> String {
        let mut b = self.base.0.get_base_to_display();
        b.push(self.get_op());
        b += &self.base.2.get_base_to_display();
        b
    }

    fn get_name() -> &'static str {
        "CompDimen"
    }
}

impl Display for CompDimen {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}{}", self.value, self.unit)
    }
}

impl From<CompDimenBase> for CompDimen {
    fn from(base: CompDimenBase) -> Self {
        Self::verify_op(&base.1).unwrap();
        let unit = Self::updated_unit(&base);
        let mut cd = CompDimen {
            value: 0.0,
            unit,
            base,
        };
        cd.update_value();
        cd
    }
}

pub enum GeneDimenBase {
    GeneSimpDimenBase(SimpDimenBase),
    GeneCompDimenBase(CompDimenBase),
}

#[derive(Clone, PartialEq, Debug)]
pub enum GeneDimen {
    GenSimpDimen(SimpDimen),
    GenCompDimen(CompDimen),
}

impl GeneDimen {
    pub fn unwrap_sd(&self) -> SimpDimen {
        match self {
            GenSimpDimen(sd) => sd.clone(),
            GenCompDimen(_) => {
                panic!("Called `GeneDimen::unwrap_sd()` on a `GenCompDimen`.")
            }
        }
    }

    pub fn unwrap_cd(&self) -> CompDimen {
        match self {
            GenSimpDimen(_) => {
                panic!("Called `GeneDimen::unwrap_cd()` on a `GenSimpDimen`.")
            }
            GenCompDimen(cd) => cd.clone(),
        }
    }

    pub fn soft_from<T: AsRef<str>>(value: T) -> NewDimen<GeneDimen> {
        let value = value.as_ref();

        SimpDimen::soft_from(value).map_or_else(
            |_| {
                CompDimen::soft_from(value).map_or_else(
                    |_| Err(UnitError(Self::get_name(), None, value.to_string())),
                    |cd| Ok(cd.to_generic()),
                )
            },
            |sd| Ok(sd.to_generic()),
        )
    }

    pub fn unwrap_to_debug(&self) -> String {
        match self {
            GenSimpDimen(sd) => format!("{:?}", sd),
            GenCompDimen(cd) => format!("{:?}", cd),
        }
    }
}

impl DimenBasics for GeneDimen {
    fn bcm_si(&mut self) {
        match self {
            GenSimpDimen(ref mut sd) => sd.bcm_si(),
            GenCompDimen(ref mut cd) => cd.bcm_si(),
        }
    }

    fn bcm_unit<T: AsRef<str>>(&mut self, unit: T) -> Result<(), DimenError> {
        match self {
            GenSimpDimen(ref mut sd) => sd.bcm_unit(unit)?,
            GenCompDimen(ref mut cd) => cd.bcm_unit(unit)?,
        }
        Ok(())
    }

    fn to_generic(self) -> GeneDimen {
        self
    }

    fn verified_add(&self, other: &GeneDimen) -> ModDimen<GeneDimen> {
        match (self, &other) {
            (GenSimpDimen(d1), GenSimpDimen(d2)) => Ok(Self::from(d1.verified_add(d2)?)),
            (GenCompDimen(dr1), GenCompDimen(dr2)) => Ok(Self::from(dr1.verified_add(dr2)?)),
            (GenSimpDimen(_), GenCompDimen(_)) => Err(VerifiedOpError(
                "GeneDimen::GenSimpDimen",
                self.get_unit(),
                "add",
                "GeneDimen::GenCompDimen",
                other.get_unit(),
            )),
            (GenCompDimen(_), GenSimpDimen(_)) => Err(VerifiedOpError(
                "GeneDimen::GenCompDimen",
                self.get_unit(),
                "add",
                "GeneDimen::GenSimpDimen",
                other.get_unit(),
            )),
        }
    }

    fn verified_sub(&self, other: &GeneDimen) -> ModDimen<GeneDimen> {
        match (self, &other) {
            (GenSimpDimen(d1), GenSimpDimen(d2)) => Ok(Self::from(d1.verified_sub(d2)?)),
            (GenCompDimen(dr1), GenCompDimen(dr2)) => Ok(Self::from(dr1.verified_sub(dr2)?)),
            (GenSimpDimen(_), GenCompDimen(_)) => Err(VerifiedOpError(
                "GeneDimen::GenSimpDimen",
                self.get_unit(),
                "sub",
                "GeneDimen::GenCompDimen",
                other.get_unit(),
            )),
            (GenCompDimen(_), GenSimpDimen(_)) => Err(VerifiedOpError(
                "GeneDimen::GenCompDimen",
                self.get_unit(),
                "sub",
                "GeneDimen::GenSimpDimen",
                other.get_unit(),
            )),
        }
    }

    fn verified_mul(&self, other: &GeneDimen) -> ModDimen<GeneDimen> {
        match (self, &other) {
            (GenSimpDimen(d1), GenSimpDimen(d2)) => Ok(Self::from(d1.verified_mul(d2)?)),
            (GenCompDimen(dr1), GenCompDimen(dr2)) => Ok(Self::from(dr1.verified_mul(dr2)?)),
            (GenSimpDimen(_), GenCompDimen(_)) => Err(VerifiedOpError(
                "GeneDimen::GenSimpDimen",
                self.get_unit(),
                "mul",
                "GeneDimen::GenCompDimen",
                other.get_unit(),
            )),
            (GenCompDimen(_), GenSimpDimen(_)) => Err(VerifiedOpError(
                "GeneDimen::GenCompDimen",
                self.get_unit(),
                "mul",
                "GeneDimen::GenSimpDimen",
                other.get_unit(),
            )),
        }
    }

    fn verified_div(&self, other: &GeneDimen) -> ModDimen<GeneDimen> {
        match (self, &other) {
            (GenSimpDimen(d1), GenSimpDimen(d2)) => Ok(Self::from(d1.verified_div(d2)?)),
            (GenCompDimen(dr1), GenCompDimen(dr2)) => Ok(Self::from(dr1.verified_div(dr2)?)),
            (GenSimpDimen(_), GenCompDimen(_)) => Err(VerifiedOpError(
                "GeneDimen::GenSimpDimen",
                self.get_unit(),
                "div",
                "GeneDimen::GenCompDimen",
                other.get_unit(),
            )),
            (GenCompDimen(_), GenSimpDimen(_)) => Err(VerifiedOpError(
                "GeneDimen::GenCompDimen",
                self.get_unit(),
                "div",
                "GeneDimen::GenSimpDimen",
                other.get_unit(),
            )),
        }
    }

    fn verified_powf(&self, other: &GeneDimen) -> ModDimen<GeneDimen> {
        match (self, &other) {
            (GenSimpDimen(d1), GenSimpDimen(d2)) => Ok(Self::from(d1.verified_powf(d2)?)),
            (GenCompDimen(dr1), GenCompDimen(dr2)) => Ok(Self::from(dr1.verified_powf(dr2)?)),
            (GenSimpDimen(_), GenCompDimen(_)) => Err(VerifiedOpError(
                "GeneDimen::GenSimpDimen",
                self.get_unit(),
                "powf",
                "GeneDimen::GenCompDimen",
                other.get_unit(),
            )),
            (GenCompDimen(_), GenSimpDimen(_)) => Err(VerifiedOpError(
                "GeneDimen::GenCompDimen",
                self.get_unit(),
                "powf",
                "GeneDimen::GenSimpDimen",
                other.get_unit(),
            )),
        }
    }
}

impl DimenBaseDependent<GeneDimenBase> for GeneDimen {
    fn init(value: f64, unit: &str, base: GeneDimenBase) -> NewDimen<GeneDimen> {
        Ok(match base {
            GeneDimenBase::GeneSimpDimenBase(sdb) => Self::from(SimpDimen::init(value, unit, sdb)?),
            GeneDimenBase::GeneCompDimenBase(cdb) => Self::from(CompDimen::init(value, unit, cdb)?),
        })
    }

    fn get_base(&self) -> GeneDimenBase {
        match self {
            GenSimpDimen(sd) => GeneDimenBase::GeneSimpDimenBase(sd.get_base()),
            GenCompDimen(cd) => GeneDimenBase::GeneCompDimenBase(cd.get_base()),
        }
    }
}

impl DimenSetAndGet for GeneDimen {
    fn set_value(&mut self, other: f64) {
        match self {
            GenSimpDimen(sd) => sd.set_value(other),
            GenCompDimen(cd) => cd.set_value(other),
        }
    }

    fn get_value(&self) -> f64 {
        match self {
            GenSimpDimen(sd) => sd.get_value(),
            GenCompDimen(cd) => cd.get_value(),
        }
    }

    fn get_unit(&self) -> String {
        match self {
            GenSimpDimen(ref sd) => sd.get_unit(),
            GenCompDimen(ref cd) => cd.get_unit(),
        }
    }

    fn get_base_to_display(&self) -> String {
        match self {
            GenSimpDimen(sd) => sd.get_base_to_display(),
            GenCompDimen(cd) => cd.get_base_to_display(),
        }
    }

    fn get_name() -> &'static str {
        "GeneDimen"
    }
}

impl Display for GeneDimen {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            GenSimpDimen(sd) => {
                write!(f, "{}", sd)
            }
            GenCompDimen(cd) => {
                write!(f, "{}", cd)
            }
        }
    }
}

impl From<SimpDimen> for GeneDimen {
    fn from(value: SimpDimen) -> Self {
        GenSimpDimen(value)
    }
}

impl From<CompDimen> for GeneDimen {
    fn from(value: CompDimen) -> Self {
        GenCompDimen(value)
    }
}

pass_ds_to_impl_ops!(SimpDimen, CompDimen, GeneDimen);
pass_nums_to_impl_ops!(f32, i32, u32, isize, usize);
impl_ops_asn_gd_for_gd!(AddAssign, add_assign, +; SubAssign, sub_assign, -; MulAssign, mul_assign, *; DivAssign, div_assign, /);
impl_exp_for_ds!(SimpDimen, CompDimen, GeneDimen);
impl_from_str_string_for_ds!(SimpDimen, CompDimen, GeneDimen);

#[cfg(test)]
mod sd_tests {
    use crate::*;
    use std::f64::consts::PI;

    type Biggest = &'static str;
    type Si = &'static str;
    type Smallest = &'static str;
    type WithPrefix = &'static str;
    const DIMENS_WITH_UNITS: [(SimpDimenBase, Biggest, Si, Smallest, WithPrefix); 9] = [
        (Length, "pc", "m", "ang", "mm"),
        (Time, "y", "s", "s", "ms"),
        (Mass, "t", "kg", "g", "mg"),
        (ElCurrent, "A", "A", "A", "mA"),
        (Temperature, "C", "K", "K", "mK"),
        (AmOfSubstance, "mole", "mole", "mole", "mmole"),
        (LuminousIn, "cd", "cd", "cd", "mcd"),
        (Angle, "rad", "°", r#"""#, r#"m""#),
        (ND, "", "", "", ""),
    ];

    #[test]
    fn init() {
        for d in DIMENS_WITH_UNITS {
            let current = SimpDimen::init(0.0, d.2, d.0).unwrap();
            let expected = SimpDimen::new(d.0);
            assert_eq!(current, expected)
        }
    }

    #[test]
    fn init_nd() {
        let current = SimpDimen::init_nd(0.0);
        let expected = SimpDimen::new(ND);
        assert_eq!(current, expected)
    }

    #[test]
    fn from() {
        for d in DIMENS_WITH_UNITS {
            if d.0 != ND {
                let current = SimpDimen::from("5".to_string() + d.1);
                let expect = SimpDimen::init(5.0, d.1, d.0).unwrap();
                assert_eq!(current, expect)
            } else {
                let current = SimpDimen::from(5.0);
                let expect = SimpDimen::init_nd(5.0);
                assert_eq!(current, expect);
            }
        }
    }

    #[test]
    fn get_value() {
        let current = SimpDimen::init(1234.5, "m", Length).unwrap();
        assert_eq!(current.get_value(), 1234.5)
    }

    #[test]
    fn get_unit() {
        let current = SimpDimen::init(1234.5, "m", Length).unwrap();
        assert_eq!(current.get_unit(), "m")
    }

    #[test]
    fn get_unit_owner() {
        for d in DIMENS_WITH_UNITS {
            assert_eq!(SimpDimen::get_unit_owner(d.2).unwrap(), d.0);
        }
    }

    #[test]
    fn verified_ops() {
        let numbers = [
            ("10m", "35m", "15m", "250m", "2.5m", "625m"),
            ("-10m", "15m", "35m", "-250m", "-2.5m", "0.0016m"),
            ("2.5m", "27.5m", "22.5m", "62.5m", "10m", "5m"),
            ("-2.5m", "22.5m", "27.5m", "-62.5m", "-10m", "0.2m"),
        ];
        for n in numbers {
            let old = SimpDimen::from("25m");
            let other = SimpDimen::from(n.0);

            let current = old.verified_add(&other).unwrap();
            assert_eq!(current, SimpDimen::from(n.1));

            let current = old.verified_sub(&other).unwrap();
            assert_eq!(current, SimpDimen::from(n.2));

            let current = old.verified_mul(&other).unwrap();
            assert_eq!(current, SimpDimen::from(n.3));

            let current = old.verified_div(&other).unwrap();
            assert_eq!(current, SimpDimen::from(n.4));

            let current = old.verified_powf(&(other / 5)).unwrap();
            assert_eq!(current, SimpDimen::from(n.5));
        }
    }

    fn si_test_base(fn_to_conv: fn(x: SimpDimen) -> SimpDimen) {
        let expected_values = [
            (3.0856775813e16, 1.0, 1e-10, 1e-3),
            (31557600.0, 1.0, 1.0, 1e-3),
            (1e3, 1.0, 1e-3, 1e-6),
            (1.0, 1.0, 1.0, 1e-3),
            (274.15, 1.0, 1.0, 1e-3),
            (1.0, 1.0, 1.0, 1e-3),
            (1.0, 1.0, 1.0, 1e-3),
            (180.0 / PI, 1.0, 1.0 / 3600.0, 1.0 / 3600e3),
            (1.0, 1.0, 1.0, 1.0),
        ];
        for (i, d) in DIMENS_WITH_UNITS.iter().enumerate() {
            let units_and_values = [
                (d.1, expected_values[i].0),
                (d.2, expected_values[i].1),
                (d.3, expected_values[i].2),
                (d.4, expected_values[i].3),
            ];
            for (u, v) in units_and_values {
                let current = SimpDimen::init(1.0, u, d.0).unwrap();
                let expected = SimpDimen::init(v, d.2, d.0).unwrap();
                assert_eq!(fn_to_conv(current), expected);
            }
        }
    }

    #[test]
    fn to_si() {
        let fn_to_conv = |x: SimpDimen| x.to_si();
        si_test_base(fn_to_conv)
    }

    #[test]
    fn bcm_si() {
        let fn_to_conv = |x: SimpDimen| {
            let mut x = x;
            x.bcm_si();
            x
        };
        si_test_base(fn_to_conv)
    }
}
