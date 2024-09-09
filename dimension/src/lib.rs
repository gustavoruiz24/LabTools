use std::cmp::Ordering::{self, Equal};
use std::fmt::{self, Debug, Display, Formatter, LowerExp, UpperExp};
use std::ops::*;

use data::*;
use error::*;
use error::DimenError::*;
use macros::*;
use traits::*;

use self::GeneDimen::*;
use self::SimpDimenBase::*;

pub mod data;
pub mod error;
pub mod utils;
pub mod macros;
pub mod traits;

fn unwrap_brackets(value: &str) -> String {
    let mut value = value;
    if value.contains('(') {
        value = value.strip_prefix('(').unwrap();
        value = value.strip_suffix(')').unwrap();
    };
    value.to_string()
}

fn push_with_exp(op: char, unit: &str, units: &mut Vec<(String, f64)>) {
    if unit.contains('(') {
        create_exponents(op, unit, units)
    } else {
        let mut pushing: Vec<(String, f64)> = vec![];
        take_units_ops(unit, &mut pushing);
        if op == '/' {
            pushing = pushing.iter().map(|(u, e)| (u.clone(), -e)).collect();
        }
        units.append(&mut pushing);
    }
}

fn create_exponents(op: char, unit: &str, units: &mut Vec<(String, f64)>) {
    let base_ops = CompDimen::get_ops();
    let mut op2: char;
    let (mut unit1, mut unit2): (&str, &str);

    if let Some(new_op2) = unit
        .chars()
        .rfind(|x: &char| base_ops.contains(x) && x != &'^')
    {
        op2 = new_op2;
        (unit1, unit2) = unit.rsplit_once(op2).unwrap();
    } else {
        let unit = unwrap_brackets(unit);
        push_with_exp(op, &unit, units);
        return;
    };

    if unit.ends_with(')') {
        let partner = bracket_partner(unit, unit.len() - 1);
        if partner == 0 {
            let unit = unwrap_brackets(unit);
            create_exponents(op, &unit, units);
            return;
        } else {
            op2 = unit.chars().nth(partner - 1).unwrap();
            (unit1, unit2) = unit.split_at(partner - 1);
            unit2 = unit2.trim_start_matches(op2);
        }
    }

    if op2 != op {
        op2 = '/'
    }

    push_with_exp(op, unit1, units);
    push_with_exp(op2, unit2, units);
}

fn take_units_ops(current: &str, exp_units: &mut Vec<(String, f64)>) {
    let basic_ops = CompDimen::get_ops();

    let units = current
        .split(|x: char| basic_ops.contains(&x))
        .map(|x| x.to_string())
        .collect::<Vec<String>>();

    let mut ops = vec!['*'];

    ops.append(
        &mut current
            .chars()
            .filter(|x: &char| basic_ops.contains(x))
            .collect::<Vec<char>>(),
    );

    let mut current_exp = 1.0;
    for (i, unit) in units.iter().enumerate().rev() {
        match ops[i] {
            '^' => {
                current_exp *= unit.parse::<f64>().unwrap();
                continue;
            }
            '/' => {
                current_exp *= -1.0;
            }
            _ => {}
        };

        exp_units.push((unit.clone(), current_exp));
        current_exp = 1.0;
    }
}

fn join_op_unit(unit: &(String, f64)) -> String {
    let mut joined = String::from('*');
    joined += &unit.0;

    if unit.1 != 1.0 {
        joined.push('^');
        joined += &unit.1.to_string();
    };

    joined
}

fn shorten_expression(
    current: &str,
    current_op: char,
    pushing: &str,
) -> Option<(String, Option<char>, String)> {
    let ops = CompDimen::get_ops();

    if !current.contains(|x: char| ops.contains(&x) && x != '^')
        && !pushing.contains(|x: char| ops.contains(&x) && x != '^')
        && current != pushing
    {
        return None;
    }

    let mut all_units = vec![];

    let mut units1 = vec![];
    create_exponents('*', current, &mut units1);

    if current_op == '^' {
        let exp = pushing.parse::<f64>().unwrap();
        for val in &mut units1 {
            val.1 *= exp;
        }
        all_units.append(&mut units1)
    } else {
        let mut units2 = vec![];
        create_exponents(current_op, pushing, &mut units2);

        let append_to_all_units =
            |pushing: &Vec<(String, f64)>, all_units: &mut Vec<(String, f64)>| {
                for unit in pushing {
                    let pos_res = all_units.iter().position(|(u, _)| u == &unit.0);
                    if let Some(pos) = pos_res {
                        all_units[pos].1 += unit.1;
                    } else {
                        all_units.push(unit.clone())
                    }
                }
            };

        append_to_all_units(&units1, &mut all_units);
        append_to_all_units(&units2, &mut all_units);

        all_units.retain(|(_, e)| e != &0.0);

        if all_units.is_empty() {
            return Some((String::new(), None, String::new()));
        }

        let mut units_to_sort: Vec<(usize, &(String, f64))> =
            all_units.iter().enumerate().collect();
        units_to_sort.sort_unstable_by(|(ia, (_, a)), (ib, (_, b))| {
            if b.partial_cmp(a).unwrap() == Equal {
                ib.cmp(ia)
            } else {
                b.partial_cmp(a).unwrap()
            }
        });
        all_units = units_to_sort
            .iter()
            .map(|(_, (u, e))| (u.clone(), *e))
            .collect();
    }

    let old_unit = (current.to_string(), Some(current_op), pushing.to_string());
    let mut new_unit = (String::new(), None, String::new());

    if all_units.len() > 1 {
        let last_pos = all_units.len() - 1;
        let last = all_units.last().unwrap();

        new_unit.1 = if last.1 > 0.0 { Some('*') } else { Some('/') };

        all_units[last_pos].1 = last.1.abs();
        let last = all_units.last().unwrap();

        new_unit.2 = join_op_unit(last).trim_start_matches('*').to_string();

        all_units.remove(last_pos);
    }

    let first_div = all_units.iter().position(|(_, e)| e < &0.0);
    match first_div {
        Some(pos) if pos > 0 => {
            all_units = all_units.into_iter().map(|(u, e)| (u, e.abs())).collect();

            new_unit.0 += &all_units[0..pos]
                .iter()
                .map(join_op_unit)
                .collect::<String>()
                .trim_start_matches('*');

            if all_units[0..pos].len() > 1 {
                new_unit.0 = format!("({})", new_unit.0);
            }

            new_unit.0.push('/');

            let mut dividing = String::new();

            dividing += &all_units[pos..]
                .iter()
                .map(join_op_unit)
                .collect::<String>()
                .trim_start_matches('*');

            if all_units[pos..].len() > 1 {
                dividing = format!("({})", dividing);
            }

            new_unit.0 += &dividing;
        }
        _ => {
            new_unit.0 += &all_units
                .iter()
                .map(join_op_unit)
                .collect::<String>()
                .trim_start_matches('*');
        }
    }

    if new_unit == old_unit {
        None
    } else {
        if let (None, Some((unit1, unit2))) = (new_unit.1, new_unit.0.split_once('^')) {
            new_unit = (unit1.to_string(), Some('^'), unit2.to_string())
        }

        Some(new_unit)
    }
}

//noinspection ALL
fn is_balanced(expression: &str) -> bool {
    let mut balance = 0;

    for p in expression.chars() {
        if p == '(' {
            balance += 1
        } else if p == ')' {
            balance -= 1;
        }

        if balance < 0 {
            return false;
        }
    }

    balance == 0
}

//noinspection ALL
fn bracket_partner(expression: &str, pos: usize) -> usize {
    let mut balance = 0;
    let mut chars = expression
        .chars()
        .enumerate()
        .collect::<Vec<(usize, char)>>();
    let mut pos = pos;

    if chars[pos].1 == ')' {
        chars.reverse();
        pos = chars.len() - pos - 1;
    }

    for (i, p) in &chars[pos..] {
        if p == &'(' {
            balance += 1
        } else if p == &')' {
            balance -= 1;
        }

        if balance == 0 {
            return *i;
        }
    }

    0
}

fn take_num_unit<T: AsRef<str>>(
    owner: &'static str,
    value: T,
) -> Result<(f64, String), DimenError> {
    let value = value.as_ref();

    let number = value
        .split_once(|x: char| x.is_alphabetic() || x == '(')
        .ok_or(InvalidValError(owner))?
        .0;

    let unit = value.trim_start_matches(number).to_string();

    if unit.is_empty() {
        return Err(InvalidValError(owner));
    }

    let number = if !number.is_empty() {
        number.parse::<f64>().unwrap()
    } else {
        0.0
    };

    Ok((number, unit))
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

pub const SIMP_DIMEN_BASES: [SimpDimenBase; 9] = [
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

#[derive(Copy, Clone, PartialEq)]
pub struct SimpDimen {
    value: f64,
    prefix: usize,
    unit: usize,
    base: SimpDimenBase,
    si_unit: usize,
    prefixes: &'static [(&'static str, f64)],
    units: &'static [&'static str],
    conv_rates: &'static [(ConvFromSi, ConvToSi)],
}

impl SimpDimen {
    fn get_dimen_unit_pos(&self, unit: &str) -> usize {
        self.units.iter().position(|x| x == &unit).unwrap()
    }

    fn get_base_info(base: SimpDimenBase) -> SimpDimen {
        let info: (usize, &[&str], &[(ConvFromSi, ConvToSi)]);

        match base {
            Length => info = (3, &LENGTH_INFO.0, &LENGTH_INFO.1),
            Time => info = (6, &TIME_INFO.0, &TIME_INFO.1),
            Mass => info = (1, &MASS_INFO.0, &MASS_INFO.1),
            ElCurrent => info = (0, &EL_CURRENT_INFO.0, &EL_CURRENT_INFO.1),
            Temperature => info = (2, &TEMPERATURE_INFO.0, &TEMPERATURE_INFO.1),
            AmOfSubstance => info = (0, &AM_OF_SUBSTANCE_INFO.0, &AM_OF_SUBSTANCE_INFO.1),
            LuminousIn => info = (0, &LUMINOUS_IN_INFO.0, &LUMINOUS_IN_INFO.1),
            Angle => info = (1, &ANGLE_INFO.0, &ANGLE_INFO.1),
            ND => info = (0, &ND_INFO.0, &ND_INFO.1),
            Custom(_) => {
                panic!("Custom dimensions must be created using `SimpDimen::create_custom`.")
            }
        }

        let (prefixes, prefix): (&[(&str, f64)], usize) = match base {
            ND => (&ND_PREFIXES, 0),
            _ => (&PREFIXES, 10),
        };

        SimpDimen {
            value: 0.0,
            prefix,
            unit: info.0,
            base,
            si_unit: info.0,
            prefixes,
            units: info.1,
            conv_rates: info.2,
        }
    }

    fn separate_unit(&self, unit: &str) -> Option<(usize, usize)> {
        if self.is_nd() || unit.is_empty() {
            return None;
        }

        let mut chars = unit.chars();
        let (prefix, unit) = (chars.next().unwrap(), String::from_iter(chars));

        if let (Some(prefix_pos), Some(unit_pos)) = (
            self.prefixes
                .iter()
                .position(|(x, _)| x == &prefix.to_string()),
            self.units.iter().position(|x| x == &unit),
        ) {
            Some((prefix_pos, unit_pos))
        } else {
            None
        }
    }

    fn verify_unit(&self, unit: &str) -> Result<(usize, usize), DimenError> {
        if self.units.contains(&unit) {
            let prefix = if self.base != ND { 10 } else { 0 };
            Ok((prefix, self.get_dimen_unit_pos(unit)))
        } else {
            self.separate_unit(unit).ok_or(UnitError(
                Self::get_name(),
                Some(self.get_base_to_display()),
                unit.to_string(),
            ))
        }
    }

    pub fn val_to_display(value: f64, unit: String, show_unit: bool) -> String {
        if show_unit {
            format!("{}{}", value, unit)
        } else {
            value.to_string()
        }
    }

    pub fn remove_prefix(&mut self) {
        self.value *= self.prefixes[self.prefix].1;
        self.prefix = if self.base != ND { 10 } else { 0 };
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
        units: &'static [&'static str],
        conv_rates: &'static [(ConvFromSi, ConvToSi)],
    ) -> NewDimenRes<SimpDimen> {
        if units.len() != conv_rates.len() {
            return Err(DiffLenError(Self::get_name(), "units", "conv_rates"));
        }

        let unit_err = UnitError(
            Self::get_name(),
            Some(Custom(name).to_string()),
            si.to_string(),
        );
        let unit = units.iter().position(|x| x == &si).ok_or(unit_err)?;

        Ok(SimpDimen {
            value: 0.0,
            prefix: 10,
            unit,
            base: Custom(name),
            si_unit: unit,
            prefixes: &PREFIXES,
            units,
            conv_rates,
        })
    }

    pub fn init_nd(value: f64) -> SimpDimen {
        SimpDimen {
            value,
            ..Self::get_base_info(ND)
        }
    }

    pub fn init_clone(self, value: f64, unit: &str) -> NewDimenRes<SimpDimen> {
        let (prefix, unit) = self.verify_unit(unit)?;
        Ok(SimpDimen {
            value,
            prefix,
            unit,
            ..self
        })
    }

    pub fn init_unchecked(
        value: f64,
        prefix: usize,
        unit: usize,
        base: SimpDimenBase,
    ) -> SimpDimen {
        let base = Self::get_base_info(base);
        SimpDimen {
            value,
            prefix,
            unit,
            ..base
        }
    }

    pub fn soft_from<T: AsRef<str>>(value: T) -> NewDimenRes<SimpDimen> {
        let (number, unit) = take_num_unit(Self::get_name(), value)?;
        Self::init(number, &unit, Self::get_unit_owner(&unit)?)
    }

    pub fn get_prefix(&self) -> usize {
        self.prefix
    }

    pub fn get_unit_owner(unit: &str) -> Result<SimpDimenBase, DimenError> {
        for base in SIMP_DIMEN_BASES {
            let d = Self::get_base_info(base);
            if d.verify_unit(unit).is_ok() {
                return Ok(base);
            }
        }

        Err(UnitError(Self::get_name(), None, unit.to_string()))
    }
}

impl DimenBasics for SimpDimen {
    fn is_nd(&self) -> bool {
        self.base == ND
    }

    fn bcm_si(&mut self) {
        if (self.unit != self.si_unit || self.prefix != 10) && self.base != ND {
            self.remove_prefix();
            let unit = self.si_unit;
            self.value = self.conv_rates[self.unit].1(self.value);
            self.unit = unit;
        }
    }

    fn bcm_unit<T: AsRef<str>>(&mut self, unit: T) -> Result<(), DimenError> {
        let (prefix, unit) = self.verify_unit(unit.as_ref())?;
        self.bcm_unit_unchecked(prefix, unit);
        Ok(())
    }

    fn to_generic(self) -> GeneDimen {
        GeneDimen::from(self)
    }

    fn verified_add(&self, other: &SimpDimen) -> ModDimenRes<SimpDimen> {
        crate::apply_verified_ops!(self, other, SimpDimen, SimpDimen, add, "add")
    }

    fn verified_sub(&self, other: &SimpDimen) -> ModDimenRes<SimpDimen> {
        crate::apply_verified_ops!(self, other, SimpDimen, SimpDimen, sub, "sub")
    }

    fn verified_mul(&self, other: &SimpDimen) -> ModDimenRes<SimpDimen> {
        crate::apply_verified_ops!(self, other, SimpDimen, SimpDimen, mul, "mul")
    }

    fn verified_div(&self, other: &SimpDimen) -> ModDimenRes<SimpDimen> {
        crate::apply_verified_ops!(self, other, SimpDimen, SimpDimen, div, "div")
    }

    fn verified_pow(&self, other: &SimpDimen) -> ModDimenRes<SimpDimen> {
        crate::apply_verified_ops!(self, other, SimpDimen, SimpDimen, powf, "pow")
    }
}

impl DimenBaseDependents<SimpDimenBase> for SimpDimen {
    fn init(value: f64, unit: &str, base: SimpDimenBase) -> NewDimenRes<SimpDimen> {
        let base = Self::get_base_info(base);
        let (prefix, unit) = base.verify_unit(unit)?;
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
        if !self.is_nd() {
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

pub type CompDimenBase = (Box<GeneDimen>, char, Box<GeneDimen>);

#[derive(Clone, PartialEq)]
pub struct CompDimen {
    value: f64,
    unit: String,
    base: (Box<GeneDimen>, char, Box<GeneDimen>),
}

impl CompDimen {
    fn get_op(&self) -> char {
        self.base.1
    }

    fn get_ops() -> [char; 3] {
        ['*', '/', '^']
    }

    fn get_base_values(&self) -> (f64, f64) {
        (self.base.0.get_value(), self.base.2.get_value())
    }

    fn get_parent_as_unit(parent: &GeneDimen) -> String {
        if parent.is_nd() {
            parent.get_value().to_string()
        } else {
            parent.get_unit()
        }
    }

    fn updated_unit(base: &CompDimenBase) -> String {
        let mut unit = base.0.get_unit();
        let mut unit2 = Self::get_parent_as_unit(&base.2);

        if unit.contains('/') && base.1 == '/' {
            let (unit1, unit1_2) = unit.split_once('/').unwrap();
            let (mut unit1, mut unit1_2) = (unit1.to_string(), unit1_2.to_string());

            if unit1_2.contains('(') {
                unit1_2 = unwrap_brackets(&unit1_2)
            }

            match base.0.get_base() {
                GeneDimenBase::GeneCDBase(_) if !unit1.contains('(') => {
                    unit1 = format!("({})", unit1)
                }
                _ => {}
            }

            unit = format!("{}/({}*{})", unit1, unit1_2, unit2);

            unit
        } else {
            if base.1 == '/' {
                match base.0.get_base() {
                    GeneDimenBase::GeneCDBase(_) if !unit.contains('(') => {
                        unit = format!("({})", unit)
                    }
                    _ => {}
                }
                match base.2.get_base() {
                    GeneDimenBase::GeneCDBase(_) if !unit2.contains('(') => {
                        unit2 = format!("({})", unit2)
                    }
                    _ => {}
                }
            }
            unit.push(base.1);
            unit += &unit2;

            unit
        }
    }

    fn update_value(&mut self) {
        let (value1, value2) = self.get_base_values();
        self.value *= match self.get_op() {
            '*' => value1 * value2,
            '/' => value1 / value2,
            '^' => value1.powf(value2),
            _ => {
                panic!("{}", OperatorError(Some(self.get_op().to_string())))
            }
        };
    }

    fn verify_op(op: &char) -> Result<(), DimenError> {
        if Self::get_ops().contains(op) {
            Ok(())
        } else {
            Err(OperatorError(Some(op.to_string())))
        }
    }

    fn default_bases_val(bases: (&mut Box<GeneDimen>, &mut Box<GeneDimen>)) {
        bases.0.set_value(1.0);
        if !bases.1.is_nd() {
            bases.1.set_value(1.0)
        }
    }

    fn shorten_base(base: CompDimenBase) -> Result<CompDimenBase, DimenError> {
        let base2_unit = Self::get_parent_as_unit(&base.2);

        let new_base = shorten_expression(&base.0.get_unit(), base.1, &base2_unit);

        let mut base = match new_base {
            Some((unit1, Some(op), unit2)) => Self::base_by_op_units(&unit1, op, &unit2, false)?,
            Some((_, None, _)) => return Err(OperatorError(None)),
            _ => base,
        };

        Self::default_bases_val((&mut base.0, &mut base.2));

        Ok(base)
    }

    fn units_base(unit: &str) -> Result<(String, char, String), DimenError> {
        let mut unit = unit.to_string();
        if !is_balanced(&unit) {
            return Err(BracketError(unit));
        }
        if unit.ends_with(')') {
            let partner = bracket_partner(&unit, unit.len() - 1);
            if partner == 0 {
                unit = unit.strip_prefix('(').unwrap().to_string();
                unit = unit.strip_suffix(')').unwrap().to_string();
            } else {
                let op = unit.chars().nth(partner - 1).unwrap();
                let (unit1, mut unit2) = unit.split_at(partner - 1);
                unit2 = unit2.trim_start_matches(op);
                return Ok((unit1.to_string(), op, unit2.to_string()));
            }
        }

        let op = unit
            .chars()
            .rfind(|x| Self::verify_op(x).is_ok())
            .ok_or(OperatorError(None))?;

        let (unit1, unit2) = unit.rsplit_once(|x| x == op).unwrap();

        Ok((unit1.to_string(), op, unit2.to_string()))
    }

    fn propagate_top_to_bottom(&self) -> Vec<(SimpDimenBase, String)> {
        let mut units = self.base.0.propagate_top_to_bottom();
        units.append(&mut self.base.2.propagate_top_to_bottom());
        units
    }

    fn propagate_bottom_to_top(&mut self, units: &Vec<(SimpDimenBase, String)>) {
        self.base.0.propagate_bottom_to_top(units);
        self.base.2.propagate_bottom_to_top(units);
    }

    pub fn new(base: CompDimenBase) -> NewDimenRes<CompDimen> {
        Self::verify_op(&base.1)?;
        let base = Self::shorten_base(base)?;
        let unit = Self::updated_unit(&base);

        Ok(CompDimen {
            value: 0.0,
            unit,
            base,
        })
    }

    pub fn from_units_base(
        unit1: &str,
        op: char,
        unit2: &str,
        resume: bool,
    ) -> NewDimenRes<CompDimen> {
        Self::verify_op(&op)?;
        let base = Self::base_by_op_units(unit1, op, unit2, resume)?;
        let unit = Self::updated_unit(&base);

        Ok(CompDimen {
            value: 0.0,
            unit,
            base,
        })
    }

    pub fn soft_from<T: AsRef<str>>(value: T) -> NewDimenRes<CompDimen> {
        let (number, unit) = take_num_unit(Self::get_name(), value)?;
        let base = Self::base_by_unit(&unit)?;
        let unit = Self::updated_unit(&base);
        Ok(Self::init(number, &unit, base).unwrap())
    }

    pub fn base_by_unit(unit: &str) -> Result<CompDimenBase, DimenError> {
        let base = Self::units_base(unit)?;
        Self::base_by_op_units(&base.0, base.1, &base.2, true)
    }

    pub fn base_by_op_units(
        unit1: &str,
        op: char,
        unit2: &str,
        resume: bool,
    ) -> Result<CompDimenBase, DimenError> {
        let make_base_piece = |unit: &str| -> NewDimenRes<GeneDimen> {
            if let Ok(value) = unit.parse::<f64>() {
                Ok(SimpDimen::init_nd(value).to_generic())
            } else {
                GeneDimen::soft_from_checked(unit, false)
            }
        };

        let error = Err(UnitError(
            Self::get_name(),
            None,
            unit1.to_string() + &op.to_string() + unit2,
        ));

        let base1 = make_base_piece(unit1).or(error.clone())?;
        let mut base2 = make_base_piece(unit2).or(error)?;

        let units = base1.propagate_top_to_bottom();
        base2.propagate_bottom_to_top(&units);

        let mut base = (Box::new(base1), op, Box::new(base2));
        if resume {
            base = Self::shorten_base(base)?;
        }
        Self::default_bases_val((&mut base.0, &mut base.2));
        Ok(base)
    }
}

impl DimenBasics for CompDimen {
    fn is_nd(&self) -> bool {
        false
    }

    fn bcm_si(&mut self) {
        self.base.0.bcm_si();
        self.base.2.bcm_si();
        self.update_value();
        Self::default_bases_val((&mut self.base.0, &mut self.base.2));

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
            let mut base2 = Ok(());

            if !self.base.2.is_nd() {
                base2 = self.base.2.bcm_unit(unit2)
            };

            base1.and(base2).or(Err(dimen_error))?;

            self.update_value();
            Self::default_bases_val((&mut self.base.0, &mut self.base.2));

            self.unit = Self::updated_unit(&self.base);
        }
        Ok(())
    }

    fn to_generic(self) -> GeneDimen {
        GeneDimen::from(self)
    }

    fn verified_add(&self, other: &CompDimen) -> ModDimenRes<CompDimen> {
        apply_verified_ops!(self, other, CompDimen, CompDimen, add, "add")
    }

    fn verified_sub(&self, other: &CompDimen) -> ModDimenRes<CompDimen> {
        apply_verified_ops!(self, other, CompDimen, CompDimen, sub, "sub")
    }

    fn verified_mul(&self, other: &CompDimen) -> ModDimenRes<CompDimen> {
        apply_verified_ops!(self, other, CompDimen, CompDimen, mul, "mul")
    }

    fn verified_div(&self, other: &CompDimen) -> ModDimenRes<CompDimen> {
        apply_verified_ops!(self, other, CompDimen, CompDimen, div, "div")
    }

    fn verified_pow(&self, other: &CompDimen) -> ModDimenRes<CompDimen> {
        apply_verified_ops!(self, other, CompDimen, CompDimen, powf, "pow")
    }
}

impl DimenBaseDependents<CompDimenBase> for CompDimen {
    fn init(value: f64, unit: &str, base: CompDimenBase) -> NewDimenRes<CompDimen> {
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

impl Debug for CompDimen {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum GeneDimenBase {
    GeneSDBase(SimpDimenBase),
    GeneCDBase(CompDimenBase),
}

#[derive(Clone, PartialEq, Debug)]
pub enum GeneDimen {
    GenSimpDimen(SimpDimen),
    GenCompDimen(CompDimen),
}

impl GeneDimen {
    fn init_from_operation(base1: GeneDimen, op: char, base2: GeneDimen) -> GeneDimen {
        let mut base2 = base2;
        let c_units = base1.propagate_top_to_bottom();
        base2.propagate_bottom_to_top(&c_units);

        let val = match op {
                '+' => base1.get_value() + base2.get_value(),
                '-' => base1.get_value() - base2.get_value(),
                '*' => base1.get_value() * base2.get_value(),
                '/' => base1.get_value() / base2.get_value(),
                '^' => base1.get_value().powf(base2.get_value()),
                _ => unreachable!(),
        };

        let (unit1, unit2) = (base1.get_unit(), CompDimen::get_parent_as_unit(&base2));
        let base = (Box::new(base1), op, Box::new(base2));

        let mut gd = match shorten_expression(&unit1, op, &unit2) {
            Some((unit1, Some(new_op), unit2)) => {
                CompDimen::from_units_base(&unit1, new_op, &unit2, true)
                    .unwrap()
                    .to_generic()
            }
            Some((unit, None, _)) => SimpDimen::from(unit).to_generic(),
            _ => CompDimen::new(base).unwrap().to_generic(),
        };

        gd.set_value(val);
        gd
    }

    fn soft_from_checked<T: AsRef<str>>(value: T, resume: bool) -> NewDimenRes<GeneDimen> {
        let value = value.as_ref();
        let error = UnitError(Self::get_name(), None, value.to_string());

        let gd = if value.contains(|x: char| CompDimen::verify_op(&x).is_ok()) {
            let (num, unit) = take_num_unit("", value).unwrap();
            let unit_base = CompDimen::units_base(&unit).unwrap();
            if resume {
                match shorten_expression(&unit_base.0, unit_base.1, &unit_base.2) {
                    Some((unit1, Some(op), unit2)) => {
                        CompDimen::from_units_base(&unit1, op, &unit2, false)
                            .or(Err(error))?
                            .to_generic()
                    }
                    Some((unit, None, _)) => SimpDimen::init(
                        num,
                        &unit,
                        SimpDimen::get_unit_owner(&unit).or(Err(error))?,
                    )
                        .unwrap()
                        .to_generic(),
                    _ => CompDimen::from_units_base(&unit_base.0, unit_base.1, &unit_base.2, false)
                        .or(Err(error))?
                        .to_generic(),
                }
            } else {
                CompDimen::from_units_base(&unit_base.0, unit_base.1, &unit_base.2, false)
                    .or(Err(error))?
                    .to_generic()
            }
        } else {
            SimpDimen::soft_from(value).or(Err(error))?.to_generic()
        };

        Ok(gd)
    }

    fn propagate_top_to_bottom(&self) -> Vec<(SimpDimenBase, String)> {
        match self {
            GenSimpDimen(sd) => vec![(sd.get_base(), sd.get_unit())],
            GenCompDimen(cd) => cd.propagate_top_to_bottom(),
        }
    }

    fn propagate_bottom_to_top(&mut self, units: &Vec<(SimpDimenBase, String)>) {
        match self {
            GenSimpDimen(sd) => {
                if let Some((_, new_unit)) = units
                    .iter()
                    .find(|(base, _)| base == &sd.get_base() && base != &ND)
                {
                    sd.bcm_unit(new_unit).unwrap();
                }
            }
            GenCompDimen(cd) => {
                cd.propagate_bottom_to_top(units);
                cd.update_value();
                cd.unit = CompDimen::updated_unit(&cd.base)
            }
        };
    }

    pub fn unwrap_sd(&self) -> SimpDimen {
        match self {
            GenSimpDimen(sd) => *sd,
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

    pub fn soft_from<T: AsRef<str>>(value: T) -> NewDimenRes<GeneDimen> {
        Self::soft_from_checked(value, true)
    }

    pub fn unwrap_to_debug(&self) -> String {
        match self {
            GenSimpDimen(sd) => format!("{:?}", sd),
            GenCompDimen(cd) => format!("{:?}", cd),
        }
    }
}

impl DimenBasics for GeneDimen {
    fn is_nd(&self) -> bool {
        match self {
            GenSimpDimen(sd) => sd.is_nd(),
            GenCompDimen(_) => false,
        }
    }

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

    fn verified_add(&self, other: &GeneDimen) -> ModDimenRes<GeneDimen> {
        apply_verified_ops_gd!(self, other, verified_add, "add")
    }

    fn verified_sub(&self, other: &GeneDimen) -> ModDimenRes<GeneDimen> {
        apply_verified_ops_gd!(self, other, verified_sub, "sub")
    }

    fn verified_mul(&self, other: &GeneDimen) -> ModDimenRes<GeneDimen> {
        apply_verified_ops_gd!(self, other, verified_mul, "mul")
    }

    fn verified_div(&self, other: &GeneDimen) -> ModDimenRes<GeneDimen> {
        apply_verified_ops_gd!(self, other, verified_div, "div")
    }

    fn verified_pow(&self, other: &GeneDimen) -> ModDimenRes<GeneDimen> {
        apply_verified_ops_gd!(self, other, verified_pow, "pow")
    }
}

impl DimenBaseDependents<GeneDimenBase> for GeneDimen {
    fn init(value: f64, unit: &str, base: GeneDimenBase) -> NewDimenRes<GeneDimen> {
        Ok(match base {
            GeneDimenBase::GeneSDBase(sdb) => Self::from(SimpDimen::init(value, unit, sdb)?),
            GeneDimenBase::GeneCDBase(cdb) => Self::from(CompDimen::init(value, unit, cdb)?),
        })
    }

    fn get_base(&self) -> GeneDimenBase {
        match self {
            GenSimpDimen(sd) => GeneDimenBase::GeneSDBase(sd.get_base()),
            GenCompDimen(cd) => GeneDimenBase::GeneCDBase(cd.get_base()),
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

impl<Dimen1, Dimen2> PowD<Dimen2> for Dimen1
    where
        Dimen1: DimenBasics + DimenSetAndGet,
        Dimen2: DimenBasics + DimenSetAndGet,
{
    type Output = GeneDimen;

    fn powd(mut self, other: Dimen2) -> Self::Output {
        if !other.is_nd() {
            panic!(
                "{}",
                OperationError(
                    Self::get_name(),
                    self.get_base_to_display(),
                    "pow",
                    Dimen2::get_name(),
                    other.get_base_to_display(),
                )
            )
        } else if self.is_nd() {
            self.set_value(self.get_value().powf(other.get_value()));
            self.to_generic()
        } else {
            GeneDimen::init_from_operation(self.to_generic(),
                '^',
                other.to_generic(),
            )
        }
    }
}

impl<T, Dimen> Pow<T> for Dimen
    where
        T: Into<f64>,
        Dimen: DimenBasics + DimenSetAndGet,
{
    type Output = GeneDimen;

    fn pow(self, other: T) -> Self::Output {
        self.powd(SimpDimen::from(other.into()))
    }
}

pass_ds_to_impl_ops!(SimpDimen, CompDimen, GeneDimen);
impl_ops_asn_gd_for_gd!(AddAssign, add_assign, +; SubAssign, sub_assign, -; MulAssign, mul_assign, *; DivAssign, div_assign, /);
impl_exp_for_ds!(SimpDimen, CompDimen, GeneDimen);
impl_from_str_string_for_ds!(SimpDimen, CompDimen, GeneDimen);
